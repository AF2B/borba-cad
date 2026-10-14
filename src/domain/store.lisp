(in-package #:borba-cad/domain)

;;;; The entity store: an id table for lookup plus a vector sorted by id for ordered iteration.
;;;; Ids only grow, so creating an entity appends; undoing a deletion re-inserts at the position
;;;; its id dictates, which restores the stacking order.

(defconstant +initial-store-capacity+ 64
  "Initial length of the store's vector; it doubles when full.")

(defstruct (entity-store (:conc-name store-)
                         (:constructor make-entity-store ())
                         (:copier nil))
  "All entities of a drawing, ordered by id.

TABLE: id to entity.
VECTOR: entities sorted by ascending id; only the first COUNT slots are meaningful.
COUNT: number of entities."
  (table (make-hash-table) :type hash-table :read-only t)
  (vector (make-array +initial-store-capacity+ :initial-element nil) :type simple-vector)
  (count 0 :type fixnum))

(declaim (inline store-entity-at))
(defun store-entity-at (store
                        index)
  "Return the INDEX-th entity in id order.

STORE: an entity store.
INDEX: position from 0 below the count."
  (svref (store-vector store) index))

(defun store-find (store
                   id)
  "Return the entity with ID, or NIL.

STORE: an entity store.
ID: entity id."
  (values (gethash id (store-table store))))

(defun lower-bound-index (store
                          id)
  "Return the index of the first entity whose id is not less than ID (the count when none).

STORE: an entity store.
ID: the id searched for."
  (let ((low 0)
        (high (store-count store))
        (vector (store-vector store)))
    (declare (type fixnum low high))
    (loop while (< low high)
          do (let ((middle (ash (+ low high) -1)))
               (if (< (entity-id (svref vector middle)) id)
                   (setf low (1+ middle))
                   (setf high middle))))
    low))

(defun ensure-store-capacity (store
                              needed)
  "Grow the store's vector so it can hold NEEDED entities.

STORE: an entity store.
NEEDED: required capacity."
  (let ((vector (store-vector store)))
    (when (> needed (length vector))
      (let ((bigger (make-array (max needed (* 2 (length vector))) :initial-element nil)))
        (replace bigger vector :end2 (store-count store))
        (setf (store-vector store) bigger)))))

(defun store-add (store
                  entity)
  "Insert ENTITY at the position its id dictates. Signals an error when the id exists.

STORE: an entity store.
ENTITY: the entity to add."
  (let ((id (entity-id entity)))
    (when (gethash id (store-table store))
      (error "Entity id ~d is already in the drawing" id))
    (ensure-store-capacity store (1+ (store-count store)))
    (let* ((position (lower-bound-index store id))
           (vector (store-vector store)))
      (loop for index from (store-count store) above position
            do (setf (svref vector index) (svref vector (1- index))))
      (setf (svref vector position) entity)
      (incf (store-count store))
      (setf (gethash id (store-table store)) entity)
      entity)))

(defun store-position (store
                       id)
  "Return the index of the entity with ID, or NIL when absent.

STORE: an entity store.
ID: entity id."
  (let ((position (lower-bound-index store id)))
    (and (< position (store-count store))
         (= id (entity-id (svref (store-vector store) position)))
         position)))

(defun store-remove (store
                     id)
  "Remove and return the entity with ID. Signals an error when absent.

STORE: an entity store.
ID: entity id."
  (let ((position (store-position store id)))
    (unless position
      (error "Entity id ~d is not in the drawing" id))
    (let ((vector (store-vector store))
          (removed (svref (store-vector store) position)))
      (loop for index from position below (1- (store-count store))
            do (setf (svref vector index) (svref vector (1+ index))))
      (decf (store-count store))
      (setf (svref vector (store-count store)) nil)
      (remhash id (store-table store))
      removed)))

(defun store-replace (store
                      entity)
  "Replace the stored entity that has the same id as ENTITY. Signals an error when absent.

STORE: an entity store.
ENTITY: the new value."
  (let ((position (store-position store (entity-id entity))))
    (unless position
      (error "Entity id ~d is not in the drawing" (entity-id entity)))
    (setf (svref (store-vector store) position) entity
          (gethash (entity-id entity) (store-table store)) entity)))

(defun store-remove-many (store
                          ids)
  "Remove every entity whose id is in IDS in a single pass; return the removed entities.
Signals an error, before changing anything, when an id is absent.

STORE: an entity store.
IDS: list of entity ids."
  (let ((doomed (make-hash-table))
        (removed '()))
    (dolist (id ids)
      (unless (gethash id (store-table store))
        (error "Entity id ~d is not in the drawing" id))
      (setf (gethash id doomed) t))
    (let ((vector (store-vector store))
          (kept 0))
      (declare (type fixnum kept))
      (dotimes (index (store-count store))
        (let ((entity (svref vector index)))
          (if (gethash (entity-id entity) doomed)
              (progn (push entity removed)
                     (remhash (entity-id entity) (store-table store)))
              (progn (setf (svref vector kept) entity)
                     (incf kept)))))
      (loop for index from kept below (store-count store)
            do (setf (svref vector index) nil))
      (setf (store-count store) kept))
    (nreverse removed)))

(defun store-add-many (store
                       entities)
  "Insert many entities in a single merge. Signals an error, before changing anything, when
an id already exists or is repeated.

STORE: an entity store.
ENTITIES: list of entities to add."
  (let ((sorted (sort (copy-list entities) #'< :key #'entity-id))
        (seen (make-hash-table)))
    (dolist (entity sorted)
      (when (or (gethash (entity-id entity) (store-table store))
                (gethash (entity-id entity) seen))
        (error "Entity id ~d is already in the drawing" (entity-id entity)))
      (setf (gethash (entity-id entity) seen) t))
    (let* ((count (store-count store))
           (total (+ count (length sorted)))
           (merged (make-array (max total (length (store-vector store))) :initial-element nil))
           (old (store-vector store))
           (old-index 0)
           (target 0))
      (declare (type fixnum old-index target count))
      (loop while (or (< old-index count) sorted)
            do (if (and sorted
                        (or (>= old-index count)
                            (< (entity-id (first sorted)) (entity-id (svref old old-index)))))
                   (setf (svref merged target) (pop sorted))
                   (progn (setf (svref merged target) (svref old old-index))
                          (incf old-index)))
               (incf target))
      (setf (store-vector store) merged
            (store-count store) total)
      (dotimes (index total)
        (let ((entity (svref merged index)))
          (setf (gethash (entity-id entity) (store-table store)) entity))))
    store))

(defun map-entities (function
                     store)
  "Call FUNCTION on every entity in id order (bottom of the stacking order first).
FUNCTION must not add or remove entities.

FUNCTION: a function of one argument, the entity.
STORE: an entity store."
  (declare (type function function))
  (let ((vector (store-vector store)))
    (dotimes (index (store-count store))
      (funcall function (svref vector index)))))
