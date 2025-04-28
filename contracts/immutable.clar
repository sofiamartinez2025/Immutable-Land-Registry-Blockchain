;; Immutable Land Registry Blockchain System
;; Transparent and tamper-proof digital land title documentation system utilizing blockchain technology for permanent record keeping



;; ========== STATE VARIABLES ==========
;; Global registry entry counter
(define-data-var entry-counter uint u0)

;; ========== DATA STRUCTURES ==========
;; Main registry database for land titles
(define-map land-title-registry
  { entry-id: uint }
  {
    title-name: (string-ascii 64),
    title-owner: principal,
    document-size: uint,
    registration-block: uint,
    description: (string-ascii 128),
    tags: (list 10 (string-ascii 32))
  }
)

;; Permission management for title viewing
(define-map view-permissions
  { entry-id: uint, viewer: principal }
  { can-view: bool }
)

;; ========== CONSTANTS ==========
;; System status codes for responses

(define-constant error-not-title-holder (err u306))
(define-constant error-registry-unauthorized (err u300))
(define-constant error-entry-not-found (err u301))
(define-constant error-duplicate-entry (err u302))
(define-constant error-invalid-title-name (err u303))
(define-constant error-invalid-document-size (err u304))
(define-constant error-permission-denied (err u305))
(define-constant error-viewing-restricted (err u307))
(define-constant error-invalid-tag-format (err u308))

;; System administrator identifier
(define-constant registry-admin tx-sender)

;; ========== HELPER FUNCTIONS ==========
;; Validates if a tag meets the required format
(define-private (is-valid-tag (tag (string-ascii 32)))
  (and
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; Ensures all tags in a set meet formatting requirements
(define-private (validate-tag-collection (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter is-valid-tag tags)) (len tags))
  )
)

;; Checks if entry exists in the registry
(define-private (entry-exists (entry-id uint))
  (is-some (map-get? land-title-registry { entry-id: entry-id }))
)

;; Verifies if a principal is the title owner
(define-private (is-title-owner (entry-id uint) (user principal))
  (match (map-get? land-title-registry { entry-id: entry-id })
    title-data (is-eq (get title-owner title-data) user)
    false
  )
)

;; Retrieves document size for an entry
(define-private (get-document-size (entry-id uint))
  (default-to u0
    (get document-size
      (map-get? land-title-registry { entry-id: entry-id })
    )
  )
)

;; ========== PUBLIC FUNCTIONS ==========
;; Registers a new land title with complete information
(define-public (register-land-title
  (title (string-ascii 64))
  (doc-size uint)
  (desc (string-ascii 128))
  (tag-list (list 10 (string-ascii 32)))
)
  (let
    (
      (next-id (+ (var-get entry-counter) u1))
    )
    ;; Validate input parameters
    (asserts! (> (len title) u0) error-invalid-title-name)
    (asserts! (< (len title) u65) error-invalid-title-name)
    (asserts! (> doc-size u0) error-invalid-document-size)
    (asserts! (< doc-size u1000000000) error-invalid-document-size)
    (asserts! (> (len desc) u0) error-invalid-title-name)
    (asserts! (< (len desc) u129) error-invalid-title-name)
    (asserts! (validate-tag-collection tag-list) error-invalid-tag-format)

    ;; Create new title entry
    (map-insert land-title-registry
      { entry-id: next-id }
      {
        title-name: title,
        title-owner: tx-sender,
        document-size: doc-size,
        registration-block: block-height,
        description: desc,
        tags: tag-list
      }
    )

    ;; Grant view permission to creator
    (map-insert view-permissions
      { entry-id: next-id, viewer: tx-sender }
      { can-view: true }
    )

    ;; Update registry counter
    (var-set entry-counter next-id)
    (ok next-id)
  )
)

;; Update an existing land title information
(define-public (update-land-title
  (entry-id uint)
  (new-title (string-ascii 64))
  (new-doc-size uint)
  (new-desc (string-ascii 128))
  (new-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (title-data (unwrap! (map-get? land-title-registry { entry-id: entry-id })
        error-entry-not-found))
    )
    ;; Validate ownership and input parameters
    (asserts! (entry-exists entry-id) error-entry-not-found)
    (asserts! (is-eq (get title-owner title-data) tx-sender) error-not-title-holder)
    (asserts! (> (len new-title) u0) error-invalid-title-name)
    (asserts! (< (len new-title) u65) error-invalid-title-name)
    (asserts! (> new-doc-size u0) error-invalid-document-size)
    (asserts! (< new-doc-size u1000000000) error-invalid-document-size)
    (asserts! (> (len new-desc) u0) error-invalid-title-name)
    (asserts! (< (len new-desc) u129) error-invalid-title-name)
    (asserts! (validate-tag-collection new-tags) error-invalid-tag-format)

    ;; Update title with new information
    (map-set land-title-registry
      { entry-id: entry-id }
      (merge title-data {
        title-name: new-title,
        document-size: new-doc-size,
        description: new-desc,
        tags: new-tags
      })
    )
    (ok true)
  )
)

;; Remove a land title from the registry
(define-public (delete-land-title (entry-id uint))
  (let
    (
      (title-data (unwrap! (map-get? land-title-registry { entry-id: entry-id })
        error-entry-not-found))
    )
    ;; Verify ownership
    (asserts! (entry-exists entry-id) error-entry-not-found)
    (asserts! (is-eq (get title-owner title-data) tx-sender) error-not-title-holder)

    ;; Remove the title entry
    (map-delete land-title-registry { entry-id: entry-id })
    (ok true)
  )
)

;; Transfer title ownership to another person
(define-public (transfer-title (entry-id uint) (new-owner principal))
  (let
    (
      (title-data (unwrap! (map-get? land-title-registry { entry-id: entry-id })
        error-entry-not-found))
    )
    ;; Verify caller is the current owner
    (asserts! (entry-exists entry-id) error-entry-not-found)
    (asserts! (is-eq (get title-owner title-data) tx-sender) error-not-title-holder)

    ;; Update ownership information
    (map-set land-title-registry
      { entry-id: entry-id }
      (merge title-data { title-owner: new-owner })
    )
    (ok true)
  )
)

