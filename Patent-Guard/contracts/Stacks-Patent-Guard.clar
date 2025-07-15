;; Decentralized IP Protection & Licensing Platform
;; A comprehensive smart contract for registering, managing, and licensing intellectual property
;; with built-in dispute resolution and automated expiration tracking

;; =======================================
;; ERROR CONSTANTS
;; =======================================
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-RESOURCE-NOT-FOUND (err u101))
(define-constant ERR-DUPLICATE-REGISTRATION (err u102))
(define-constant ERR-INVALID-INPUT-DATA (err u103))
(define-constant ERR-REGISTRATION-EXPIRED (err u104))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u105))
(define-constant ERR-INVALID-STATUS-TRANSITION (err u106))
(define-constant ERR-INVALID-TIME-DURATION (err u107))
(define-constant ERR-LICENSE-ALREADY-EXISTS (err u108))
(define-constant ERR-OPERATION-NOT-PERMITTED (err u109))

;; =======================================
;; SYSTEM CONSTANTS
;; =======================================
(define-constant contract-administrator tx-sender)
(define-constant blocks-per-year u525600)
(define-constant maximum-royalty-rate u10000)
(define-constant minimum-string-length u1)

;; =======================================
;; INTELLECTUAL PROPERTY CATEGORIES
;; =======================================
(define-constant ip-category-patent u1)
(define-constant ip-category-copyright u2)
(define-constant ip-category-trademark u3)
(define-constant ip-category-trade-secret u4)

;; =======================================
;; REGISTRATION STATUS DEFINITIONS
;; =======================================
(define-constant registration-status-pending u1)
(define-constant registration-status-approved u2)
(define-constant registration-status-rejected u3)
(define-constant registration-status-expired u4)

;; =======================================
;; LICENSE TYPE DEFINITIONS
;; =======================================
(define-constant license-type-exclusive u1)
(define-constant license-type-non-exclusive u2)

;; =======================================
;; DISPUTE STATUS DEFINITIONS
;; =======================================
(define-constant dispute-status-open u1)
(define-constant dispute-status-resolved u2)
(define-constant dispute-status-dismissed u3)

;; =======================================
;; REGISTRATION FEE STRUCTURE (microSTX)
;; =======================================
(define-constant patent-registration-fee u1000000)      ;; 1 STX
(define-constant copyright-registration-fee u500000)     ;; 0.5 STX
(define-constant trademark-registration-fee u750000)     ;; 0.75 STX
(define-constant trade-secret-registration-fee u250000)  ;; 0.25 STX

;; =======================================
;; PROTECTION DURATION (in years)
;; =======================================
(define-constant patent-protection-years u20)
(define-constant copyright-protection-years u70)
(define-constant trademark-protection-years u10)
(define-constant trade-secret-protection-years u99999)

;; =======================================
;; DATA STRUCTURES
;; =======================================

;; Intellectual Property Registry
(define-map intellectual-property-records
  { intellectual-property-identifier: uint }
  {
    property-owner: principal,
    intellectual-property-category: uint,
    property-title: (string-ascii 100),
    detailed-description: (string-ascii 500),
    registration-timestamp: uint,
    expiration-timestamp: uint,
    current-status: uint,
    content-verification-hash: (buff 32)
  }
)

;; Licensing Agreements
(define-map licensing-agreements
  { intellectual-property-identifier: uint, licensed-party: principal }
  {
    agreement-type: uint,
    license-start-timestamp: uint,
    license-end-timestamp: uint,
    royalty-percentage: uint,
    license-active-status: bool
  }
)

;; Dispute Management
(define-map dispute-records
  { dispute-case-identifier: uint }
  {
    disputed-property-id: uint,
    dispute-plaintiff: principal,
    dispute-defendant: principal,
    dispute-detailed-description: (string-ascii 500),
    dispute-filing-timestamp: uint,
    dispute-current-status: uint,
    administrative-resolution: (optional (string-ascii 500))
  }
)

;; =======================================
;; SYSTEM COUNTERS
;; =======================================
(define-data-var intellectual-property-counter uint u0)
(define-data-var dispute-case-counter uint u0)
(define-data-var platform-metadata-uri (optional (string-utf8 256)) none)

;; =======================================
;; VALIDATION HELPER FUNCTIONS
;; =======================================

(define-private (validate-ip-category (category-type uint))
  (or (is-eq category-type ip-category-patent)
      (is-eq category-type ip-category-copyright)
      (is-eq category-type ip-category-trademark)
      (is-eq category-type ip-category-trade-secret)))

(define-private (validate-license-type (license-category uint))
  (or (is-eq license-category license-type-exclusive)
      (is-eq license-category license-type-non-exclusive)))

(define-private (validate-string-input (input-string (string-ascii 100)))
  (> (len input-string) minimum-string-length))

(define-private (validate-description-input (description-text (string-ascii 500)))
  (> (len description-text) minimum-string-length))

(define-private (validate-royalty-rate (royalty-amount uint))
  (<= royalty-amount maximum-royalty-rate))

;; =======================================
;; FEE CALCULATION FUNCTIONS
;; =======================================

(define-private (calculate-registration-fee (ip-category uint))
  (if (is-eq ip-category ip-category-patent)
    patent-registration-fee
    (if (is-eq ip-category ip-category-copyright)
      copyright-registration-fee
      (if (is-eq ip-category ip-category-trademark)
        trademark-registration-fee
        trade-secret-registration-fee))))

(define-private (calculate-protection-duration (ip-category uint) (registration-time uint))
  (let ((protection-years (if (is-eq ip-category ip-category-patent)
                            patent-protection-years
                            (if (is-eq ip-category ip-category-copyright)
                              copyright-protection-years
                              (if (is-eq ip-category ip-category-trademark)
                                trademark-protection-years
                                trade-secret-protection-years)))))
    (+ registration-time (* protection-years blocks-per-year))))

;; =======================================
;; PROPERTY STATUS FUNCTIONS
;; =======================================

(define-private (check-property-expiration (property-id uint))
  (match (map-get? intellectual-property-records { intellectual-property-identifier: property-id })
    property-data (> stacks-block-height (get expiration-timestamp property-data))
    true))

(define-private (verify-property-ownership (property-id uint) (claimed-owner principal))
  (match (map-get? intellectual-property-records { intellectual-property-identifier: property-id })
    property-data (is-eq (get property-owner property-data) claimed-owner)
    false))

(define-private (verify-approved-status (property-id uint))
  (match (map-get? intellectual-property-records { intellectual-property-identifier: property-id })
    property-data (is-eq (get current-status property-data) registration-status-approved)
    false))

;; =======================================
;; CORE REGISTRATION FUNCTIONS
;; =======================================

(define-public (register-intellectual-property 
  (property-category uint) 
  (property-title (string-ascii 100)) 
  (property-description (string-ascii 500)) 
  (verification-hash (buff 32)))
  (let ((new-property-id (+ (var-get intellectual-property-counter) u1))
        (required-fee (calculate-registration-fee property-category))
        (current-timestamp stacks-block-height)
        (calculated-expiration (calculate-protection-duration property-category stacks-block-height)))
    
    ;; Input validation
    (asserts! (validate-ip-category property-category) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-string-input property-title) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-description-input property-description) ERR-INVALID-INPUT-DATA)
    (asserts! (> (len verification-hash) u0) ERR-INVALID-INPUT-DATA)
    
    ;; Process registration fee
    (try! (stx-transfer? required-fee tx-sender contract-administrator))
    
    ;; Create property record
    (map-set intellectual-property-records
      { intellectual-property-identifier: new-property-id }
      {
        property-owner: tx-sender,
        intellectual-property-category: property-category,
        property-title: property-title,
        detailed-description: property-description,
        registration-timestamp: current-timestamp,
        expiration-timestamp: calculated-expiration,
        current-status: registration-status-pending,
        content-verification-hash: verification-hash
      })
    
    ;; Update system counter
    (var-set intellectual-property-counter new-property-id)
    
    (ok new-property-id)))

(define-public (approve-property-registration (property-id uint))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND)))
    
    ;; Input validation
    (asserts! (> property-id u0) ERR-INVALID-INPUT-DATA)
    (asserts! (<= property-id (var-get intellectual-property-counter)) ERR-RESOURCE-NOT-FOUND)
    
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (is-eq (get current-status property-data) registration-status-pending) ERR-INVALID-STATUS-TRANSITION)
    
    ;; Update status
    (map-set intellectual-property-records
      { intellectual-property-identifier: property-id }
      (merge property-data { current-status: registration-status-approved }))
    
    (ok true)))

(define-public (reject-property-registration (property-id uint))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND)))
    
    ;; Input validation
    (asserts! (> property-id u0) ERR-INVALID-INPUT-DATA)
    (asserts! (<= property-id (var-get intellectual-property-counter)) ERR-RESOURCE-NOT-FOUND)
    
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (is-eq (get current-status property-data) registration-status-pending) ERR-INVALID-STATUS-TRANSITION)
    
    ;; Update status
    (map-set intellectual-property-records
      { intellectual-property-identifier: property-id }
      (merge property-data { current-status: registration-status-rejected }))
    
    (ok true)))

;; =======================================
;; OWNERSHIP TRANSFER FUNCTIONS
;; =======================================

(define-public (transfer-property-ownership (property-id uint) (new-owner principal))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND)))
    
    ;; Validation checks
    (asserts! (verify-property-ownership property-id tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (verify-approved-status property-id) ERR-INVALID-STATUS-TRANSITION)
    (asserts! (not (check-property-expiration property-id)) ERR-REGISTRATION-EXPIRED)
    
    ;; Transfer ownership
    (map-set intellectual-property-records
      { intellectual-property-identifier: property-id }
      (merge property-data { property-owner: new-owner }))
    
    (ok true)))

;; =======================================
;; LICENSING MANAGEMENT FUNCTIONS
;; =======================================

(define-public (create-licensing-agreement 
  (property-id uint) 
  (licensee-party principal) 
  (agreement-type uint) 
  (license-duration uint) 
  (royalty-rate uint))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND))
        (license-start-time stacks-block-height)
        (license-end-time (+ stacks-block-height license-duration)))
    
    ;; Input validation
    (asserts! (> property-id u0) ERR-INVALID-INPUT-DATA)
    (asserts! (<= property-id (var-get intellectual-property-counter)) ERR-RESOURCE-NOT-FOUND)
    (asserts! (not (is-eq licensee-party tx-sender)) ERR-INVALID-INPUT-DATA)
    (asserts! (not (is-eq licensee-party contract-administrator)) ERR-INVALID-INPUT-DATA)
    (asserts! (> license-duration u0) ERR-INVALID-TIME-DURATION)
    (asserts! (validate-license-type agreement-type) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-royalty-rate royalty-rate) ERR-INVALID-INPUT-DATA)
    
    ;; Validation checks
    (asserts! (verify-property-ownership property-id tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (verify-approved-status property-id) ERR-INVALID-STATUS-TRANSITION)
    (asserts! (not (check-property-expiration property-id)) ERR-REGISTRATION-EXPIRED)
    
    ;; Check for existing license
    (asserts! (is-none (map-get? licensing-agreements { intellectual-property-identifier: property-id, licensed-party: licensee-party })) ERR-LICENSE-ALREADY-EXISTS)
    
    ;; Create licensing agreement
    (map-set licensing-agreements
      { intellectual-property-identifier: property-id, licensed-party: licensee-party }
      {
        agreement-type: agreement-type,
        license-start-timestamp: license-start-time,
        license-end-timestamp: license-end-time,
        royalty-percentage: royalty-rate,
        license-active-status: true
      })
    
    (ok true)))

(define-public (terminate-licensing-agreement (property-id uint) (licensee-party principal))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND))
        (license-data (unwrap! (map-get? licensing-agreements { intellectual-property-identifier: property-id, licensed-party: licensee-party }) ERR-RESOURCE-NOT-FOUND)))
    
    ;; Input validation
    (asserts! (> property-id u0) ERR-INVALID-INPUT-DATA)
    (asserts! (<= property-id (var-get intellectual-property-counter)) ERR-RESOURCE-NOT-FOUND)
    (asserts! (not (is-eq licensee-party tx-sender)) ERR-INVALID-INPUT-DATA)
    (asserts! (not (is-eq licensee-party contract-administrator)) ERR-INVALID-INPUT-DATA)
    
    ;; Validation checks
    (asserts! (verify-property-ownership property-id tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (get license-active-status license-data) ERR-INVALID-STATUS-TRANSITION)
    
    ;; Terminate license
    (map-set licensing-agreements
      { intellectual-property-identifier: property-id, licensed-party: licensee-party }
      (merge license-data { license-active-status: false }))
    
    (ok true)))

;; =======================================
;; DISPUTE RESOLUTION FUNCTIONS
;; =======================================

(define-public (file-property-dispute (property-id uint) (accused-party principal) (dispute-description (string-ascii 500)))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND))
        (new-dispute-id (+ (var-get dispute-case-counter) u1)))
    
    ;; Input validation
    (asserts! (> property-id u0) ERR-INVALID-INPUT-DATA)
    (asserts! (<= property-id (var-get intellectual-property-counter)) ERR-RESOURCE-NOT-FOUND)
    (asserts! (not (is-eq accused-party tx-sender)) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-description-input dispute-description) ERR-INVALID-INPUT-DATA)
    
    ;; Validation checks
    (asserts! (verify-approved-status property-id) ERR-INVALID-STATUS-TRANSITION)
    
    ;; Create dispute record
    (map-set dispute-records
      { dispute-case-identifier: new-dispute-id }
      {
        disputed-property-id: property-id,
        dispute-plaintiff: tx-sender,
        dispute-defendant: accused-party,
        dispute-detailed-description: dispute-description,
        dispute-filing-timestamp: stacks-block-height,
        dispute-current-status: dispute-status-open,
        administrative-resolution: none
      })
    
    ;; Update dispute counter
    (var-set dispute-case-counter new-dispute-id)
    
    (ok new-dispute-id)))

(define-public (resolve-property-dispute (dispute-id uint) (resolution-details (string-ascii 500)))
  (let ((dispute-data (unwrap! (map-get? dispute-records { dispute-case-identifier: dispute-id }) ERR-RESOURCE-NOT-FOUND)))
    
    ;; Input validation
    (asserts! (> dispute-id u0) ERR-INVALID-INPUT-DATA)
    (asserts! (<= dispute-id (var-get dispute-case-counter)) ERR-RESOURCE-NOT-FOUND)
    (asserts! (validate-description-input resolution-details) ERR-INVALID-INPUT-DATA)
    
    ;; Authorization and validation checks
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (is-eq (get dispute-current-status dispute-data) dispute-status-open) ERR-INVALID-STATUS-TRANSITION)
    
    ;; Update dispute with resolution
    (map-set dispute-records
      { dispute-case-identifier: dispute-id }
      (merge dispute-data { 
        dispute-current-status: dispute-status-resolved,
        administrative-resolution: (some resolution-details)
      }))
    
    (ok true)))

;; =======================================
;; PROPERTY RENEWAL FUNCTIONS
;; =======================================

(define-public (renew-trademark-registration (property-id uint))
  (let ((property-data (unwrap! (map-get? intellectual-property-records { intellectual-property-identifier: property-id }) ERR-RESOURCE-NOT-FOUND))
        (renewal-fee (calculate-registration-fee (get intellectual-property-category property-data)))
        (extended-expiration (+ (get expiration-timestamp property-data) (* trademark-protection-years blocks-per-year))))
    
    ;; Validation checks
    (asserts! (verify-property-ownership property-id tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (is-eq (get intellectual-property-category property-data) ip-category-trademark) ERR-OPERATION-NOT-PERMITTED)
    (asserts! (verify-approved-status property-id) ERR-INVALID-STATUS-TRANSITION)
    
    ;; Process renewal fee
    (try! (stx-transfer? renewal-fee tx-sender contract-administrator))
    
    ;; Extend expiration date
    (map-set intellectual-property-records
      { intellectual-property-identifier: property-id }
      (merge property-data { expiration-timestamp: extended-expiration }))
    
    (ok true)))

;; =======================================
;; READ-ONLY QUERY FUNCTIONS
;; =======================================

(define-read-only (get-property-details (property-id uint))
  (map-get? intellectual-property-records { intellectual-property-identifier: property-id }))

(define-read-only (get-licensing-agreement-details (property-id uint) (licensee-party principal))
  (map-get? licensing-agreements { intellectual-property-identifier: property-id, licensed-party: licensee-party }))

(define-read-only (get-dispute-case-details (dispute-id uint))
  (map-get? dispute-records { dispute-case-identifier: dispute-id }))

(define-read-only (check-valid-license (property-id uint) (licensed-user principal))
  (match (map-get? licensing-agreements { intellectual-property-identifier: property-id, licensed-party: licensed-user })
    license-data (and (get license-active-status license-data)
                      (>= stacks-block-height (get license-start-timestamp license-data))
                      (<= stacks-block-height (get license-end-timestamp license-data)))
    false))

(define-read-only (get-total-registered-properties)
  (var-get intellectual-property-counter))

(define-read-only (get-total-dispute-cases)
  (var-get dispute-case-counter))

(define-read-only (confirm-property-ownership (property-id uint) (claimed-owner principal))
  (verify-property-ownership property-id claimed-owner))

(define-read-only (get-registration-fee-amount (property-category uint))
  (if (validate-ip-category property-category)
    (ok (calculate-registration-fee property-category))
    ERR-INVALID-INPUT-DATA))

(define-read-only (check-property-expiration-status (property-id uint))
  (check-property-expiration property-id))

(define-read-only (get-platform-administrator)
  contract-administrator)

;; =======================================
;; PLATFORM METADATA FUNCTIONS
;; =======================================

(define-public (set-platform-metadata-uri (metadata-uri (string-utf8 256)))
  (begin
    ;; Input validation
    (asserts! (> (len metadata-uri) u0) ERR-INVALID-INPUT-DATA)
    (asserts! (< (len metadata-uri) u257) ERR-INVALID-INPUT-DATA)
    
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    
    (ok (var-set platform-metadata-uri (some metadata-uri)))))

(define-read-only (get-platform-metadata-uri)
  (ok (var-get platform-metadata-uri)))