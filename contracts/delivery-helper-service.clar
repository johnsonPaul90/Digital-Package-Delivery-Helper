;; Digital Package Delivery Helper
;; A neighbor assistance system for package receiving when people aren't home

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-REQUEST (err u101))
(define-constant ERR-REQUEST-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-ACCEPTED (err u103))
(define-constant ERR-NOT-HELPER (err u104))
(define-constant ERR-ALREADY-CONFIRMED (err u105))
(define-constant ERR-CANNOT-RATE-SELF (err u106))
(define-constant ERR-INVALID-RATING (err u107))
(define-constant ERR-ALREADY-RATED (err u108))

;; Data Variables
(define-data-var next-request-id uint u1)
(define-data-var timestamp-counter uint u1)

;; Data Maps
(define-map package-requests
    { request-id: uint }
    {
        requester: principal,
        helper: (optional principal),
        description: (string-ascii 500),
        pickup-location: (string-ascii 200),
        delivery-location: (string-ascii 200),
        reward-amount: uint,
        status: (string-ascii 20), ;; "open", "accepted", "delivered", "confirmed", "cancelled"
        created-at: uint,
        accepted-at: (optional uint),
        delivered-at: (optional uint),
        confirmed-at: (optional uint)
    }
)

(define-map user-profiles
    { user: principal }
    {
        total-requests: uint,
        total-helps: uint,
        total-rating-points: uint,
        total-ratings-count: uint,
        trust-score: uint, ;; Calculated score out of 1000
        is-active: bool
    }
)

(define-map request-ratings
    { request-id: uint, rater: principal }
    {
        rating: uint, ;; 1-5 stars
        comment: (string-ascii 200),
        rated-at: uint
    }
)

;; Helper Functions
(define-private (calculate-trust-score (total-points uint) (total-ratings uint))
    (if (is-eq total-ratings u0)
        u500 ;; Default score for new users
        (/ (* total-points u200) total-ratings) ;; Scale to 0-1000
    )
)

(define-private (update-user-trust-score (user principal))
    (let (
        (profile (default-to
            { total-requests: u0, total-helps: u0, total-rating-points: u0,
              total-ratings-count: u0, trust-score: u500, is-active: true }
            (map-get? user-profiles { user: user })
        ))
    )
    (map-set user-profiles { user: user }
        (merge profile {
            trust-score: (calculate-trust-score
                (get total-rating-points profile)
                (get total-ratings-count profile)
            )
        })
    )
    )
)

;; Public Functions

;; Create a new package delivery request
(define-public (create-request
    (description (string-ascii 500))
    (pickup-location (string-ascii 200))
    (delivery-location (string-ascii 200))
    (reward-amount uint)
)
    (let (
        (request-id (var-get next-request-id))
        (current-timestamp (var-get timestamp-counter))
    )
    (asserts! (> (len description) u0) ERR-INVALID-REQUEST)
    (asserts! (> (len pickup-location) u0) ERR-INVALID-REQUEST)
    (asserts! (> (len delivery-location) u0) ERR-INVALID-REQUEST)

    ;; Create the request
    (map-set package-requests { request-id: request-id }
        {
            requester: tx-sender,
            helper: none,
            description: description,
            pickup-location: pickup-location,
            delivery-location: delivery-location,
            reward-amount: reward-amount,
            status: "open",
            created-at: current-timestamp,
            accepted-at: none,
            delivered-at: none,
            confirmed-at: none
        }
    )

    ;; Update user profile
    (let (
        (current-profile (default-to
            { total-requests: u0, total-helps: u0, total-rating-points: u0,
              total-ratings-count: u0, trust-score: u500, is-active: true }
            (map-get? user-profiles { user: tx-sender })
        ))
    )
    (map-set user-profiles { user: tx-sender }
        (merge current-profile {
            total-requests: (+ (get total-requests current-profile) u1),
            is-active: true
        })
    )
    )

    ;; Increment timestamp counter
    (var-set timestamp-counter (+ current-timestamp u1))

    ;; Increment request ID for next request
    (var-set next-request-id (+ request-id u1))

    (ok request-id)
    )
)

;; Accept a delivery request as a helper
(define-public (accept-request (request-id uint))
    (let (
        (request-data (unwrap! (map-get? package-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
        (current-timestamp (var-get timestamp-counter))
    )
    (asserts! (is-eq (get status request-data) "open") ERR-ALREADY-ACCEPTED)
    (asserts! (not (is-eq tx-sender (get requester request-data))) ERR-NOT-AUTHORIZED)

    ;; Update request with helper
    (map-set package-requests { request-id: request-id }
        (merge request-data {
            helper: (some tx-sender),
            status: "accepted",
            accepted-at: (some current-timestamp)
        })
    )

    ;; Update helper's profile
    (let (
        (helper-profile (default-to
            { total-requests: u0, total-helps: u0, total-rating-points: u0,
              total-ratings-count: u0, trust-score: u500, is-active: true }
            (map-get? user-profiles { user: tx-sender })
        ))
    )
    (map-set user-profiles { user: tx-sender }
        (merge helper-profile {
            total-helps: (+ (get total-helps helper-profile) u1),
            is-active: true
        })
    )
    )

    ;; Increment timestamp counter
    (var-set timestamp-counter (+ current-timestamp u1))

    (ok true)
    )
)

;; Mark delivery as completed (called by helper)
(define-public (mark-delivered (request-id uint))
    (let (
        (request-data (unwrap! (map-get? package-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
        (current-timestamp (var-get timestamp-counter))
    )
    (asserts! (is-eq (get status request-data) "accepted") ERR-INVALID-REQUEST)
    (asserts! (is-eq (some tx-sender) (get helper request-data)) ERR-NOT-HELPER)

    ;; Update request status
    (map-set package-requests { request-id: request-id }
        (merge request-data {
            status: "delivered",
            delivered-at: (some current-timestamp)
        })
    )

    ;; Increment timestamp counter
    (var-set timestamp-counter (+ current-timestamp u1))

    (ok true)
    )
)

;; Confirm delivery receipt (called by requester)
(define-public (confirm-delivery (request-id uint))
    (let (
        (request-data (unwrap! (map-get? package-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
        (current-timestamp (var-get timestamp-counter))
    )
    (asserts! (is-eq (get status request-data) "delivered") ERR-INVALID-REQUEST)
    (asserts! (is-eq tx-sender (get requester request-data)) ERR-NOT-AUTHORIZED)

    ;; Update request status
    (map-set package-requests { request-id: request-id }
        (merge request-data {
            status: "confirmed",
            confirmed-at: (some current-timestamp)
        })
    )

    ;; Increment timestamp counter
    (var-set timestamp-counter (+ current-timestamp u1))

    (ok true)
    )
)

;; Rate a completed delivery
(define-public (rate-delivery
    (request-id uint)
    (rating uint)
    (comment (string-ascii 200))
)
    (let (
        (request-data (unwrap! (map-get? package-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
        (current-timestamp (var-get timestamp-counter))
    )
    (asserts! (is-eq (get status request-data) "confirmed") ERR-INVALID-REQUEST)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (is-none (map-get? request-ratings { request-id: request-id, rater: tx-sender })) ERR-ALREADY-RATED)

    ;; Determine who is being rated
    (let (
        (rated-user (if (is-eq tx-sender (get requester request-data))
            (unwrap! (get helper request-data) ERR-NOT-HELPER)
            (get requester request-data)
        ))
    )
    (asserts! (not (is-eq tx-sender rated-user)) ERR-CANNOT-RATE-SELF)

    ;; Store the rating
    (map-set request-ratings { request-id: request-id, rater: tx-sender }
        {
            rating: rating,
            comment: comment,
            rated-at: current-timestamp
        }
    )

    ;; Update rated user's profile
    (let (
        (rated-profile (default-to
            { total-requests: u0, total-helps: u0, total-rating-points: u0,
              total-ratings-count: u0, trust-score: u500, is-active: true }
            (map-get? user-profiles { user: rated-user })
        ))
    )
    (map-set user-profiles { user: rated-user }
        (merge rated-profile {
            total-rating-points: (+ (get total-rating-points rated-profile) rating),
            total-ratings-count: (+ (get total-ratings-count rated-profile) u1)
        })
    )

    ;; Update trust score
    (update-user-trust-score rated-user)
    )

    ;; Increment timestamp counter
    (var-set timestamp-counter (+ current-timestamp u1))
    )

    (ok true)
    )
)

;; Cancel a request (only by requester, only if not accepted)
(define-public (cancel-request (request-id uint))
    (let (
        (request-data (unwrap! (map-get? package-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get requester request-data)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status request-data) "open") ERR-INVALID-REQUEST)

    ;; Update request status
    (map-set package-requests { request-id: request-id }
        (merge request-data { status: "cancelled" })
    )

    (ok true)
    )
)

;; Read-only Functions

;; Get request details
(define-read-only (get-request (request-id uint))
    (map-get? package-requests { request-id: request-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles { user: user })
)

;; Get rating for a specific request and rater
(define-read-only (get-request-rating (request-id uint) (rater principal))
    (map-get? request-ratings { request-id: request-id, rater: rater })
)

;; Get current request ID counter
(define-read-only (get-next-request-id)
    (var-get next-request-id)
)

;; Get trust score for a user
(define-read-only (get-trust-score (user principal))
    (let (
        (profile (map-get? user-profiles { user: user }))
    )
    (match profile
        some-profile (some (get trust-score some-profile))
        none
    )
    )
)

;; Check if user can accept requests (basic eligibility)
(define-read-only (can-accept-requests (user principal))
    (let (
        (profile (map-get? user-profiles { user: user }))
    )
    (match profile
        some-profile (and
            (get is-active some-profile)
            (>= (get trust-score some-profile) u300) ;; Minimum trust score
        )
        true ;; New users can accept requests
    )
    )
)
