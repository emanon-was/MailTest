(ns mail-test.core
  (:require
   [clojure.string :as str]
   [mail-test.dns  :as dns]
   [mail-test.smtp :as smtp])
  (:import
   [javax.mail.internet InternetAddress]))

(defn valid-smtp [email server]
  (println server)
  (with-open [client (smtp/client server 25)]
    (smtp/verify client email)))

(defn valid-email [email]
  (try (-> (InternetAddress. email) .validate) true
       (catch Exception e false)))

(defn verify [email]
  (if (valid-email email)
    (let [domain  (second (str/split email #"@"))
          servers (or (->> domain (dns/record "MX") (map dns/mx-record-value))
                      (->> domain (dns/record "A")))]
      (valid-smtp email (rand-nth servers)))))

;; mail-test.core> (verify "emanon.was@gmail.com")
;; 220 mx.google.com ESMTP dn5si18046173pbd.244 - gsmtp
;; EHLO localhost
;; 250-mx.google.com at your service, [59.106.191.188]
;; 250-SIZE 35882577
;; 250-8BITMIME
;; 250-STARTTLS
;; 250-ENHANCEDSTATUSCODES
;; 250-PIPELINING
;; 250-CHUNKING
;; 250 SMTPUTF8
;; MAIL FROM:<emanon.was@gmail.com>
;; 250 2.1.0 OK dn5si18046173pbd.244 - gsmtp
;; RCPT TO:<emanon.was@gmail.com>
;; 250 2.1.5 OK dn5si18046173pbd.244 - gsmtp
;; true
