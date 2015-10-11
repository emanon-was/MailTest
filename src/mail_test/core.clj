(ns mail-test.core
  (:require
   [clojure.string :as str])
  (:import
   [java.io 
    InputStreamReader OutputStreamWriter
    BufferedReader BufferedWriter]
   [java.net Socket InetSocketAddress]
   [java.util Hashtable]
   [javax.naming.directory InitialDirContext]
   [javax.mail.internet InternetAddress]))

(defn- dns-record-seq [attrs]
  (let [attr (if (.hasMore attrs) (.next attrs))]
    (if attr
      (cons attr
            (lazy-seq (dns-record-seq attrs))))))

(defn dns-record-lookup [record host]
  (let [env (doto (Hashtable.)
              (.put "java.naming.factory.initial"
                    "com.sun.jndi.dns.DnsContextFactory"))
        attrs (-> (InitialDirContext. env)
                  (.getAttributes host (into-array String [record]))
                  (.get record))]
    (if attrs
      (dns-record-seq (.getAll attrs)))))

(defn- socket-connect
  [server port & {:keys [timeout]
                  :or   {timeout 200}}]
  (doto (Socket.)
    (.connect (InetSocketAddress. server port)
              timeout)))

(defn- socket-reader [socket]
  (-> socket
      .getInputStream
      InputStreamReader.
      BufferedReader.))

(defn- socket-writer [socket]
  (-> socket .getOutputStream
      OutputStreamWriter.
      BufferedWriter.))

(defn- smtp-response-seq [br]
  (let [line (.readLine br)]
    (println line)
    (if (not= "-" (subs line 3 4))
      (cons line nil)
      (cons line (lazy-seq (smtp-response-seq br))))))

(defn- smtp-status
  ([br] (let [smtp-responses (smtp-response-seq br)]
          (try
            (-> (last smtp-responses)
                (subs 0 3)
                Integer/parseInt)
            (catch Exception e -1))))
  ([br status] (= status (smtp-status br))))

(defn- smtp-command [bw command]
  (println command)
  (doto bw
    (.write (str command "\r\n"))
    .flush))

(defn valid-smtp [email server]
  (println server)
  (with-open [socket (socket-connect server 25)
              reader (socket-reader socket)
              writer (socket-writer socket)]
    (-> (smtp-status reader 220)
        (assert "Invalid header"))
    (smtp-command writer "EHLO localhost")
    (-> (smtp-status reader 250)
        (assert "Not ESMTP"))
    (smtp-command writer (str "MAIL FROM:<" email ">"))
    (-> (smtp-status reader 250)
        (assert "Sender rejected"))
    (smtp-command writer (str "RCPT TO:<" email ">"))
    (-> (smtp-status reader 250))))

(defn valid-email [email]
  (try (-> (InternetAddress. email) .validate) true
       (catch Exception e false)))

(defn mx-server [s]
  (if-let [matches (re-matches #"^\d+\s(.+)\.$" s)]
    (second matches)))

(defn verify [email]
  (if (valid-email email)
    (let [domain  (second (str/split email #"@"))
          servers (or (->> domain (dns-record-lookup "MX") (map mx-server))
                      (->> domain (dns-record-lookup "A")))]
      (valid-smtp email (first servers)))))

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
