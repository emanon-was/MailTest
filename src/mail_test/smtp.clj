(ns mail-test.smtp
  (:import
   [java.io 
    InputStreamReader OutputStreamWriter
    BufferedReader BufferedWriter]
   [java.net Socket InetSocketAddress]))

(defn- socket-reader [socket]
  (-> socket
      .getInputStream
      InputStreamReader.
      BufferedReader.))

(defn- socket-writer [socket]
  (-> socket .getOutputStream
      OutputStreamWriter.
      BufferedWriter.))

(defn- socket-connect [server port timeout]
  (doto (Socket.)
    (.connect (InetSocketAddress. server port) timeout)))

(defrecord SmtpClient [socket reader writer ready]
  java.io.Closeable
  (close [this]
    (.close (:writer this))
    (.close (:reader this))
    (.close (:socket this))))

(defn- response-seq [br]
  (let [line (.readLine br)]
    (println line)
    (if (not= "-" (subs line 3 4))
      (cons line nil)
      (cons line (lazy-seq (response-seq br))))))

(defn status
  ([coll]
     (try (-> (last coll)
              (subs 0 3)
              Integer/parseInt)
          (catch Exception e -1)))
  ([coll code]
     (= code (status coll))))

(defprotocol ISmtpClient
  (ready    [client])
  (response [client])
  (command  [client command])
  (verify   [client email]))

(extend-type SmtpClient
  ISmtpClient

  (ready [client] @(:ready client))

  (response [client]
    (if (ready client)
      (do (reset! (:ready client) false)
          (-> (:reader client)
              response-seq))))

  (command [client command]
    (println command)
    (if (not (ready client))
      (do (reset! (:ready client) true)
          (-> (:writer client)
              (doto (.write (str command "\r\n")) .flush))))
    (response client))

  (verify [client email]
    (-> client
        (command (str "MAIL FROM:<" email ">"))
        (status 250)
        (assert "Sender rejected"))
    (-> client
        (command (str "RCPT TO:<" email ">"))
        (status 250))))

(defn client
  [server port & {:keys [timeout]
                  :or   {timeout 200}}]
  (let [socket (socket-connect server port timeout)
        reader (socket-reader socket)
        writer (socket-writer socket)
        smtp-client (SmtpClient.
                     socket reader writer
                     (atom true))]
    (-> smtp-client
        response
        (status 220)
        (assert "Invalid header"))
    (-> smtp-client
        (command "EHLO localhost")
        (status 250)
        (assert "Not ESMTP"))
    smtp-client))

