(ns mail-test.dns
  (:import
   [java.util Hashtable]
   [javax.naming.directory InitialDirContext]))

(defn- dns-record-seq [attrs]
  (let [attr (if (.hasMore attrs) (.next attrs))]
    (if attr
      (cons attr
            (lazy-seq (dns-record-seq attrs))))))

(defn record [record host]
  (let [env (doto (Hashtable.)
              (.put "java.naming.factory.initial"
                    "com.sun.jndi.dns.DnsContextFactory"))
        attrs (-> (InitialDirContext. env)
                  (.getAttributes host (into-array String [record]))
                  (.get record))]
    (if attrs
      (dns-record-seq (.getAll attrs)))))

(defn mx-record-value [s]
  (if-let [matches (re-matches #"^\d+\s(.+)\.$" s)]
    (second matches)))
