(ns clj-jri.R
  (:refer-clojure :exclude [eval])
  (:require [clojure.string]))

;This is a simple Clojure wrapper for JRI, Java-to-R bridge.
;refer to https://github.com/s-u/rJava
;refer to http://www.rosuda.org/r/nightly/javadoc/org/rosuda/JRI/Rengine.html

;You have to set the following environment variables.
;(example) export R_HOME=/usr/lib/R/
;(example) export LD_LIBRARY_PATH=./lib
;You may need to install R library "JGR/rJava" (https://rforge.net/JGR/linux.html)
;on R by "install.packages('rJava')"

(defn check-env [& [verbose]]
  (when (or (when (nil? (System/getenv "R_HOME"))
              (println "Environment variable R_HOME is not set.")
              true)
            (when (nil? (System/getenv "LD_LIBRARY_PATH"))
              (println "Environment variable LD_LIBRARY_PATH is not set.")
              true))
    (throw (Exception. (str "Check your environment variables."))))
  (when verbose
    (println "JAVA_HOME:" (System/getenv "JAVA_HOME"))
    (println "R_HOME:" (System/getenv "R_HOME"))
    (println "LD_LIBRARY_PATH:" (System/getenv "LD_LIBRARY_PATH"))))

(defn load-JRI [& [verbose]]
  (when verbose
    (println "loading JRI"))
  (try
    (import '[org.rosuda.JRI Rengine REXP RMainLoopCallbacks])
    (catch Exception ex (throw (Exception. (str "Failed loading org.rosuda.JRI.Rengine: " (.getMessage ex)))))))

; REngine is a singleton. Note this code may not be suitable for multi-threaded codes.
; *R* is only for internal inspection.
(def ^:private ^:dynamic *R* (atom nil))
(def ^:private ^:dynamic *options* (atom nil))

(defn get-instance
  "returns R instance. If necessary, executes R and returns it.
   the 'options' is a map and an example is as follows;
   (get-R-instance {:verbose false, :R-options [], :conversion true})"
  [& [options]]
  (assert (or (nil? options)(map? options)))
  (let [verbose (get options :verbose false)
        R-options (into [] (concat ["--no-save"] (get options :R-options [])))
        runMainLoop false
        initialCallbacks nil]
    ;callback functions are not implemented here.
    ;refer http://www.rosuda.org/r/nightly/javadoc/org/rosuda/JRI/RMainLoopCallbacks.html
    ;http://acs.lbl.gov/NetLoggerBlog/?p=54
    (or (org.rosuda.JRI.Rengine/getMainEngine)
        (do (check-env verbose)
            (load-JRI verbose)
            (reset! *options* options)
            (reset! *R*
               (org.rosuda.JRI.Rengine. (into-array String R-options)
                                        runMainLoop initialCallbacks))
            (when-not (.waitForR ^org.rosuda.JRI.Rengine @*R*)
              (throw (Exception. "Could not start up R.")))
            @*R*))))

(defn shutdown []
  (.end ^org.rosuda.JRI.Rengine (get-instance)))

(defn get-version []
  (org.rosuda.JRI.Rengine/getVersion))

(defmulti r->clj (fn [v & _]
                    (let [jri-type (type v)]
                      (if (instance? org.rosuda.JRI.REXP v)
                        (.getType v)
                        jri-type))))

(defmethod r->clj :default
  [v & _]
  v)

(defmethod r->clj org.rosuda.JRI.REXP/XT_NULL
  [^org.rosuda.JRI.REXP _ & _]
  nil)

(defmethod r->clj org.rosuda.JRI.REXP/XT_NONE
  [^org.rosuda.JRI.REXP _ & _]
  nil)

(defmethod r->clj org.rosuda.JRI.REXP/XT_INT
  [^org.rosuda.JRI.REXP v & _]
  (.asInt v))

(defmethod r->clj org.rosuda.JRI.REXP/XT_DOUBLE
  [^org.rosuda.JRI.REXP v & _]
  (.asDouble v))

(defmethod r->clj org.rosuda.JRI.REXP/XT_STR
  [^org.rosuda.JRI.REXP v & _]
  (.asString v))

(defmethod r->clj org.rosuda.JRI.REXP/XT_LANG
  [^org.rosuda.JRI.REXP v & _]
  (.asList v))

(defmethod r->clj org.rosuda.JRI.REXP/XT_SYM
  [^org.rosuda.JRI.REXP v & _]
  (.asSymbolName v))

(defmethod r->clj org.rosuda.JRI.REXP/XT_BOOL
  [^org.rosuda.JRI.REXP v & [options]]
  (r->clj (.asBool v) options))

(defmethod r->clj org.rosuda.JRI.REXP/XT_VECTOR
  [^org.rosuda.JRI.REXP v & [options]]
  (r->clj (.asVector v) options))

(defmethod r->clj org.rosuda.JRI.REXP/XT_LIST
  [^org.rosuda.JRI.REXP v & [options]]
  (r->clj (.asList v) options))

(defmethod r->clj org.rosuda.JRI.REXP/XT_FACTOR
  [^org.rosuda.JRI.REXP v & [options]]
  (r->clj (.asFactor v) options))

(defmethod r->clj org.rosuda.JRI.REXP/XT_ARRAY_DOUBLE
  [^org.rosuda.JRI.REXP v & [{conversion :conversion}]]
  (cond (= conversion :raw) (.asDoubleArray v)
        (= conversion :single) (.asDouble v)
        (= conversion :matrix) (vec (.asDoubleMatrix v))
        :else (vec (.asDoubleArray v))))

(defmethod r->clj org.rosuda.JRI.REXP/XT_ARRAY_INT
  [^org.rosuda.JRI.REXP v & [{conversion :conversion}]]
  (cond (= conversion :raw) (.asDoubleArray v)
        (= conversion :single) (.asDouble v)
        (= conversion :matrix) (vec (.asDoubleMatrix v))
        :else (vec (.asDoubleArray v))))

(defmethod r->clj org.rosuda.JRI.REXP/XT_ARRAY_STR
  [^org.rosuda.JRI.REXP v & [{conversion :conversion}]]
  (cond (= conversion :raw) (.asStringArray v)
        (= conversion :single) (.asString v)
        :else (vec (.asStringArray v))))

(defmethod r->clj org.rosuda.JRI.RBool
  [^org.rosuda.JRI.RBool v & _]
  (cond (.isNA v) nil
        (.isTRUE v) true
        (.isFalse v) false))

(defmethod r->clj org.rosuda.JRI.RVector
  [^org.rosuda.JRI.RVector v & [options]]
  (cond (= (:conversion options) :data-frame) 
        (apply mapv
               (fn [& values]
                 (zipmap (map keyword (.getNames v)) values))
               (map #(r->clj % options) v))

        (= (:conversion options) :no-recursion)
        (zipmap (map keyword (.getNames v))
                v)

        :else
        (zipmap (map keyword (.getNames v))
                (map #(r->clj % options) v))))

(defmethod r->clj org.rosuda.JRI.RList
  [^org.rosuda.JRI.RList v & [options]]
  (let [ks (.keys v)]
  (zipmap (map keyword ks)
          (map #(r->clj (.at v %) options) ks))))

(defmethod r->clj org.rosuda.JRI.RFactor
  [^org.rosuda.JRI.RFactor v & _]
  (let [ids (mapv #(.at v %) (range (.size v)))]
    {:levels (into #{} ids)
     :ids ids}))

(defn eval* [s & [options]]
  (let [v (.eval ^org.rosuda.JRI.Rengine (get-instance options) s)
        op (if (nil? options) *options* options)]
    (if (get op :conversion true)
      (r->clj v op)
      v)))

(defn eval-vec [xs]
  (assert (coll? xs))
  (loop [remainings xs,
         result nil]
    (if (empty? remainings)
      result
      (let [exp (first remainings)
            op (second remainings)]
        (recur (rest remainings)
               (when (string? exp)
                 (if (map? op)
                   (eval* exp op)
                   (eval* exp))))))))

(defn eval [x & [options]]
  (if (vector? x)
    (eval-vec x)
    (let [sx (clojure.string/split x #"\n")]
      (last (doall (map #(eval* % options) sx))))))

(defn eval-source [^String filepath]
  (eval (format "source('%s')"
                (-> (java.io.File. filepath) .getAbsolutePath))))

(defn clj->r [v]
  (cond
    (boolean? v)
    (org.rosuda.JRI.REXP. org.rosuda.JRI.REXP/XT_BOOL v)

    (int? v)
    (org.rosuda.JRI.REXP. org.rosuda.JRI.REXP/XT_INT v)

    (number? v)
    (org.rosuda.JRI.REXP. org.rosuda.JRI.REXP/XT_DOUBLE (double v))

    (string? v)
    (org.rosuda.JRI.REXP. org.rosuda.JRI.REXP/XT_STR v)

    (and (coll? v) (every? int? v))
    (org.rosuda.JRI.REXP.
      org.rosuda.JRI.REXP/XT_ARRAY_INT (int-array v))

    (and (coll? v) (every? double? v))
    (org.rosuda.JRI.REXP.
      org.rosuda.JRI.REXP/XT_ARRAY_DOUBLE (double-array v))

    (and (coll? v) (every? number? v))
    (org.rosuda.JRI.REXP.
      org.rosuda.JRI.REXP/XT_ARRAY_DOUBLE (double-array (map double v)))

    (and (coll? v) (every? string? v))
    (org.rosuda.JRI.REXP.
      org.rosuda.JRI.REXP/XT_ARRAY_STR (into-array String v))

    :else (throw
            (ex-info
              "Conversion to R type failed"
              {:cause (str "Value is an invalid candidate for conversion.")
               :value v}))))

(defn assign [var-name value]
  (assert (string? var-name))
  (let [v (clj->r value)]
    (.assign ^org.rosuda.JRI.Rengine (get-instance)
             ^String var-name
             v)))

(defn assign-tibble 
  "Assign relation, relv, to R/dplyr tibble."
  [tibble-name relv]
  (assert (string? tibble-name))
  (assert (and (coll? relv) (every? map? relv)))
  (eval ["require(dplyr)"
         (format "%s <- tibble(.rows = %d)" tibble-name (count relv))])
  (doseq [col (keys (first relv))
          :let [v (clj->r (map #(get % col) relv))]]
    (.assign ^org.rosuda.JRI.Rengine (get-instance)
             ^String (str "tmp_col_" (name col))
             v)
    (eval [(format "%1$s$%2$s <- tmp_col_%2$s" tibble-name (name col))
           (format "rm(tmp_col_%s)" (name col))])))
