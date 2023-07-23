(ns animated-potato.core)

(defn lvar [c]
  (symbol (str "?" c)))

(defn lvar? [x]
  (and (symbol? x)
       (= (first (name x)) \?)))

(defn lvar=? [a b]
  (and (lvar? a)
       (lvar? b)
       (= (name a) (name b))))

(defn walk [u s]
  (let [pr (and (lvar? u)
                (first (filter (fn [[v _ :as r]] (lvar=? v u))
                               s)))]
    (if pr
      (walk (second pr) s)
      u)))

(defn ext-s [x v s]
  (assoc s x v))

(def mzero (lazy-seq nil))

(defn mplus [$1 $2]
  (cond (nil? $1) $2
        (fn? $1) (fn [] (mplus ($1) $2))
        :else (cons (first $1)
                    (mplus (rest $1) $2))))

(defn unit [s-c]
  (lazy-seq (cons s-c mzero)))

(defn lseq? [x]
  (and (sequential? x)
       (= (count x) 2)))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond (and (lvar? u)
               (lvar? v)
               (lvar=? u v))
          s
          (lvar? u)
          (ext-s u v s)
          (lvar? v)
          (ext-s v u s))))

(defn ≡ [u v]
  (fn [[s c :as s-c]]
    (let [s (unify u v s)]
      (if s
        (unit [s c])
        mzero))))

(defn fresh [f]
  (fn [[s c :as s-c]]
    ((f (lvar c)) [s (inc c)])))

(defn disj [g1 g2]
  (fn [s-c]
    (mplus (g1 s-c) (g2 s-c))))

(defn lconj [g1 g2]
  (fn [s-c]
    (bind (g1 s-c) g2)))

