(ns pdf.protocols)

(defprotocol IMatrix
  (-display [m])
  (-score [m])
  (-subcol [m s e])
  (-drop [m c])
  (-update [m idx idxs v]))  