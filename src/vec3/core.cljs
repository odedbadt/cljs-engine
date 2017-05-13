; (ns vec3)


; (defn det [[[a b c]
;             [d e f]
;             [g h i]]]
;   (- (+ (* a e i) (* b f g) (* c d h))
;      (* a f h) (* b d i) (* c e g)))


; (defn inverse [[[a b c]
;                 [d e f]
;                 [g h i] :as mat]]
;   (if-let [det (determinant mat)
;     [[[
;       (/ (- (* e i) (f h)) det
;       (/ (- (* e i) (f h)) det

;       ]]
;   @Override
;   public Matrix33 inverse() {
;     double det=determinant();
;     if (det==0.0) return null;
;     double invDet=1.0/det;
;     return new Matrix33(
;         invDet*((m11*m22-m12*m21)),
;         invDet*((m02*m21-m01*m22)),
;         invDet*((m01*m12-m02*m11)),
;         invDet*((m12*m20-m10*m22)),
;         invDet*((m00*m22-m02*m20)),
;         invDet*((m02*m10-m00*m12)),
;         invDet*((m10*m21-m11*m20)),
;         invDet*((m01*m20-m00*m21)),
;         invDet*((m00*m11-m01*m10)));    
;   }

