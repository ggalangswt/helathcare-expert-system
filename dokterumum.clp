
;;;======================================================
;;;     Healthcare Expert Syste
;;;
;;;     This expert system diagnoses some common
;;;     diseasses from a patient.
;;;
;;;     CLIPS Version 6.4 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (print ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (print ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-demam ""
   (not (demam ?))
   (not (diagnose ?))
   =>
   (assert (demam (yes-or-no-p "Apakah anda mengalami demam? (yes/no)? "))))

(defrule determine-sesak ""
   (not (sesak ?))
   (not (diagnose ?))
   =>
   (assert (sesak (yes-or-no-p "Apakah anda merasa sesak napas? (yes/no)? "))))
   
(defrule determine-batuk ""
   (not (batuk ?))
   (not (diagnose ?))
   =>
   (assert (batuk (yes-or-no-p "Apakah ada batuk? (yes/no)? "))))

(defrule determine-jenis-batuk ""
   (batuk yes)
   (not(diagnose ?))
   =>
   (assert (jenis-batuk (ask-question "Jenis batuk apa yang anda alami? (kering/berdahak)? "
                                 kering berdahak))))

(defrule determine-nyeri ""
   (not (nyeri ?))
   (not (diagnose ?))   
   =>
   (assert (nyeri (yes-or-no-p "Apakah anda mengalami nyeri (yes/no)? "))))

(defrule determine-lokasi-nyeri ""
   (nyeri yes)
   (not(diagnose ?))
   =>
   (assert (lokasi-nyeri (ask-question "Dimana nyeri paling terasa? (dada/perut/kepala/tenggorokan)? "
                                 dada perut kepala tenggorokan))))

(defrule determine-diare ""
   (lokasi-nyeri perut)
   (not (diare ?))
   (not (diagnose ?))
   =>
   (assert (diare (yes-or-no-p "Apakah anda mengalami diare? (yes/no)? "))))
   
(defrule determine-mual-perut ""
   (lokasi-nyeri perut)
   (not (mual ?))
   (not (diagnose ?))
   =>
   (assert (mual (yes-or-no-p "Apakah terasa mual? (yes/no)? "))))

(defrule determine-mual-kepala ""
   (lokasi-nyeri kepala)
   (not (mual ?))
   (not (diagnose ?))
   =>
   (assert (mual (yes-or-no-p "Apakah terasa mual? (yes/no)? "))))

(defrule determine-pilek ""
   (batuk yes)
   (not (pilek ?))
   (not (diagnose ?))
   =>
   (assert (pilek (yes-or-no-p "Apakah anda mengalami pilek (yes/no)? "))))

;;;****************
;;;* Diagnose RULES *
;;;****************


(defrule sakit-asma ""
   (declare (salience 50))
   (sesak yes)
   (jenis-batuk kering)
   (not (diagnose ?))
   =>
   (assert (diagnose "Asma."))
   (assert (tips "Disarankan menghindari pemicu dan berkonsultasi ke tenaga medis."))
   )

(defrule sakit-migrain ""
   (declare (salience 45))
   (lokasi-nyeri kepala)
   (mual yes)
   (not (diagnose ?))
   =>
   (assert (diagnose "Migrain."))
   (assert (tips "Hindari cahaya terang dan istirahat di tempat tenang."))) 

(defrule sakit-bronkitis ""
   (declare (salience 40))
   (jenis-batuk berdahak)
   (lokasi-nyeri dada)
   (not (diagnose ?))
   =>
   (assert (diagnose "Bronkitis."))
   (assert (tips "Disarankan menghindari pemicu dan berkonsultasi ke tenaga medis.")))     

(defrule sakit-faringitis
   (declare (salience 35))
   (demam yes)
   (lokasi-nyeri tenggorokan)
   (not (diagnose ?))
   =>
   (assert (diagnose "Faringitis."))
   (assert (tips "Disarankan menghindari pemicu dan berkonsultasi ke tenaga medis.")))

(defrule sakit-influenza
   (declare (salience 30))
   (demam yes)
   (jenis-batuk kering)
   (lokasi-nyeri tenggorokan)
   (not (diagnose ?))
   =>
   (assert (diagnose "Influenza."))
   (assert (tips "Istirahat cukup dan pantau kondisi Anda.")))

(defrule sakit-common-cold
   (declare (salience 25))
   (demam yes)
   (jenis-batuk berdahak)
   (pilek yes)
   (not (diagnose ?))
   =>
   (assert (diagnose "Common Cold."))
   (assert (tips "Perbanyak istirahat dan cairan.")))

(defrule sakit-diare-akut
   (declare (salience 20))
   (diare yes)
   (lokasi-nyeri perut)
   (not (diagnose ?))
   =>
   (assert (diagnose "Diare Akut."))
   (assert (tips "Pastikan cukup cairan dan periksakan jika berlanjut.")))

(defrule sakit-gastritis
   (declare (salience 15))
   (lokasi-nyeri perut)
   (mual yes)
   (not (diagnose ?))
   =>
   (assert (diagnose "Gastritis."))
   (assert (tips "Hindari makanan pedas/asam dan konsultasikan bila nyeri menetap.")))

(defrule sakit-tension-headache
   (declare (salience 10))
   (lokasi-nyeri kepala)
   (not (diagnose ?))
   =>
   (assert (diagnose "Tension Headache."))
   (assert (tips "Istirahat dan kelola stres.")))

(defrule no-diagnosis ""
  (declare (salience -10))
  (not (diagnose ?))
  =>
  (assert (diagnose "Gejala tidak cukup spesifik. Disarankan konsultasi medis.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 100))
  =>
  (println crlf "Healthcare Expert System" crlf))

(defrule print-diagnose ""
  (declare (salience 5))
  (diagnose ?item)
  (tips ?tips)
  =>
  (println crlf "Diagnosa Kemungkinan:" crlf)
  (println " " ?item crlf)
  (println ?tips crlf)
  (println "Catatan: Sistem ini tidak menggantikan diagnosis dokter." crlf))