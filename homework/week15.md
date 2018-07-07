# 1

*Abelson & Sussman 4.56, 4.57, 4.58, 4.65*

## 4.56

*Formulate compound queries that retrieve the following information:  
1. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;  
2. all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary;  
3. all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job.*

```scheme
; 1.
(and (supervisor ?employee (Ben Bitdiddle))
  (address ?employee ?where))

; 2.
(and
  (salary (Bitdiddle Ben) ?max-salary)
  (salary ?employee ?amount)
  (lisp-value > ?max-salary ?amount))

; 3.
(and (supervisor ?employee ?supervisor)
  (not (job ?supervisor (computer . ?type)))
  (job ?supervisor ?job)
```

## 4.57

*Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person 2 or someone who does person 1's job can also do person 2's job, and if person 1 and person 2 are not the same person. Using your rule, give queries that find the following:  
1. all people who can replace Cy D. Fect;  
2. all people who can replace someone who is being paid more than they are, together with the two salaries.*

```scheme
; 1.  
(rule (can-replace ?person1 ?person2)
  (and
    (job ?person1 ?job1)
    (job ?person2 ?job2)
    (or
      (same ?job1 ?job2)
      (can-do-job ?job1 ?job2))
    (not (same ?person1 ?person2))))

; 2.  
(can-replace ?person (Fect Cy D))

; 3.  
(and
  (salary ?person1 ?s1)
  (salary ?person2 ?s2)
  (can-replace ?person1 ?person2)
  (lisp-value < ?s1 ?s2))
```
