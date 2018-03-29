# 1

*Abelson & Sussman, exercises 2.74, 2.75, 2.76, 2.77, 2.79, 2.80, 2.81, 2.83*

## 2.74
*Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company's computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable's president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.

Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division's personnel records consist of a single file, which contains a set of records keyed on employees' names. The structure of the set varies from division to division. Furthermore, each employee's record is itself a set (structured differently from division to divistion) that contains information keyed under identifiers such as `address` and `salary`. In particular:

a. Implement for headquarters a `get-record` procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. Explain how the individual divisions' files should be structured. In particular, what type of information must be supplied?

b. Implement for headquarters a `get-salary` procedure that returns the salary information from a given employee's record from any division's personnel file. How should the record be structured in order to make this operation work.

c. Implement for headquarters a `find-employee-record` procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the division's files.

d. When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?*

a.
```scheme
(define (get-record division employee)
	(get division employee))
```
As long as each division tags their records with a division identifier, and each employee's record is keyed to the employee's name this will return the employee's record regardless of the remaining structure of their data. In other words each record must be tagged with a division identifier and the employee's name.

b.
```scheme
(define (get-salary division employee)
	(let (employee) (get division employee))
	(get employee salary))
```
Once again, the actual structure of the data can remain untouched as long as each entry in the record is tagged - keyed with consistent identifiers, in this case 'salary'.

c.
```scheme
(define (find-employee-record (employee-name divisions))
	(if (get-record (car divisions) employee-name))
	(get-record (car divisions) employee-name)
	(find-employee-record (employee-name (cdr divisions))))
```

d.
Each division's personell file must be keyed with it's new division identifier, each employee's record must be keyed to their employee identifier (their name, in this case) and it must be verified that each piece of information in an employee's record is tagged with consistent identifiers like 'salary' and 'address'.


	