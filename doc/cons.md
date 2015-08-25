# Serializing lisp objects to tcl and vice versa (beta) #

## Concepts 

Swank/Slime use some subset of sexp format to communicate messages to each other. See \*slime-events\* EMACS buffer to see the data. 

Inside the messages, all user-typed data and printed results are encoded as strings. Here we consider passing messages themselves, not user data. 

To deal with that data in tcl, we encode it to string on the lisp side. As we have that so-called "leashed" string in tcl, we can: 

- enquery lisp type of the data
- extract the data in a string form (type information is lost)

To pass lisp data from tcl to lisp, we use strings and add special quoting for user-entered data.

## Lisp functions ##

###clcon-server::my-tcl-form###
Encodes lisp data into a "leashed" string. 

## Tcl functions ##
Source: ([cons.tcl](../cons.tcl)). 

###::mprs::Consp###
```::mprs::Consp $LeashedDataFromLisp``` - check if the object is a cons

###::mprs::TypeTag###
Extract type tag of lisp atom. 
Tags:

- ```:``` - keyword
- ```y``` - other symbol 
- ```s``` - string
- ```n``` - number

###::mprs::Unleash###
Extracts data from leashed string. Lisp object converted to tcl as follows:

- **string** will turn into a tcl string, which can be safely passed as a single argument to tcl functions (does not disintegrate into list when you use $x)
- **keyword** - downcased keyword (this is SWANK/SLIME's tradition). If you are sure that your leashed lisp string represents a **keyword**, you can omit call to ```Unleash``` alltogether - Unleash indeed returns keywords "as is".
- **symbol** - a string package::name (package is "nil" for symbols without home package, no case transformation); avoid symbols like ```|a b|```
- **number** - printed lisp representation
- **list** - tcl list of leashed objects. E.g. 
```set nth2 [lindex [::mprs::Unleash $LeashedLispList] 2]```
```puts [::mprs::Consp $nth2]``` 

will print 1 if (nth LispList 2) is a cons. 

- **conses with atomic cdrs* - just don't try them. *FIXME we should assert it in clcon-server::my-tcl-form.* 
- **other objects* - encoding error 

###::mprs::Car###
Sugar. Returns in a leashed form first element of unleashed list. E.g. ```puts [::mprs::Consp [::mprs::Car $MyLeashedLispList]]``` will print 1 if (car lisp-list) is a cons. 

###::mprs::UnleashListOfAtoms tcl###
Sugar. Unleashes the list and all its elements. Useful when you have list of symbols or numbers.


# Serializing data from tcl to lisp #
When I need to call lisp from tcl, I just use tcl string-handling procedures to form lisp expression. Search tcl source for 'EvalInSwank' pattern. User-entered data must be passed as a string. E.g. when we complete "defu", we must send "defu", not just *defu* . 

## tcl functions ##
###::tkcon::QuoteLispToString###
E.g. example from find definition machinery: 
```set Quoted [::tkcon::QuoteLispObjToString $str]```
```set LispCmd "(cl:progn (clcon-server:server-lookup-definition $Quoted))"```
```set SwankReply [::tkcon::EvalInSwankSync $LispCmd]```

QuoteLispToString was really tested only for passing symbol names, beware!

## lisp functions ##
None. Code must be sent in a form such that SWANK could read it.

