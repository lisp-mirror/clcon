# Serializing lisp objects to tcl and vice versa #

## Concepts 

Swank/Slime use lisp format to communicate to each other. Of course, user-typed commands and results are encoded into lisp strings, but communication protocol itself is based on (limited subset of) s-expressions. So printed representation of conses is passed through sockets. See \*slime-events\* in EMACS to see the data. To deal with that data in clcon, we need to be able to handle some sorts of lisp data in tcl. 

When I need to pass lisp data to tcl, I encode it as strings with type tags. Having that string on the tcl side, you can:  

i) learn lisp type of data
ii) extract data in a string form

After you extracted data, you can not know what type it was.  So when you code working with lisp data in tcl, check type before extracting data or rely on the knowledge of the format of lisp data you have received. 

To pass lisp data from tcl to lisp, you can just use tcl strings, but you must quote strings appropriately to avoid problems with spaces and other special characters. 

## Warning
API described was not tested thoroughly. We usually assume that we get "normal" things, e.g. that symbol names do not contain spaces, dollar signs, sharpsigns etc. Some problems can occur sometimes. We definitely need some more testing. 

## Dictionary 
[cons.tcl](../cons.tcl) contains functions to work with lisp data. They are (as the time of writing) in ::mprs namespace. Lisp objects initially come in in a "leashed" form. The form contains type type tag so you can now which lisp type it represents.
```::mprs::Consp $LeashedDataFromLisp``` - check if the object is a cons
If it is not a cons, extract lisp type tag with 
```::mprs::TypeTag $LeashedDataFromLisp```
Tags are encoded in lisp function ```clcon-server::my-tcl-form``` (see it). As the time of writing,
known tags are:
```:``` - keyword
```y``` - other symbol 
```s``` - string
```n``` - number

As you found a type, you can extract lisp data with 
```[::mprs::Unleash $LeashedStringFromLisp]```
Also there is such sugar as ::mprs::Car which returns leashed first element of a list. 
Rules of converting to string are the following:

 - **string** will be returned as tcl string, which can be safely passed as a single argument to tcl functions (does not disintegrate into list when you use $x)
 - **keywords** - as downcased name (this is SWANK/SLIME's tradition). If you are sure that your leashed lisp string is a **keyword**, you can omit call to ```Unleash``` alltogether - Unleash indeed returns keywords "as is".
 - **symbol** - as a string package::name (package is "nil" for symbols without home package, no case transformation), 
 - **numbers** as their printed lisp representation
 


Avoid passing funny symbols with names like ```"a b"``` into tcl, this is untested and this can break your code. 

List is decoded into tcl list of leashed objects. Use
```[lindex [::lide::Unleash $LeashedLispList] 2]``` to extract (nth LispList 2).
Extracted object is in a leashed form, so you can again learn its type and so on. Dotted pairs (cons with atomic cdr's) are unsopported. Consequences undefined if you try to encode it on lisp side.
*FIXME we should check it in clcon-server::my-tcl-form.* 

If you know that all elements of your list are atoms and you know their text contain no spaces, you can use
```[::lide::UnleashListOfAtoms $LeashedListOfAtomsWithoutSpacesInside]```

## Serializing data from tcl to lisp ##
When I need to call lisp from tcl, I just use strings to form lisp expression. See uses of EvalSometimes lisp expects lisp data in string form. E.g. for completion we must send "partial-symbo-name", not just *partial-symbol-name* . For that cases I use ::tkcon::QuoteLispToString: 

```set Quoted [::tkcon::QuoteLispObjToString $str]
set LispCmd "(cl:progn (clcon-server:server-lookup-definition $Quoted))"
set SwankReply [::tkcon::EvalInSwankSync $LispCmd]```

QuoteLispToString is not thoroughly tested for all types of lisp data, beware!


