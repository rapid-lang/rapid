


# RAPID Language Reference Manual
## Coms W 4115

Ben Edelstein, Brian Shin, Brendon Fish, Dan Schlosser, Nate Brennand

## TODO
- section on `?` operator
- value of primitives after instantiation without setting the value
  - what is the value of number/strings types on instantiation? ""/0/0.0 or null?
  -  IS explained for dictionaries, lists are implicitly 0
- anonymous errors
- go over how this allows you to stack multiple unsafe functions. i.e. show what value this feature 
- list functions? how do I append? Can I get a subset of the list (like l[5:10])
- do we have true OOP?  like are there instances methods on classes?  are routes instance methods?
  - We could do this with an `instance ClassName self {}` block, where `instance` is a keyword like:
	```
	class User {
	     string name
	      
		 instance User self {
			 func get_name(self) string {
				 return self.name
			 }
		 }
	}	 
	```


[toc]

## 1. Introduction TODO empty

## 2. Types

### 2.1 Static Typing

RAPID is a statically typed language; variables must be explicitly typed upon declaration. Variables can be cast to other types (see Casting).

### 2.2 Primitive Types

#### null

In RAPID, the `null` keyword represents

#### Booleans

Boolean values are defined by the `true` and `false` keywords.  Because they are their own type, non-boolean values must be cast to `boolean` in order to be used in logical expressions. 

> **Example:**
> 
> `(3+5)?` is valid RAPID, but `!(3+5)` is not.

(`?` is a an operator on all primitive types that evaluates to the "truthiness" of that value)

#### Integers

Integers are preceded by the type `int`, and represent an 8 byte, signed integer. Integers can be declared and initialized later, or initialized inline.   Uninitialized integers are null.

```
int i     // null
int i = 5 // 5
```

Integers are copied by value.

```
int a = 1
int b = a
a = 2
printf("%d, %d", a, b) // 2 1
```

#### Floating Point Numbers
Floating point numbers are preceded by the type `float`, and represent IEEE-754 64-bit floating-point numbers.. They can be declared and initialized later, or initialized inline. 

```
float i        // null
float j = 3.14 // 3.14


----------


```

#### Strings

Strings in RAPID are mutable, and declared with the `string` type, and have the default value of the empty string.  String literals are declared using double quotes, and special characters may be escaped using the `\` character.  Strings may be indexed using square brackets.  Because there is no Character type in RAPID, single characters are strings of length 1. Multiline strings may be declared using triple double quotes.  Newlines are preserved and quotes do not need to be escaped, but they may not be nested. Strings are pass by value.

```
string s                      // null
string character = "c"        // c
string s = "He is \"Batman\"" // He called himself "Batman"
string c = s[0]               // H
string multi = """
Did you hear?

He calls himself "Batman".
"""                           // multi[0] => "\n"
```

### 2.3 Non-Primitive Types

#### List

The `list` type is a zero-indexed array that expands to fit it's contents.  The type of the contents must be provided within angle brackets in the type signature.  RAPID `list` literals may be declared using square brackets, and values may be accessed or set using square brackets. Uninitialized lists default to the empty list. Lists are pass by reference.

```
/* List declaration */

list< /* type */ > /* id */ = [
	/* expression */,
	/* expression */,
	...
	/* expression */
]
```

```
list<int> empty // []
list<int> numbers = [1,2,3,42]
numbers[3]      // 42
numbers[1] = 5  // [1,5,3,42]
```

#### Dictionary

The `dict` type is similar to an object, but it's key set is mutable.  The type of the key and value must be provided within angle brackets in the type signature.  Only primitive types may be used as keys. Keys may be added, set, and accessed using square brackets. RAPID `dict` literals may be declared as comma-separated `key:value` pairs surrounded by braces.  Uninitialized dictionaries default to the empty dictionary. Dictionaries are pass by reference.

```
/* Dictionary declaration */

dict< /* type */ , /* type */ > /* id */ = {
    /* expression:string */ : /* expression */,
    /* expression:string */ : /* expression */,
    ...
    /* expression:string */ : /* expression */
}
```

```
dict<string, int> empty // {}
dict<string, int> ages = {"Alice":3, "Bob":5}
ages["Alice"]           // 3
ages["Bob"] = 4         // {"Alice":3, "Bob":4}
ages["Caroline"] = 7    // {"Alice":3, "Bob":4, "Caroline":7}
```

#### Object

The `object` type is a generic, non-primitive, dictionary-backed type that has attributes for instance variables and functions.  Accessing instance variables or functions can be done with dot notation.  Objects may not be declared anonymously; they must be declared as instances of classes.  Objects have immutable key sets, so variables and functions may not be added or removed, although their values may be changed.

If a class has been defined, it may be constructed using the class name followed by parenthesis, in which are comma-separated 

#### JSON Object TODO: JSON or json, all other types / keywords are lowercase.

JSON object is very similar to an Object, but instances do not represent rows in the database.  The keys and types of values must conform to the instance variables of the class on which it is based.  Each class defines an Object type and a JSON object type, and only JSON objects that are associated with classes may be instantiated.

For example, if we previously defined a `User` object with three instance variables `username`, `full_name`, and `password` (all strings), then we may declare a JSON User like so:

```
/* JSON Object initialization */

JSON /* id:classname */ /* id */ = {
	/* expression:string */ : /* expression */,
	/* expression:string */ : /* expression */,
	...
	/* expression:string */ : /* expression */
}
```

```
JSON User steve = {
    "username": "sedwards",
    "full_name": "Stephen Edwards",
    "password": "easypeasy"
}
```

#### Error TODO: Error or error? all other types are lowercase

Errors in RAPID are not thrown and caught, rather they are returned directly by unsafe functions (see Functions).  Errors contain a string message, which can be dot-accessed, an integer error code that conforms with the HTTP/1.1 standard, and an optional string name.

For example, to declare a custom error:

```
Error e = Error(name="RequestError", 
				code=400, 
				message="There was an error with that Request.")
```

Unsafe operations return an Error:

```
dict<string, int> d = {"foo": 4, "bar": 5}
int val, Error e = d["baz"]
if (!e?) {
    printf("%s\n", e.message) // Key error when accessing "baz" on `d`.
    printf("%d\n", e.code)    // 500
    printf("%s\n", e.name)    // KeyError
}
```

Many standard library classes and builtin objects define errors pertinent to their functions, to which an Error instance may be compared.

```
dict<string, int> d = {"foo": 4, "bar": 5}
int val, Error e = d["baz"]
if (!e?) {
    printf("%s", e == dict.KeyError) // true
}
```

##### Stacking

Unsafe functions (like list and dictionary access) may exist in the same expression.  If unsafe functions return successfully, the Error that is returned is consumed (ignored), and the return value is taken.  If an unsafe function returns an error, the expression evaluation short-circuits, and the value of the expression is null and the Error that is returned by the failed function call.

```
dict<string, list<int>> d = {"foo": [4,5], "bar": [1,2,3]}
int val, Error e = d["foo"][2]     // List index out of bounds...
printf("%d", val)                  // null
printf("%s", e.name)               // IndexError
printf("%t", e == list.IndexError) // true

val, e = d["baz"][0]             // No such key, short circuit
printf("%d", val)                // null
printf("%s", e.name)             // KeyError
printf("%t", e == dict.KeyError) // true
```

##### Anonymous Errors

Unsafe functions may also choose to return an *anonymous error*, which is an integer literal that will be cast to a generic Error object at compile time. See Functions for more details.

##### Status Code Definitions

Code | Message / Name 
-----|----------------
100  | Continue
200  | OK
201  | Created
301  | Moved Permanently
302  | Found
304  | Not Modified
400  | Bad Request
401  | Unauthorized
403  | Forbidden 
404  | Not Found
405  | Method Not Allowed
410  | Gone
413  | Request Entity Too Large
414  | Request-URI Too Long
417  | Expectation Failed
500  | Internal Server Error
501  | Not Implemented
502  | Bad Gateway
503  | Service Unavailable
504  | Gateway Timeout

### 2.4 Casting

Casting between float and int can be done using the `float()` and `int()` keyword functions. Floats are floored when they are cast to int.

```
float f = 7.5
int i = 3
float f = float(i) // f == 3.0
int i = int(f)     // i == 7
```

#### Functions

Functions are first class objects, and may be passed around as variables (see Functions)

## 3. Lexical Conventions

### 3.1 Identifiers

Identifiers must start with a letter or an underscore, followed by any combination of letters, numbers, and underscores.

> **Valid Identifiers:**
> 
> `abc`, `abc_def`, `a___1`, `__a__`, `_1`, `ABC`

> **Invalid Identifiers:**
> 
> `123`, `abc-def`, `1abc`, `ab\ cd`

### 3.2 Keywords

The following identifiers are keywords in RAPID, and are reserved. They can not be used for any other purpose.

`if`, `else`, `for`, `in`, `while`, `switch`, `case`, `default`, `fallthrough`, `http`, `func`, `JSON`, `class`, `namespace`, `param`, `true`, `false`, `new`, `optional`, `unsafe`

### 3.3 Literals

#### Integer literals

Integer literals may be declared using digits.

```
int x = 5
```

#### Float literals

Float literals are declared as an integer part, a decimal point, and a fraction part, all of which are mandatory.  The integer part may not start with a zero, unless it is only a zero (for floats less than `1.0`), in which case it is still required.  There may not be any whitespace between these three parts.

```
// Valid float literals:
float x = 15.0
float y = 0.25

// Invalid float literals:
float z = .5
float w = 10.
float v = 1 . 4
```

#### String literals

String literals are declared using double quotes.  Special characters may be declared using the `\` escape character.

```
string a = "hello"
string b = " \"world\"\n"
```

#### Boolean literals

Boolean literals are declared using the `true` and `false` keywords.  

```
boolean t = true
boolean f = false
```

#### List literals

List literals may be declared between square brackets, with comma-separated values. 

```
list<int> a = [1,2,3,4]
list<string> b = ["hello", "world"]
```

#### Dictionary literals

Dictionary literals may be declared as comma-separated key value pairs between braces, with a colon separating the key and value.  Whitespace is ignored.

```
dict<string, int> a = {"hello": 4, "world": 5}
dict<string, ing> b = {
    "a": 42,
    "b": 27
}
```

### 3.4 Comments

There are two types of comments in RAPID: single line comments, and block comments.  Single line comments are preceded by `//` and block comments begin with `/*` and end with `*/`

```
// This is a single line comment

/*
This is a multi-line
comment
/* They may be nested */
*/
```

### 3.5 Operators

Operator | Use | Associativity
:-------:|-----|-----------------
`+` | Addition | left
`*` | Multiplication | left
`/` | Division | left
`-` | Subtraction | left
`%` | Modulus | left
`=`  | Assignment | non-associative
`==` | Equal to | non-associative
`!=` | Not equal to | non-associative
`>` | Greater than | non-associative
`<` | Less than | non-associative
`>=` | Greater than or equal to | non-associative
`<=` | Less than or equal to | non-associative

## 4. Database Backing

### 4.1 Classes

RAPID classes are backed by a PostgreSQL database. Classes are defined using the `class` keyword, and represent an SQL table.  Instance variables (variables declared directly within the `class` block) represent columns for the table.  Instances of a class represent rows in SQL.  By default, columns are not nullable, but this may be overwritten using the `optional` keyword.  If the assignment syntax is used, a default value will be given.

```
class /* id */ {
	/* declaration */
	/* declaration */
	...
	/* declaration */
}
```

Take the following example:

```
class User {
    string username
    optional string full_name
    int age = 18
    string password
}
```

In this example, the "User" table has four columns: `username`, `full_name`, `age`, and `password`.  The `full_name` column may be omitted in the instantiation, and if `age` is omitted, it will take the value `18`.

#### Instantiation

New instances of a class may be declared using the `new` keyword.  The `new` keyword is followed by the name of the class and a pair of parenthesis, in which a JSON User literal (described more in-depth in the next section) may be passed to declare instance variables.

```
User bob = new User({
    "username": "burgerbob",
    "full_name": "Bob Belcher",
    "password": "burgersrock",    
    "age": 42
})
```

#### JSON

Defining the "User" class defines a `User` type, as well as a `JSON User` type.  The `JSON User` type has the same keys and value types as the User class, and may be declared in dictionary literal syntax.

``` 
JSON User bob_json = {
    "username": "burgerbob",
    "full_name": "Bob Belcher",
    "password": "burgersrock",    
    "age": 42
}
```

This JSON User does not represent a row in the database, and will be deallocated when it leaves scope.

It may be passed into an instantiation statement for a User object, to be persisted:

```
User bob = new User(bob_json)
```

A JSON User literal may also be passed in to create a User instances (as seen in the previous section).

### 4.2 Querying TODO complete, define some parameters, filters, etc.

Objects may be queried from the database using the `get` function, which is automatically defined on all classes.   

The following example queries all User objects from the database:

```
Tweet[] tweets = Tweet.get()
```

## 5. Functions

### 5.1 Declaration TODO: Default return type of null?

Functions in RAPID are first-class objects, but may not be declared anonymously.  Functions are declared using the `func` keyword.  The arguments (within parenthesis), return type (after the parenthesis, but before the braces), and the body of the function (within the braces) must be declared explicitly.  Return types may include multiple types separated by commas, or may be omitted (for a default value of `null`). 

Return values are specified using the `return` keyword.  If it is followed by an expression, the expression is returned.  If not, `null` is returned. If the `return` keyword is omitted, the function also returns `null`

```
return /* expression */
```   

The arguments must be in order `namespace` arguments, then formal arguments.

```
[unsafe] func /* id */ ( /* namespace args */ /* formal args */ ) {
	// statements
}
```

For example:

```
func sum(int a, int b) int {
    return a + b
}
```

### 5.2 Unsafe Functions

If a function performs actions that may be unsafe, it must be preceded by the keyword `unsafe`.   Unsafe functions return unsafe expressions, which is denoted by the presence of an `Error`-typed second value that is returned.

```
unsafe func access(dict<string, int> d, string key) int {
	int val, Error error = d[key]
	return val, error
}
```
Notice that the return type remains `int`, although an error is also returned. For more on unsafe expressions, see Expressions.

Unsafe functions may also return *anonymous errors*, which are integer literals that will be cast to a generic Error object at compile time.  See Status Code Definitions for a complete list of error codes that may be declared as anonymous errors.

```
/* Default dict accessing:
 *    If there is a KeyError, return 0 with a 400 error
 */
unsafe func access(dict<string, int> d, string key) int {
	int val, Error error = d[key]
	if (error == dict.KeyError) {
		return 0, 400
	}
	return val, 200
}
```

## 6. Routing

One of the core features of RAPID is it's ability to easily define routes for a REST API server.  

### 6.1 Declaring Routes

Routes may be declared like functions, but substituting the `http` keyword for the `func` keyword.  Routes specify a REST API endpoint, it's parameters, it's response, and any side effects. 

Like functions, routes take namespace arguments, and then other formal arguments.  Unlike functions, however, routes may also take a single request body argument that of a JSON object type.  It will be read from the request body and interpreted as JSON.

```
http /* id */ ( /* namespace args */ /* formal args */ /* request body args */) {
	// statements
}
```

Routes are unsafe by default, and therefore must include `Error` in their return types.  This may be an anonymous error (see Functions).

For example, the following route echos the URL parameter that it is passed.

```
http echo(string foo) string, Error {
	return foo, 200                
}
```

The name of the function will be the name of the route. Therefore, in the preceding example,  a `GET` request to `/echo?foo=Dog` will return `"Dog"`.

### 6.2 Path context

The endpoint associated with each route is determined by the combination of one or more blocks associated with it and the name of the route itself.  There is a one-to-one mapping from any route to a series of accessors on class instances.

#### Classes

Classes provide path context.  Class names are put to lowercase, and appended to path context.  The following example defines a route named `add` inside a class called `Math`.  

```
class Math {
	http add(int a, int b) int {
		return a + b, 200
	}
}
```

A `GET` request to `/math/add?a=3&b=4` will return `7`.

Similarly, the following code will print `7`:

```
math = Math()
int sum, Error _ = math.add(3,4)
printf("%d", sum)
```

#### Namespaces

Sometimes, functions or routes should be grouped together for organization purposes, rather than any functional purpose.  The `namespace` keyword defines a named block of functions that has the namespace name appended to the path context for those functions.

```
/* Namespace declaration */

namespace /* id */ {
	// statements
}
```

```
class Math {
	namespace ops {
		http add(int a, int b) int { return a + b, 200 }
		http sub(int a, int b) int { return a - b, 200 }	
	}
	namespace convert {
		func ft_to_in(float feet) float { return feet*12, 200 }	
	}
}
```

This defines routes at `/math/ops/add` and `/math/ops/sub`, and functions at `Math.ops.add`, `Math.ops.sub`, and `Math.convert.ft_to_in`.

A `GET` request to `/math/ops/add?a=3&b=4` will return `7`.

#### Parameters

Variable URL parameters may be defined similar to namespaces, using a named block with the `param` keyword.  The `param` keyword is followed by a type and an identifier.  

Any function or route defined within a `param` block must take the parameters defined by the `param` blocks in order from inside to out.

```
param /* type */ /* id */ {
	// statements
}
```

For example:
```
class Math {
	param int a {
		param int b {
			http add(int a, int b) int { return a + b, 200 }
		}	
		http square(int a) int { return a*a, 200 }
	}
}
```

A `GET` request to `/math/5/7/add` will return `12`, and a `GET` request to `/math/5/square` will return `25`. A `GET` request to `/math/5/7/add?a=4` will return return a 400 HTTP error. The following code snipped will print `12` then `25`:

```
math = Math()
int sum, Error _ = math.add(5,7)
printf("%d", sum)
int sqr, Error _ = math.square(5)
printf("%d", sqr)
```

## 7. Syntax

### 7.1 Program Structure

A valid RAPID program is a series of valid statements.  If the program contains any `http` blocks, it will be interpreted as a restful web API, and will run a HTTP web server on `localhost:5000`.

### 7.2 Expressions

Expressions are series of operators and operands that may be evaluated to a value and type.  Any subexpressions are evaluated from left to right, and side effects of evaluations occur by the time the evaluation is complete.  Type checking on operations occur in compile time.

#### Constants

Constants may be string, integer, float, or boolean, dict, list, or JSON object literals.  See Lexical Conventions for more information.

#### Identifiers

Identifiers could be primitive types, lists, dictionaries, objects, JSON objects, functions, classes, or errors.  Identifiers can be modified, and reused throughout a program.

For example, in the following example, the variable `a` changes value three times.

```
class a {} // `a` is a class
func a() void {} // `a` is a function, the class no longer exists.
int a = 5 // `a` is an int, the function no longer exists.
```

Identifiers are tied to the scope that they are declared in.  The following example prints `3`, then `5`, then `3`:
```
int a = 3
if (true) {
	printf("%d", a) // `a` is from the parent scope.
	int a = 5
	printf("%d", a) // `a` is from the local scope.
}
printf("%d", a) // the `a` from within the block does not leave it
```

#### Binary Operators

Binary operators have two operands, one on the left side, and one on the right.

```
/* expression */ /* bin-op */ /* expression */
```

In the case of multiple consecutive binary operations without parenthesis, the association of the binary operator is followed (see Operators).

#### Parenthesized Expressions

Parenthesis may be used to alter the order of operand evaluation.

### 7.3 Statements

#### Assignments

Assignments have an `lvalue`, and an expression, separated by an equal sign.  Possible `lvalue`s include identifiers, accessors (either list, dict, or object), a declaration, or another assignment:

```
/* lvalue */ = /* expression */
```

Examples include:
```
a = b
int i = 7
j = square(i)
k = 5 * b
```

#### Declarations

A declaration may be the declaration of a variable, an assignment, or the declaration of a function, route, class, namespace, or param.

##### Variable Declaration

A variable declaration consists of a type and an id.

```
/* type */ /* id */
```

##### Function Declaration

The declaration of a function is a valid statement (see Functions). 

##### Route Declaration

The declaration of a class is a valid statement (see Routing).

##### Class Declaration

The declaration of a class is a valid statement (see Classes).

##### Namespace or Parameter Declaration

The declaration of a namespace or parameter is a valid statement (see Path Context).

#### Function call

A function call is an identifier of a declared function and a set of parenthesis containing the comma-separated arguments.  There may not be a space between the identifier and the open parenthesis.

```
my_func(4,5)
int x = add(2, 6, 7)
```

#### Control flow

##### If 

If the expression between the parenthesis of an `if` statement evaluates to `true`, then the statements within the body are executed.  Note that non-boolean values will not be cast to boolean, and will result in a compile-time error.

```
if (/* expression */) { /* statements */ }
```

##### If-else

An `if` statement may be immediately followed by an `else` statement, in which case the block of code within the braces after the `else` keyword will be executed if the `if`'s expression evaluates to `false`.

```
if (/* expression */) {
    // statements
}
else {
    // statements
}
```

##### Else-if

An `if` statement may be followed by an `else if` statement, in which case the the second `if` statement will be evaluated if and only if the first `if` statement evaluates to `false`. The body of the `else if` is executed if the second `if` statement is evaluated, and evaluates to `true`. An `else if` statement may be followed by another `else if` statement, or an `else` statement.

```
if (/* expression */) {
    // statements
}
else if (/* expression */) {
    // statements
}
...
else if (/* expression */ ) {
    // statements
}
else {
    // statements
}
```

##### Switch

A `switch` statement includes an expression, which is evaluated and then compared in order to a series of one or more `case` expressions.  If the expressions are equal, the body of the `case` statement that matches will be executed, and then the switch statement will short circuit.  The `fallthrough` keyword may be used to avoid this short circuit, continuing to compare the `switch` expression with subsequent `case` expressions.

The `default` statement may be included after all `case` statements, and will be executed if it is reached.  This can be thought of as a `case` whose expression always equals that of the `switch`.  Observe the syntax below:

```
switch (/* expression */) {
    case (/* expression */) {
        // statements
        fallthrough
    }
    case (/* expression */) {
        // statements
    }
    default {
        // statements
    }
}
```

#### While loops

While loops contain an expression and a body.  If the expression evaluates to `true`, the body will be executed.  Afterwards, the expression will be evaluated again, and the process repeats.  Like `if` statements, `while` statements must have expressions that evaluate to a boolean in order to compile.

```
while (/* expression */) {
    // statements
}
```

#### For loops

A `for` loop may be used to iterate over a `list`.  The syntax is:

```
for (/* type */ /* id */ in /* list expr */) {
    // statements
}
```

For example:

```
list<int> my_list = [1,2,3,4,5]
for (int num in my_list) {
    printf("%d ", num)
}
// 1 2 3 4 5
```

The `range()` function in the standard library may be used to generate lists of sequential integers.

```
for (int num in range(1,6)) {
    printf("%d ", num)
}
// 1 2 3 4 5
```

#### Return statements

A `return` statement may be used to exit a function, optionally passing the value of an expression as the return value of the function. 

```
return /* optional expression */
```

For example:

```
func int add(int x, int y) int {
    return x + y
}
printf("%d", add(3,4))
// 7
```

## 8. Built-in Functions TODO: complete

### 8.1 length() TODO overloading or common behavior?

```
func length(string s) int
func length(list<T> l) int
func length(dict<T,S> d) int
func length(JSON T j) int
```

Returns the length of the argument.  For strings, this is the number of characters in the string, for lists, this is the number of elements in the list.  For dictionaries, this is the number of keys, for JSON objects, this is the number of keys.

Examples:
```
length("hello")                                // 5
length([0,1,2,3])                              // 4
length({"a":0, "b":null, "c": False, "d": ""}) // 4
```

### 8.2 range()

```
func range(int stop) int[]
func range(int start, int stop[, int step=1]) int[]
```

Returns a list of integers `r` where `r[i] = start + step*i`  where `i>=0` and while `r[i] < stop`.  If start is omitted, it defaults to 0.  If step is omitted, it defaults to 1.

Step may be negative, in which case  `r[i] > stop`

Examples:
```
range(5)       // [1,2,3,4,5]
range(4,5)     // [4]
range(3,7,2)   // [3,5]
range(10,4,-2) // [10,8,6]
```

## 9. Standard Library TODO: complete

### 9.1 string

#### string.is_empty()

```
func is_empty() boolean
```

#### string.substring()

```
unsafe func substring(start, stop) string
```

#### string.&#95;&#95;get&#95;&#95;()

```
unsafe func __get__(int index) string
```

#### string.&#95;&#95;set&#95;&#95;()

```
func __set__(int index, string char)
```

#### string.&#95;&#95;iter&#95;&#95;() TODO Iterables?

```
func __iter__() iterator
```

#### string.&#95;&#95;slice&#95;&#95;()

```
unsafe func __slice__(start, stop[, step=1])
```

### 9.2 list

#### list.is_empty()

```
func is_empty() boolean
```

#### list.append()

```
func append(T elem) 
```

#### list.pop()

```
unsafe func pop() T
```

#### list.push()

```
func push(T elem) list<T>
```

#### list.concat()

```
func concat(list<T> l) list<T>
```

#### list.reverse()

```
func reverse() list<T>
```

#### list.copy()

```
func copy() list<T>
```

#### list.&#95;&#95;get&#95;&#95;()

```
unsafe func __get__(int index) T
```

#### list.&#95;&#95;set&#95;&#95;()

```
func __set__(int index, T elem) 
```

#### list.&#95;&#95;iter&#95;&#95;() TODO: Iterables?

```
func __iter__() iterable
```

#### list.&#95;&#95;slice&#95;&#95;()

```
func __slice__(int start, int stop[, int step]) list<T>
```

### 9.3 dict

#### dict.is_empty()

```
func is_empty() boolean
```

#### dict.has_key()

```
func has_key(T key) boolean
```

#### dict.insert()

```
func insert(T key, S value)
```

#### dict.remove()

```
unsafe func remove(T key)
```

#### dict.keys()

```
func keys() list<T>
```

#### dict.values()

```
func values() list<S>
```

#### dict.&#95;&#95;get&#95;&#95;()

```
unsafe func __get__(T key) S
```

#### dict.&#95;&#95;set&#95;&#95;()

```
func __set__(T key, S value)
```

#### dict.&#95;&#95;iter&#95;&#95;() TODO iterators?

```
func __iter__() iterator
```

### 9.4 Error

#### Error.message

```
string message
```

#### Error.code

```
int code
```

#### Error.name

```
string name
```

### 9.2 sys TODO: include?

## 10. Program Execution TODO: explain compilation / execution

RAPID programs compile to a Go executable.  If a RAPID program contains an `http` route, then running the executable will start a server on `localhost:5000`.  Otherwise, the statements will be executed in order.


<!--se_discussion_list:{"zQORJhlYbDMZtVnLBFF07Yf0":{"selectionStart":2232,"selectionEnd":2233,"commentList":[{"author":"Nate","content":"are they?  we never discussed immutable values"},{"author":"Dan","content":"idk I just wrote this. Are go strings mutable or immutable?"},{"author":"Nate","content":"clarifying it to that strings are pass by value"}],"discussionIndex":"zQORJhlYbDMZtVnLBFF07Yf0"},"6MbsJsjDcGY1cUhC7pz69VHo":{"selectionStart":3690,"selectionEnd":3690,"commentList":[{"author":"Nate","content":"What is the use case for declaring an object as JSON? I thought we were only intending to handle regular objects and then serialize them when they're returned to the user?"},{"author":"Dan","content":"You'd rarely do this, it's mostly so that you can create some fake users in the database without acutally posting JSON. Also, the important point here is answering the question of what actually is getting passed into the Tweet constructor in our example program?"}],"discussionIndex":"6MbsJsjDcGY1cUhC7pz69VHo"},"0Bjr02Cp5Exac4vTcBETcCf8":{"selectionStart":9102,"selectionEnd":8996,"commentList":[{"author":"Nate","content":"I don't think it makes sense to have a special case for errors w.r.t the equality operator to suddenly just use types"},{"author":"Dan","content":"If we do types, we'll need inheritance. Do we want to implement that?"}],"discussionIndex":"0Bjr02Cp5Exac4vTcBETcCf8"},"4J6VrHafL9OQqYpxIME1ohbC":{"selectionStart":11371,"selectionEnd":11396,"commentList":[{"author":"Nate","content":"Why should we allow nesting of comments?"},{"author":"Dan","content":"So that you can comment out blocks of code that include comments"}],"discussionIndex":"4J6VrHafL9OQqYpxIME1ohbC"},"XNfBpahmJ9OSKiHJAPeIT2ti":{"selectionStart":13382,"selectionEnd":13411,"commentList":[{"author":"Nate","content":"What is the defined behavior for \"optional\" attributes here as well as default values?"},{"author":"Nate","content":"if the age is missing, should it be omitted or should 18 be put in"},{"author":"Dan","content":"I think 18 should be put in.  Any problems with that?"}],"discussionIndex":"XNfBpahmJ9OSKiHJAPeIT2ti"},"cjb2t10o4cxpmXyE0z1wOatk":{"selectionStart":19535,"selectionEnd":19610,"commentList":[{"author":"Nate","content":"added this defined behavior, comment if you disagree"}],"discussionIndex":"cjb2t10o4cxpmXyE0z1wOatk"},"Uz4Cqbm2symqFX77UWn9RQEe":{"selectionStart":19792,"selectionEnd":19792,"commentList":[{"author":"Nate","content":"I think this is too much magic for this aspect. I know that we have DB & HTTP magic, but function parameters should not be implicit like this."}],"discussionIndex":"Uz4Cqbm2symqFX77UWn9RQEe"},"6A4TW9AVUcnIAfICkXBqZS4V":{"selectionStart":20659,"selectionEnd":20720,"commentList":[{"author":"Nate","content":"I think this needs to be tied to scope, please see below example of an edge case"},{"author":"Dan","content":"See my answer below."},{"author":"Dan","content":"We could also get rid of this behavior"}],"discussionIndex":"6A4TW9AVUcnIAfICkXBqZS4V"}}-->
