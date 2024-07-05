(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// VIRTUAL-DOM WIDGETS


var _Markdown_toHtml = F3(function(options, factList, rawMarkdown)
{
	return _VirtualDom_custom(
		factList,
		{
			a: options,
			b: rawMarkdown
		},
		_Markdown_render,
		_Markdown_diff
	);
});



// WIDGET IMPLEMENTATION


function _Markdown_render(model)
{
	return A2(_Markdown_replace, model, _VirtualDom_doc.createElement('div'));
}


function _Markdown_diff(x, y)
{
	return x.b === y.b && x.a === y.a
		? false
		: _Markdown_replace(y);
}


var _Markdown_replace = F2(function(model, div)
{
	div.innerHTML = _Markdown_marked(model.b, _Markdown_formatOptions(model.a));
	return div;
});



// ACTUAL MARKDOWN PARSER


var _Markdown_marked = function() {
	// catch the `marked` object regardless of the outer environment.
	// (ex. a CommonJS module compatible environment.)
	// note that this depends on marked's implementation of environment detection.
	var module = {};
	var exports = module.exports = {};

	/**
	 * marked - a markdown parser
	 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
	 * https://github.com/chjj/marked
	 * commit cd2f6f5b7091154c5526e79b5f3bfb4d15995a51
	 */
	(function(){var block={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:noop,hr:/^( *[-*_]){3,} *(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *#* *(?:\n+|$)/,nptable:noop,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,blockquote:/^( *>[^\n]+(\n(?!def)[^\n]+)*\n*)+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:/^ *(?:comment *(?:\n|\s*$)|closed *(?:\n{2,}|\s*$)|closing *(?:\n{2,}|\s*$))/,def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +["(]([^\n]+)[")])? *(?:\n+|$)/,table:noop,paragraph:/^((?:[^\n]+\n?(?!hr|heading|lheading|blockquote|tag|def))+)\n*/,text:/^[^\n]+/};block.bullet=/(?:[*+-]|\d+\.)/;block.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/;block.item=replace(block.item,"gm")(/bull/g,block.bullet)();block.list=replace(block.list)(/bull/g,block.bullet)("hr","\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))")("def","\\n+(?="+block.def.source+")")();block.blockquote=replace(block.blockquote)("def",block.def)();block._tag="(?!(?:"+"a|em|strong|small|s|cite|q|dfn|abbr|data|time|code"+"|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo"+"|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b";block.html=replace(block.html)("comment",/<!--[\s\S]*?-->/)("closed",/<(tag)[\s\S]+?<\/\1>/)("closing",/<tag(?:"[^"]*"|'[^']*'|[^'">])*?>/)(/tag/g,block._tag)();block.paragraph=replace(block.paragraph)("hr",block.hr)("heading",block.heading)("lheading",block.lheading)("blockquote",block.blockquote)("tag","<"+block._tag)("def",block.def)();block.normal=merge({},block);block.gfm=merge({},block.normal,{fences:/^ *(`{3,}|~{3,})[ \.]*(\S+)? *\n([\s\S]*?)\s*\1 *(?:\n+|$)/,paragraph:/^/,heading:/^ *(#{1,6}) +([^\n]+?) *#* *(?:\n+|$)/});block.gfm.paragraph=replace(block.paragraph)("(?!","(?!"+block.gfm.fences.source.replace("\\1","\\2")+"|"+block.list.source.replace("\\1","\\3")+"|")();block.tables=merge({},block.gfm,{nptable:/^ *(\S.*\|.*)\n *([-:]+ *\|[-| :]*)\n((?:.*\|.*(?:\n|$))*)\n*/,table:/^ *\|(.+)\n *\|( *[-:]+[-| :]*)\n((?: *\|.*(?:\n|$))*)\n*/});function Lexer(options){this.tokens=[];this.tokens.links={};this.options=options||marked.defaults;this.rules=block.normal;if(this.options.gfm){if(this.options.tables){this.rules=block.tables}else{this.rules=block.gfm}}}Lexer.rules=block;Lexer.lex=function(src,options){var lexer=new Lexer(options);return lexer.lex(src)};Lexer.prototype.lex=function(src){src=src.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n");return this.token(src,true)};Lexer.prototype.token=function(src,top,bq){var src=src.replace(/^ +$/gm,""),next,loose,cap,bull,b,item,space,i,l;while(src){if(cap=this.rules.newline.exec(src)){src=src.substring(cap[0].length);if(cap[0].length>1){this.tokens.push({type:"space"})}}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);cap=cap[0].replace(/^ {4}/gm,"");this.tokens.push({type:"code",text:!this.options.pedantic?cap.replace(/\n+$/,""):cap});continue}if(cap=this.rules.fences.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"code",lang:cap[2],text:cap[3]||""});continue}if(cap=this.rules.heading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[1].length,text:cap[2]});continue}if(top&&(cap=this.rules.nptable.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].split(/ *\| */)}this.tokens.push(item);continue}if(cap=this.rules.lheading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[2]==="="?1:2,text:cap[1]});continue}if(cap=this.rules.hr.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"hr"});continue}if(cap=this.rules.blockquote.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"blockquote_start"});cap=cap[0].replace(/^ *> ?/gm,"");this.token(cap,top,true);this.tokens.push({type:"blockquote_end"});continue}if(cap=this.rules.list.exec(src)){src=src.substring(cap[0].length);bull=cap[2];this.tokens.push({type:"list_start",ordered:bull.length>1});cap=cap[0].match(this.rules.item);next=false;l=cap.length;i=0;for(;i<l;i++){item=cap[i];space=item.length;item=item.replace(/^ *([*+-]|\d+\.) +/,"");if(~item.indexOf("\n ")){space-=item.length;item=!this.options.pedantic?item.replace(new RegExp("^ {1,"+space+"}","gm"),""):item.replace(/^ {1,4}/gm,"")}if(this.options.smartLists&&i!==l-1){b=block.bullet.exec(cap[i+1])[0];if(bull!==b&&!(bull.length>1&&b.length>1)){src=cap.slice(i+1).join("\n")+src;i=l-1}}loose=next||/\n\n(?!\s*$)/.test(item);if(i!==l-1){next=item.charAt(item.length-1)==="\n";if(!loose)loose=next}this.tokens.push({type:loose?"loose_item_start":"list_item_start"});this.token(item,false,bq);this.tokens.push({type:"list_item_end"})}this.tokens.push({type:"list_end"});continue}if(cap=this.rules.html.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:!this.options.sanitizer&&(cap[1]==="pre"||cap[1]==="script"||cap[1]==="style"),text:cap[0]});continue}if(!bq&&top&&(cap=this.rules.def.exec(src))){src=src.substring(cap[0].length);this.tokens.links[cap[1].toLowerCase()]={href:cap[2],title:cap[3]};continue}if(top&&(cap=this.rules.table.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/(?: *\| *)?\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].replace(/^ *\| *| *\| *$/g,"").split(/ *\| */)}this.tokens.push(item);continue}if(top&&(cap=this.rules.paragraph.exec(src))){src=src.substring(cap[0].length);this.tokens.push({type:"paragraph",text:cap[1].charAt(cap[1].length-1)==="\n"?cap[1].slice(0,-1):cap[1]});continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"text",text:cap[0]});continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return this.tokens};var inline={escape:/^\\([\\`*{}\[\]()#+\-.!_>])/,autolink:/^<([^ >]+(@|:\/)[^ >]+)>/,url:noop,tag:/^<!--[\s\S]*?-->|^<\/?\w+(?:"[^"]*"|'[^']*'|[^'">])*?>/,link:/^!?\[(inside)\]\(href\)/,reflink:/^!?\[(inside)\]\s*\[([^\]]*)\]/,nolink:/^!?\[((?:\[[^\]]*\]|[^\[\]])*)\]/,strong:/^_\_([\s\S]+?)_\_(?!_)|^\*\*([\s\S]+?)\*\*(?!\*)/,em:/^\b_((?:[^_]|_\_)+?)_\b|^\*((?:\*\*|[\s\S])+?)\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`])\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:noop,text:/^[\s\S]+?(?=[\\<!\[_*`]| {2,}\n|$)/};inline._inside=/(?:\[[^\]]*\]|[^\[\]]|\](?=[^\[]*\]))*/;inline._href=/\s*<?([\s\S]*?)>?(?:\s+['"]([\s\S]*?)['"])?\s*/;inline.link=replace(inline.link)("inside",inline._inside)("href",inline._href)();inline.reflink=replace(inline.reflink)("inside",inline._inside)();inline.normal=merge({},inline);inline.pedantic=merge({},inline.normal,{strong:/^_\_(?=\S)([\s\S]*?\S)_\_(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/});inline.gfm=merge({},inline.normal,{escape:replace(inline.escape)("])","~|])")(),url:/^(https?:\/\/[^\s<]+[^<.,:;"')\]\s])/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:replace(inline.text)("]|","~]|")("|","|https?://|")()});inline.breaks=merge({},inline.gfm,{br:replace(inline.br)("{2,}","*")(),text:replace(inline.gfm.text)("{2,}","*")()});function InlineLexer(links,options){this.options=options||marked.defaults;this.links=links;this.rules=inline.normal;this.renderer=this.options.renderer||new Renderer;this.renderer.options=this.options;if(!this.links){throw new Error("Tokens array requires a `links` property.")}if(this.options.gfm){if(this.options.breaks){this.rules=inline.breaks}else{this.rules=inline.gfm}}else if(this.options.pedantic){this.rules=inline.pedantic}}InlineLexer.rules=inline;InlineLexer.output=function(src,links,options){var inline=new InlineLexer(links,options);return inline.output(src)};InlineLexer.prototype.output=function(src){var out="",link,text,href,cap;while(src){if(cap=this.rules.escape.exec(src)){src=src.substring(cap[0].length);out+=cap[1];continue}if(cap=this.rules.autolink.exec(src)){src=src.substring(cap[0].length);if(cap[2]==="@"){text=cap[1].charAt(6)===":"?this.mangle(cap[1].substring(7)):this.mangle(cap[1]);href=this.mangle("mailto:")+text}else{text=escape(cap[1]);href=text}out+=this.renderer.link(href,null,text);continue}if(!this.inLink&&(cap=this.rules.url.exec(src))){src=src.substring(cap[0].length);text=escape(cap[1]);href=text;out+=this.renderer.link(href,null,text);continue}if(cap=this.rules.tag.exec(src)){if(!this.inLink&&/^<a /i.test(cap[0])){this.inLink=true}else if(this.inLink&&/^<\/a>/i.test(cap[0])){this.inLink=false}src=src.substring(cap[0].length);out+=this.options.sanitize?this.options.sanitizer?this.options.sanitizer(cap[0]):escape(cap[0]):cap[0];continue}if(cap=this.rules.link.exec(src)){src=src.substring(cap[0].length);this.inLink=true;out+=this.outputLink(cap,{href:cap[2],title:cap[3]});this.inLink=false;continue}if((cap=this.rules.reflink.exec(src))||(cap=this.rules.nolink.exec(src))){src=src.substring(cap[0].length);link=(cap[2]||cap[1]).replace(/\s+/g," ");link=this.links[link.toLowerCase()];if(!link||!link.href){out+=cap[0].charAt(0);src=cap[0].substring(1)+src;continue}this.inLink=true;out+=this.outputLink(cap,link);this.inLink=false;continue}if(cap=this.rules.strong.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.strong(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.em.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.em(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.codespan(escape(cap[2],true));continue}if(cap=this.rules.br.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.br();continue}if(cap=this.rules.del.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.del(this.output(cap[1]));continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.text(escape(this.smartypants(cap[0])));continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return out};InlineLexer.prototype.outputLink=function(cap,link){var href=escape(link.href),title=link.title?escape(link.title):null;return cap[0].charAt(0)!=="!"?this.renderer.link(href,title,this.output(cap[1])):this.renderer.image(href,title,escape(cap[1]))};InlineLexer.prototype.smartypants=function(text){if(!this.options.smartypants)return text;return text.replace(/---/g,"—").replace(/--/g,"–").replace(/(^|[-\u2014\/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014\/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…")};InlineLexer.prototype.mangle=function(text){if(!this.options.mangle)return text;var out="",l=text.length,i=0,ch;for(;i<l;i++){ch=text.charCodeAt(i);if(Math.random()>.5){ch="x"+ch.toString(16)}out+="&#"+ch+";"}return out};function Renderer(options){this.options=options||{}}Renderer.prototype.code=function(code,lang,escaped){if(this.options.highlight){var out=this.options.highlight(code,lang);if(out!=null&&out!==code){escaped=true;code=out}}if(!lang){return"<pre><code>"+(escaped?code:escape(code,true))+"\n</code></pre>"}return'<pre><code class="'+this.options.langPrefix+escape(lang,true)+'">'+(escaped?code:escape(code,true))+"\n</code></pre>\n"};Renderer.prototype.blockquote=function(quote){return"<blockquote>\n"+quote+"</blockquote>\n"};Renderer.prototype.html=function(html){return html};Renderer.prototype.heading=function(text,level,raw){return"<h"+level+' id="'+this.options.headerPrefix+raw.toLowerCase().replace(/[^\w]+/g,"-")+'">'+text+"</h"+level+">\n"};Renderer.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"};Renderer.prototype.list=function(body,ordered){var type=ordered?"ol":"ul";return"<"+type+">\n"+body+"</"+type+">\n"};Renderer.prototype.listitem=function(text){return"<li>"+text+"</li>\n"};Renderer.prototype.paragraph=function(text){return"<p>"+text+"</p>\n"};Renderer.prototype.table=function(header,body){return"<table>\n"+"<thead>\n"+header+"</thead>\n"+"<tbody>\n"+body+"</tbody>\n"+"</table>\n"};Renderer.prototype.tablerow=function(content){return"<tr>\n"+content+"</tr>\n"};Renderer.prototype.tablecell=function(content,flags){var type=flags.header?"th":"td";var tag=flags.align?"<"+type+' style="text-align:'+flags.align+'">':"<"+type+">";return tag+content+"</"+type+">\n"};Renderer.prototype.strong=function(text){return"<strong>"+text+"</strong>"};Renderer.prototype.em=function(text){return"<em>"+text+"</em>"};Renderer.prototype.codespan=function(text){return"<code>"+text+"</code>"};Renderer.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"};Renderer.prototype.del=function(text){return"<del>"+text+"</del>"};Renderer.prototype.link=function(href,title,text){if(this.options.sanitize){try{var prot=decodeURIComponent(unescape(href)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return""}if(prot.indexOf("javascript:")===0||prot.indexOf("vbscript:")===0||prot.indexOf("data:")===0){return""}}var out='<a href="'+href+'"';if(title){out+=' title="'+title+'"'}out+=">"+text+"</a>";return out};Renderer.prototype.image=function(href,title,text){var out='<img src="'+href+'" alt="'+text+'"';if(title){out+=' title="'+title+'"'}out+=this.options.xhtml?"/>":">";return out};Renderer.prototype.text=function(text){return text};function Parser(options){this.tokens=[];this.token=null;this.options=options||marked.defaults;this.options.renderer=this.options.renderer||new Renderer;this.renderer=this.options.renderer;this.renderer.options=this.options}Parser.parse=function(src,options,renderer){var parser=new Parser(options,renderer);return parser.parse(src)};Parser.prototype.parse=function(src){this.inline=new InlineLexer(src.links,this.options,this.renderer);this.tokens=src.reverse();var out="";while(this.next()){out+=this.tok()}return out};Parser.prototype.next=function(){return this.token=this.tokens.pop()};Parser.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0};Parser.prototype.parseText=function(){var body=this.token.text;while(this.peek().type==="text"){body+="\n"+this.next().text}return this.inline.output(body)};Parser.prototype.tok=function(){switch(this.token.type){case"space":{return""}case"hr":{return this.renderer.hr()}case"heading":{return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,this.token.text)}case"code":{return this.renderer.code(this.token.text,this.token.lang,this.token.escaped)}case"table":{var header="",body="",i,row,cell,flags,j;cell="";for(i=0;i<this.token.header.length;i++){flags={header:true,align:this.token.align[i]};cell+=this.renderer.tablecell(this.inline.output(this.token.header[i]),{header:true,align:this.token.align[i]})}header+=this.renderer.tablerow(cell);for(i=0;i<this.token.cells.length;i++){row=this.token.cells[i];cell="";for(j=0;j<row.length;j++){cell+=this.renderer.tablecell(this.inline.output(row[j]),{header:false,align:this.token.align[j]})}body+=this.renderer.tablerow(cell)}return this.renderer.table(header,body)}case"blockquote_start":{var body="";while(this.next().type!=="blockquote_end"){body+=this.tok()}return this.renderer.blockquote(body)}case"list_start":{var body="",ordered=this.token.ordered;while(this.next().type!=="list_end"){body+=this.tok()}return this.renderer.list(body,ordered)}case"list_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.token.type==="text"?this.parseText():this.tok()}return this.renderer.listitem(body)}case"loose_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.tok()}return this.renderer.listitem(body)}case"html":{var html=!this.token.pre&&!this.options.pedantic?this.inline.output(this.token.text):this.token.text;return this.renderer.html(html)}case"paragraph":{return this.renderer.paragraph(this.inline.output(this.token.text))}case"text":{return this.renderer.paragraph(this.parseText())}}};function escape(html,encode){return html.replace(!encode?/&(?!#?\w+;)/g:/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function unescape(html){return html.replace(/&(#(?:\d+)|(?:#x[0-9A-Fa-f]+)|(?:\w+));?/g,function(_,n){n=n.toLowerCase();if(n==="colon")return":";if(n.charAt(0)==="#"){return n.charAt(1)==="x"?String.fromCharCode(parseInt(n.substring(2),16)):String.fromCharCode(+n.substring(1))}return""})}function replace(regex,opt){regex=regex.source;opt=opt||"";return function self(name,val){if(!name)return new RegExp(regex,opt);val=val.source||val;val=val.replace(/(^|[^\[])\^/g,"$1");regex=regex.replace(name,val);return self}}function noop(){}noop.exec=noop;function merge(obj){var i=1,target,key;for(;i<arguments.length;i++){target=arguments[i];for(key in target){if(Object.prototype.hasOwnProperty.call(target,key)){obj[key]=target[key]}}}return obj}function marked(src,opt,callback){if(callback||typeof opt==="function"){if(!callback){callback=opt;opt=null}opt=merge({},marked.defaults,opt||{});var highlight=opt.highlight,tokens,pending,i=0;try{tokens=Lexer.lex(src,opt)}catch(e){return callback(e)}pending=tokens.length;var done=function(err){if(err){opt.highlight=highlight;return callback(err)}var out;try{out=Parser.parse(tokens,opt)}catch(e){err=e}opt.highlight=highlight;return err?callback(err):callback(null,out)};if(!highlight||highlight.length<3){return done()}delete opt.highlight;if(!pending)return done();for(;i<tokens.length;i++){(function(token){if(token.type!=="code"){return--pending||done()}return highlight(token.text,token.lang,function(err,code){if(err)return done(err);if(code==null||code===token.text){return--pending||done()}token.text=code;token.escaped=true;--pending||done()})})(tokens[i])}return}try{if(opt)opt=merge({},marked.defaults,opt);return Parser.parse(Lexer.lex(src,opt),opt)}catch(e){e.message+="\nPlease report this to https://github.com/chjj/marked.";if((opt||marked.defaults).silent){return"<p>An error occured:</p><pre>"+escape(e.message+"",true)+"</pre>"}throw e}}marked.options=marked.setOptions=function(opt){merge(marked.defaults,opt);return marked};marked.defaults={gfm:true,tables:true,breaks:false,pedantic:false,sanitize:false,sanitizer:null,mangle:true,smartLists:false,silent:false,highlight:null,langPrefix:"lang-",smartypants:false,headerPrefix:"",renderer:new Renderer,xhtml:false};marked.Parser=Parser;marked.parser=Parser.parse;marked.Renderer=Renderer;marked.Lexer=Lexer;marked.lexer=Lexer.lex;marked.InlineLexer=InlineLexer;marked.inlineLexer=InlineLexer.output;marked.parse=marked;if(typeof module!=="undefined"&&typeof exports==="object"){module.exports=marked}else if(typeof define==="function"&&define.amd){define(function(){return marked})}else{this.marked=marked}}).call(function(){return this||(typeof window!=="undefined"?window:global)}());

	return module.exports;
}();


// FORMAT OPTIONS FOR MARKED IMPLEMENTATION

function _Markdown_formatOptions(options)
{
	function toHighlight(code, lang)
	{
		if (!lang && $elm$core$Maybe$isJust(options.defaultHighlighting))
		{
			lang = options.defaultHighlighting.a;
		}

		if (typeof hljs !== 'undefined' && lang && hljs.listLanguages().indexOf(lang) >= 0)
		{
			return hljs.highlight(lang, code, true).value;
		}

		return code;
	}

	var gfm = options.githubFlavored.a;

	return {
		highlight: toHighlight,
		gfm: gfm,
		tables: gfm && gfm.tables,
		breaks: gfm && gfm.breaks,
		sanitize: options.sanitize,
		smartypants: options.smartypants
	};
}
var $author$project$Types$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var $author$project$Types$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Types$Loading = {$: 'Loading'};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Types$GotCharacterList = function (a) {
	return {$: 'GotCharacterList', a: a};
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{body: $elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Types$HttpResponse = function (a) {
	return {$: 'HttpResponse', a: a};
};
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $author$project$Types$mkHttpResponseMsg = F2(
	function (f, result) {
		return $author$project$Types$HttpResponse(
			A2($elm$core$Result$map, f, result));
	});
var $elm$url$Url$Builder$toQueryPair = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return key + ('=' + value);
};
var $elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			$elm$core$String$join,
			'&',
			A2($elm$core$List$map, $elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var $elm$url$Url$Builder$crossOrigin = F3(
	function (prePath, pathSegments, parameters) {
		return prePath + ('/' + (A2($elm$core$String$join, '/', pathSegments) + $elm$url$Url$Builder$toQuery(parameters)));
	});
var $elm$url$Url$Builder$QueryParameter = F2(
	function (a, b) {
		return {$: 'QueryParameter', a: a, b: b};
	});
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $elm$url$Url$Builder$string = F2(
	function (key, value) {
		return A2(
			$elm$url$Url$Builder$QueryParameter,
			$elm$url$Url$percentEncode(key),
			$elm$url$Url$percentEncode(value));
	});
var $author$project$Request$requestUrl = F2(
	function (req, params) {
		return A3(
			$elm$url$Url$Builder$crossOrigin,
			'http://localhost:8000',
			_List_fromArray(
				['request', req]),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var k = _v0.a;
					var v = _v0.b;
					return A2($elm$url$Url$Builder$string, k, v);
				},
				params));
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$loadSelectCharacterPage = $elm$http$Http$get(
	{
		expect: A2(
			$elm$http$Http$expectJson,
			$author$project$Types$mkHttpResponseMsg($author$project$Types$GotCharacterList),
			A2(
				$elm$json$Json$Decode$field,
				'list',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string))),
		url: A2($author$project$Request$requestUrl, 'list_characters', _List_Nil)
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $author$project$Main$init = F3(
	function (_v0, url, key) {
		return _Utils_Tuple2(
			{
				focusedDropdownId: $elm$core$Maybe$Nothing,
				key: key,
				lastTick: $elm$time$Time$millisToPosix(0),
				page: $author$project$Types$Loading,
				preparedSpells: $elm$core$Dict$empty,
				showOnlyPreparedSpells: false,
				url: url
			},
			$author$project$Main$loadSelectCharacterPage);
	});
var $author$project$Types$ClickOut = {$: 'ClickOut'};
var $author$project$Types$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onClick = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'click');
var $author$project$Main$subscriptions = function (_v0) {
	var focusedDropdownId = _v0.focusedDropdownId;
	var page = _v0.page;
	var timeSub = function () {
		if (page.$ === 'EditCharacterPage') {
			return A2($elm$time$Time$every, 500, $author$project$Types$Tick);
		} else {
			return $elm$core$Platform$Sub$none;
		}
	}();
	var clickoutSub = function () {
		if (focusedDropdownId.$ === 'Just') {
			return $elm$browser$Browser$Events$onClick(
				$elm$json$Json$Decode$succeed($author$project$Types$ClickOut));
		} else {
			return $elm$core$Platform$Sub$none;
		}
	}();
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[clickoutSub, timeSub]));
};
var $elm$core$Debug$log = _Debug_log;
var $author$project$Types$CardsPage = F2(
	function (a, b) {
		return {$: 'CardsPage', a: a, b: b};
	});
var $author$project$Types$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Types$applyPage = F2(
	function (model, _v0) {
		var page = _v0.a;
		var cmd = _v0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{page: page}),
			cmd);
	});
var $author$project$Types$CharacterSelectionPage = function (a) {
	return {$: 'CharacterSelectionPage', a: a};
};
var $author$project$Types$CharacterSheetPage = function (a) {
	return {$: 'CharacterSheetPage', a: a};
};
var $author$project$Types$EditCharacterPage = function (a) {
	return {$: 'EditCharacterPage', a: a};
};
var $author$project$Types$EquipmentPage = function (a) {
	return {$: 'EquipmentPage', a: a};
};
var $author$project$Types$PrintableCharSheetPage = function (a) {
	return {$: 'PrintableCharSheetPage', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Types$errorPage = F2(
	function (model, msg) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					page: $author$project$Types$Error(msg)
				}),
			$elm$core$Platform$Cmd$none);
	});
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $author$project$Types$initPreparedSpells = A2(
	$elm$core$Basics$composeL,
	$elm$core$Dict$fromList,
	$elm$core$List$map(
		function (section) {
			return _Utils_Tuple2(section.origin, $elm$core$Set$empty);
		}));
var $author$project$Types$GotCharacterOptions = F3(
	function (a, b, c) {
		return {$: 'GotCharacterOptions', a: a, b: b, c: c};
	});
var $author$project$Types$Ability$AbilityTableEntry = F6(
	function (base, totalBonus, score, mod, st, stProf) {
		return {base: base, mod: mod, score: score, st: st, stProf: stProf, totalBonus: totalBonus};
	});
var $elm_community$json_extra$Json$Decode$Extra$andMap = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Decoder$AbilityTable$abilityTableDec = $elm$json$Json$Decode$dict(
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'stProf', $elm$json$Json$Decode$bool),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'st', $elm$json$Json$Decode$int),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'mod', $elm$json$Json$Decode$int),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$int),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'total_bonus', $elm$json$Json$Decode$int),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2($elm$json$Json$Decode$field, 'base', $elm$json$Json$Decode$int),
							$elm$json$Json$Decode$succeed($author$project$Types$Ability$AbilityTableEntry))))))));
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Decoder$CharacterOptions$sequenceDecoders = function (decs) {
	if (!decs.b) {
		return $elm$json$Json$Decode$succeed(_List_Nil);
	} else {
		var d = decs.a;
		var ds = decs.b;
		return A2(
			$elm$json$Json$Decode$andThen,
			function (x) {
				return A2(
					$elm$json$Json$Decode$andThen,
					function (xs) {
						return $elm$json$Json$Decode$succeed(
							A2($elm$core$List$cons, x, xs));
					},
					$author$project$Decoder$CharacterOptions$sequenceDecoders(ds));
			},
			d);
	}
};
var $author$project$Decoder$CharacterOptions$intDictDec = function (valueDec) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (kvPairs) {
			return A2(
				$elm$json$Json$Decode$map,
				$elm$core$Dict$fromList,
				$author$project$Decoder$CharacterOptions$sequenceDecoders(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var key = _v0.a;
							var val = _v0.b;
							var _v1 = A2($elm$json$Json$Decode$decodeString, $elm$json$Json$Decode$int, key);
							if (_v1.$ === 'Ok') {
								var n = _v1.a;
								return $elm$json$Json$Decode$succeed(
									_Utils_Tuple2(n, val));
							} else {
								var err = _v1.a;
								return $elm$json$Json$Decode$fail(
									$elm$json$Json$Decode$errorToString(err));
							}
						},
						kvPairs)));
		},
		$elm$json$Json$Decode$keyValuePairs(valueDec));
};
var $author$project$Types$Options = F8(
	function (charlevel, id, display_id, origin, origin_category, display_origin_category, origin_category_index, spec) {
		return {charlevel: charlevel, display_id: display_id, display_origin_category: display_origin_category, id: id, origin: origin, origin_category: origin_category, origin_category_index: origin_category_index, spec: spec};
	});
var $author$project$Types$FromSC = F3(
	function (a, b, c) {
		return {$: 'FromSC', a: a, b: b, c: c};
	});
var $author$project$Types$ListSC = F2(
	function (a, b) {
		return {$: 'ListSC', a: a, b: b};
	});
var $author$project$Types$OrSC = F3(
	function (a, b, c) {
		return {$: 'OrSC', a: a, b: b, c: c};
	});
var $author$project$Types$L = {$: 'L'};
var $author$project$Types$R = {$: 'R'};
var $author$project$Util$decSet = function (val) {
	return $elm$json$Json$Decode$map(
		function (_v0) {
			return val;
		});
};
var $author$project$Util$matchStringDec = function (val) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (other) {
			return _Utils_eq(val, other) ? $elm$json$Json$Decode$succeed(val) : $elm$json$Json$Decode$fail('expected ' + val);
		},
		$elm$json$Json$Decode$string);
};
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $author$project$Decoder$CharacterOptions$dirDec = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$author$project$Util$decSet,
			$author$project$Types$L,
			$author$project$Util$matchStringDec('left')),
			A2(
			$author$project$Util$decSet,
			$author$project$Types$R,
			$author$project$Util$matchStringDec('right'))
		]));
var $elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		$elm$json$Json$Decode$andThen,
		thunk,
		$elm$json$Json$Decode$succeed(_Utils_Tuple0));
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $author$project$Decoder$CharacterOptions$addChoiceDec = function (spec) {
	switch (spec.$) {
		case 'ListSC':
			var options = spec.b;
			return A2(
				$elm$json$Json$Decode$map,
				function (choice) {
					return A2(
						$author$project$Types$ListSC,
						$elm$core$Maybe$Just(choice),
						options);
				},
				$elm$json$Json$Decode$string);
		case 'OrSC':
			var left = spec.b;
			var right = spec.c;
			return A2(
				$elm$json$Json$Decode$andThen,
				A2($author$project$Decoder$CharacterOptions$addOrChoiceDec, left, right),
				A2(
					$elm$json$Json$Decode$andThen,
					function (_v5) {
						return A2($elm$json$Json$Decode$field, 'side', $author$project$Decoder$CharacterOptions$dirDec);
					},
					A2(
						$elm$json$Json$Decode$field,
						'choicetype',
						$author$project$Util$matchStringDec('or'))));
		default:
			if (spec.c.b) {
				var unique = spec.a;
				var n = spec.b;
				var _v6 = spec.c;
				var subspec = _v6.a;
				return A2(
					$elm$json$Json$Decode$map,
					A2($author$project$Types$FromSC, unique, n),
					$elm$json$Json$Decode$lazy(
						function (_v7) {
							return A2(
								$elm$json$Json$Decode$map,
								function (specs) {
									return _Utils_ap(
										specs,
										A2(
											$elm$core$List$repeat,
											n - $elm$core$List$length(specs),
											subspec));
								},
								$elm$json$Json$Decode$list(
									$author$project$Decoder$CharacterOptions$addChoiceDec(subspec)));
						}));
			} else {
				return $elm$json$Json$Decode$fail('Page.EditCharacter.addChoiceDec: invalid match');
			}
	}
};
var $author$project$Decoder$CharacterOptions$addOrChoiceDec = F3(
	function (_v0, _v1, dir) {
		var lname = _v0.a;
		var lspec = _v0.b;
		var rname = _v1.a;
		var rspec = _v1.b;
		var subspec = function () {
			if (dir.$ === 'L') {
				return lspec;
			} else {
				return rspec;
			}
		}();
		var choiceDec = A2(
			$elm$json$Json$Decode$field,
			'choice',
			$author$project$Decoder$CharacterOptions$addChoiceDec(subspec));
		return A2(
			$elm$json$Json$Decode$map,
			function (newspec) {
				if (dir.$ === 'L') {
					return A3(
						$author$project$Types$OrSC,
						$elm$core$Maybe$Just(dir),
						_Utils_Tuple2(lname, newspec),
						_Utils_Tuple2(rname, rspec));
				} else {
					return A3(
						$author$project$Types$OrSC,
						$elm$core$Maybe$Just(dir),
						_Utils_Tuple2(lname, lspec),
						_Utils_Tuple2(rname, newspec));
				}
			},
			choiceDec);
	});
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Decoder$CharacterOptions$listSpecDec = A2(
	$elm$json$Json$Decode$field,
	'list',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Types$ListSC($elm$core$Maybe$Nothing),
		$elm$json$Json$Decode$list(
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2(
					$elm$json$Json$Decode$field,
					'desc',
					$elm$json$Json$Decode$oneOf(
						_List_fromArray(
							[
								$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
								A2($elm$json$Json$Decode$map, $elm$core$List$singleton, $elm$json$Json$Decode$string)
							]))),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'opt', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$succeed(
						F2(
							function (x, y) {
								return _Utils_Tuple2(x, y);
							})))))));
var $author$project$Decoder$CharacterOptions$fromSpecDec = function (unique) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (n) {
			return A2(
				$elm$json$Json$Decode$map,
				A2($author$project$Types$FromSC, unique, n),
				A2(
					$elm$json$Json$Decode$field,
					'spec',
					$elm$json$Json$Decode$lazy(
						function (_v3) {
							return A2(
								$elm$json$Json$Decode$map,
								$elm$core$List$repeat(n),
								$author$project$Decoder$CharacterOptions$cyclic$specDec());
						})));
		},
		A2($elm$json$Json$Decode$field, 'num', $elm$json$Json$Decode$int));
};
function $author$project$Decoder$CharacterOptions$cyclic$orSpecDec() {
	return A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2(
			$elm$json$Json$Decode$field,
			'right',
			$elm$json$Json$Decode$lazy(
				function (_v2) {
					return $author$project$Decoder$CharacterOptions$cyclic$specDec();
				})),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'rightname', $elm$json$Json$Decode$string),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2(
					$elm$json$Json$Decode$field,
					'left',
					$elm$json$Json$Decode$lazy(
						function (_v1) {
							return $author$project$Decoder$CharacterOptions$cyclic$specDec();
						})),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'leftname', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$succeed(
						F4(
							function (lName, lSpec, rName, rSpec) {
								return A3(
									$author$project$Types$OrSC,
									$elm$core$Maybe$Nothing,
									_Utils_Tuple2(lName, lSpec),
									_Utils_Tuple2(rName, rSpec));
							}))))));
}
function $author$project$Decoder$CharacterOptions$cyclic$specDec() {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (spectype) {
			switch (spectype) {
				case 'list':
					return $author$project$Decoder$CharacterOptions$listSpecDec;
				case 'or':
					return $author$project$Decoder$CharacterOptions$cyclic$orSpecDec();
				case 'from':
					return $author$project$Decoder$CharacterOptions$fromSpecDec(false);
				case 'unique_from':
					return $author$project$Decoder$CharacterOptions$fromSpecDec(true);
				default:
					return $elm$json$Json$Decode$fail('spectype should be \'list\', \'or\', \'from\', or \'unique_from\'');
			}
		},
		A2($elm$json$Json$Decode$field, 'spectype', $elm$json$Json$Decode$string));
}
try {
	var $author$project$Decoder$CharacterOptions$orSpecDec = $author$project$Decoder$CharacterOptions$cyclic$orSpecDec();
	$author$project$Decoder$CharacterOptions$cyclic$orSpecDec = function () {
		return $author$project$Decoder$CharacterOptions$orSpecDec;
	};
	var $author$project$Decoder$CharacterOptions$specDec = $author$project$Decoder$CharacterOptions$cyclic$specDec();
	$author$project$Decoder$CharacterOptions$cyclic$specDec = function () {
		return $author$project$Decoder$CharacterOptions$specDec;
	};
} catch ($) {
	throw 'Some top-level definitions from `Decoder.CharacterOptions` are causing infinite recursion:\n\n  ┌─────┐\n  │    fromSpecDec\n  │     ↓\n  │    orSpecDec\n  │     ↓\n  │    specDec\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $author$project$Decoder$CharacterOptions$extractSpecAndChoice = A2(
	$elm$json$Json$Decode$andThen,
	function (spec) {
		return A2(
			$elm$json$Json$Decode$field,
			'choice',
			$elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						$author$project$Decoder$CharacterOptions$addChoiceDec(spec),
						$elm$json$Json$Decode$null(spec)
					])));
	},
	A2($elm$json$Json$Decode$field, 'spec', $author$project$Decoder$CharacterOptions$specDec));
var $author$project$Decoder$CharacterOptions$optionsDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	$author$project$Decoder$CharacterOptions$extractSpecAndChoice,
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'origin_category_index', $elm$json$Json$Decode$int),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'display_origin_category', $elm$json$Json$Decode$string),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'origin_category', $elm$json$Json$Decode$string),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'origin', $elm$json$Json$Decode$string),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'display_id', $elm$json$Json$Decode$string),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
							A2(
								$elm_community$json_extra$Json$Decode$Extra$andMap,
								A2($elm$json$Json$Decode$field, 'charlevel', $elm$json$Json$Decode$int),
								$elm$json$Json$Decode$succeed($author$project$Types$Options)))))))));
var $author$project$Decoder$CharacterOptions$optionsDictDec = $author$project$Decoder$CharacterOptions$intDictDec(
	$elm$json$Json$Decode$list($author$project$Decoder$CharacterOptions$optionsDec));
var $author$project$Types$Effect = F3(
	function (effect, origin, desc) {
		return {desc: desc, effect: effect, origin: origin};
	});
var $author$project$Types$Atomic = function (a) {
	return {$: 'Atomic', a: a};
};
var $author$project$Types$Compound = F2(
	function (a, b) {
		return {$: 'Compound', a: a, b: b};
	});
function $author$project$Decoder$CharacterOptions$cyclic$prologTermDec() {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $author$project$Types$Atomic, $elm$json$Json$Decode$string),
				A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2(
					$elm$json$Json$Decode$field,
					'args',
					$elm$json$Json$Decode$list(
						$elm$json$Json$Decode$lazy(
							function (_v0) {
								return $author$project$Decoder$CharacterOptions$cyclic$prologTermDec();
							}))),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'functor', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$succeed($author$project$Types$Compound)))
			]));
}
try {
	var $author$project$Decoder$CharacterOptions$prologTermDec = $author$project$Decoder$CharacterOptions$cyclic$prologTermDec();
	$author$project$Decoder$CharacterOptions$cyclic$prologTermDec = function () {
		return $author$project$Decoder$CharacterOptions$prologTermDec;
	};
} catch ($) {
	throw 'Some top-level definitions from `Decoder.CharacterOptions` are causing infinite recursion:\n\n  ┌─────┐\n  │    prologTermDec\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $author$project$Decoder$CharacterOptions$traitOrBonusDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'desc',
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2($elm$json$Json$Decode$map, $elm$core$List$singleton, $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					$elm$json$Json$Decode$null(_List_Nil)
				]))),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'origin', $author$project$Decoder$CharacterOptions$prologTermDec),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'effect', $author$project$Decoder$CharacterOptions$prologTermDec),
			$elm$json$Json$Decode$succeed($author$project$Types$Effect))));
var $author$project$Decoder$CharacterOptions$traitsAndBonusesDictDec = $author$project$Decoder$CharacterOptions$intDictDec(
	$elm$json$Json$Decode$list($author$project$Decoder$CharacterOptions$traitOrBonusDec));
var $author$project$Decoder$CharacterOptions$gotCharacterOptionsDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2($elm$json$Json$Decode$field, 'traits_and_bonuses', $author$project$Decoder$CharacterOptions$traitsAndBonusesDictDec),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'options', $author$project$Decoder$CharacterOptions$optionsDictDec),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'ability_table', $author$project$Decoder$AbilityTable$abilityTableDec),
			$elm$json$Json$Decode$succeed(
				F3(
					function (x, y, z) {
						return _Utils_Tuple3(x, y, z);
					})))));
var $author$project$Page$EditCharacter$load = $elm$http$Http$get(
	{
		expect: A2(
			$elm$http$Http$expectJson,
			$author$project$Types$mkHttpResponseMsg(
				function (_v0) {
					var x = _v0.a;
					var y = _v0.b;
					var z = _v0.c;
					return A3($author$project$Types$GotCharacterOptions, x, y, z);
				}),
			$author$project$Decoder$CharacterOptions$gotCharacterOptionsDec),
		url: A2($author$project$Request$requestUrl, 'edit_character_page', _List_Nil)
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$core$Debug$toString = _Debug_toString;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$handleHttpResponseMsg = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'GotCharacterList':
				var chars = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							page: $author$project$Types$CharacterSelectionPage(
								{characters: chars, newCharacterName: ''})
						}),
					$elm$core$Platform$Cmd$none);
			case 'CharacterLoaded':
				var id = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{page: $author$project$Types$Loading}),
					A2($elm$browser$Browser$Navigation$pushUrl, model.key, '/edit'));
			case 'GotCharacterSheet':
				var sheet = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							page: $author$project$Types$CharacterSheetPage(sheet),
							preparedSpells: $author$project$Types$initPreparedSpells(sheet.spellcasting_sections)
						}),
					$elm$core$Platform$Cmd$none);
			case 'GotPrintableCharSheet':
				var sheet = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							page: $author$project$Types$PrintableCharSheetPage(sheet)
						}),
					$elm$core$Platform$Cmd$none);
			case 'GotCharacterOptions':
				var abilityTable = msg.a;
				var optionsPerLevel = msg.b;
				var traitsAndBonusesPerLevel = msg.c;
				var charLevel = A2(
					$elm$core$Maybe$withDefault,
					1,
					$elm$core$List$maximum(
						$elm$core$Dict$keys(optionsPerLevel)));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							page: $author$project$Types$EditCharacterPage(
								{
									abilityTable: abilityTable,
									charLevel: charLevel,
									desc: $elm$core$Maybe$Nothing,
									optionsPerLevel: optionsPerLevel,
									selectedLevel: $elm$core$Maybe$Just(1),
									setAbilitiesOnNextTick: $elm$core$Dict$empty,
									traitsAndBonusesPerLevel: traitsAndBonusesPerLevel
								})
						}),
					$elm$core$Platform$Cmd$none);
			case 'GotEquipment':
				var equipment = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							page: $author$project$Types$EquipmentPage(equipment)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ChoiceRegistered':
				return _Utils_Tuple2(model, $author$project$Page$EditCharacter$load);
			default:
				return A2(
					$author$project$Types$errorPage,
					model,
					'handleHttpResponseMsg called with unsupported message: ' + $elm$core$Debug$toString(msg));
		}
	});
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $author$project$Types$GotEquipment = function (a) {
	return {$: 'GotEquipment', a: a};
};
var $author$project$Types$Equipment = F2(
	function (weapons, weapon_options) {
		return {weapon_options: weapon_options, weapons: weapons};
	});
var $author$project$Types$Weapon = F8(
	function (base_weapon, enchantment, category, range, to_hit, damage, notes, is_variant) {
		return {base_weapon: base_weapon, category: category, damage: damage, enchantment: enchantment, is_variant: is_variant, notes: notes, range: range, to_hit: to_hit};
	});
var $author$project$Decoder$Equipment$weaponDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2($elm$json$Json$Decode$field, 'is_variant', $elm$json$Json$Decode$bool),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'notes', $elm$json$Json$Decode$string),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'damage', $elm$json$Json$Decode$string),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'to_hit', $elm$json$Json$Decode$string),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'range', $elm$json$Json$Decode$string),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'category', $elm$json$Json$Decode$string),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2($elm$json$Json$Decode$field, 'enchantment', $elm$json$Json$Decode$int),
							A2(
								$elm_community$json_extra$Json$Decode$Extra$andMap,
								A2($elm$json$Json$Decode$field, 'base_weapon', $elm$json$Json$Decode$string),
								$elm$json$Json$Decode$succeed($author$project$Types$Weapon)))))))));
var $author$project$Decoder$Equipment$gotEquipmentDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'weapon_options',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2(
			$elm$json$Json$Decode$field,
			'weapons',
			$elm$json$Json$Decode$list($author$project$Decoder$Equipment$weaponDec)),
		$elm$json$Json$Decode$succeed($author$project$Types$Equipment)));
var $author$project$Page$Equipment$expectGotEquipment = A2(
	$elm$http$Http$expectJson,
	$author$project$Types$mkHttpResponseMsg($author$project$Types$GotEquipment),
	$author$project$Decoder$Equipment$gotEquipmentDec);
var $author$project$Page$Equipment$load = $elm$http$Http$get(
	{
		expect: $author$project$Page$Equipment$expectGotEquipment,
		url: A2($author$project$Request$requestUrl, 'equipment', _List_Nil)
	});
var $author$project$Types$GotPrintableCharSheet = function (a) {
	return {$: 'GotPrintableCharSheet', a: a};
};
var $author$project$Types$CharacterSheet = function (name) {
	return function (summary) {
		return function (ac_formulas) {
			return function (hit_dice) {
				return function (ability_table) {
					return function (skill_table) {
						return function (languages) {
							return function (weapons) {
								return function (armor) {
									return function (tools) {
										return function (notable_traits) {
											return function (attacks) {
												return function (pact_magic) {
													return function (spellcasting_sections) {
														return function (spell_slots) {
															return function (resources) {
																return {ability_table: ability_table, ac_formulas: ac_formulas, armor: armor, attacks: attacks, hit_dice: hit_dice, languages: languages, name: name, notable_traits: notable_traits, pact_magic: pact_magic, resources: resources, skill_table: skill_table, spell_slots: spell_slots, spellcasting_sections: spellcasting_sections, summary: summary, tools: tools, weapons: weapons};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Types$AcFormula = F3(
	function (name, ac, shield) {
		return {ac: ac, name: name, shield: shield};
	});
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Decoder$CharacterSheet$acFormulaDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'shield',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'ac', $elm$json$Json$Decode$int),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
			$elm$json$Json$Decode$succeed($author$project$Types$AcFormula))));
var $author$project$Types$Attack = F5(
	function (name, range, to_hit_or_dc, damage, notes) {
		return {damage: damage, name: name, notes: notes, range: range, to_hit_or_dc: to_hit_or_dc};
	});
var $author$project$Decoder$CharacterSheet$attackDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2($elm$json$Json$Decode$field, 'notes', $elm$json$Json$Decode$string),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'damage', $elm$json$Json$Decode$string),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'to_hit_or_dc', $elm$json$Json$Decode$string),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'range', $elm$json$Json$Decode$string),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$succeed($author$project$Types$Attack))))));
var $author$project$Types$HitDice = F2(
	function (n, d) {
		return {d: d, n: n};
	});
var $author$project$Decoder$CharacterSheet$hitDiceDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2($elm$json$Json$Decode$field, 'd', $elm$json$Json$Decode$int),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'n', $elm$json$Json$Decode$int),
		$elm$json$Json$Decode$succeed($author$project$Types$HitDice)));
var $author$project$Types$NotableTraitCategory = F2(
	function (category, traits) {
		return {category: category, traits: traits};
	});
var $author$project$Types$Trait = F2(
	function (name, desc) {
		return {desc: desc, name: name};
	});
var $author$project$Decoder$CharacterSheet$traitDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'desc',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
		$elm$json$Json$Decode$succeed($author$project$Types$Trait)));
var $author$project$Decoder$CharacterSheet$notableTraitDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'traits',
		$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$traitDec)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'category', $elm$json$Json$Decode$string),
		$elm$json$Json$Decode$succeed($author$project$Types$NotableTraitCategory)));
var $author$project$Decoder$CharacterSheet$notableTraitsDec = $elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$notableTraitDec);
var $author$project$Types$PactMagic = F2(
	function (slot_count, slot_level) {
		return {slot_count: slot_count, slot_level: slot_level};
	});
var $author$project$Decoder$CharacterSheet$pactMagicDec = $elm$json$Json$Decode$nullable(
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'slot_level', $elm$json$Json$Decode$int),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'slot_count', $elm$json$Json$Decode$int),
			$elm$json$Json$Decode$succeed($author$project$Types$PactMagic))));
var $author$project$Types$Resource = F5(
	function (feature_name, unit_name, number, short_rest, long_rest) {
		return {feature_name: feature_name, long_rest: long_rest, number: number, short_rest: short_rest, unit_name: unit_name};
	});
var $author$project$Decoder$CharacterSheet$resourceDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'long_rest',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2(
			$elm$json$Json$Decode$field,
			'short_rest',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'number', $elm$json$Json$Decode$int),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'unit_name', $elm$json$Json$Decode$string),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'feature_name', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$succeed($author$project$Types$Resource))))));
var $author$project$Types$Ability$SkillTableEntry = F2(
	function (score, proficient) {
		return {proficient: proficient, score: score};
	});
var $author$project$Decoder$AbilityTable$skillTableDec = $elm$json$Json$Decode$dict(
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'proficient', $elm$json$Json$Decode$bool),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$int),
			$elm$json$Json$Decode$succeed($author$project$Types$Ability$SkillTableEntry))));
var $author$project$Types$SpellcastingSection = F7(
	function (max_prepared_spells, origin, spell_attack_mod, spell_save_dc, spellcasting_ability, spellcasting_ability_mod, spells) {
		return {max_prepared_spells: max_prepared_spells, origin: origin, spell_attack_mod: spell_attack_mod, spell_save_dc: spell_save_dc, spellcasting_ability: spellcasting_ability, spellcasting_ability_mod: spellcasting_ability_mod, spells: spells};
	});
var $author$project$Types$Spell = function (aoe) {
	return function (casting_time) {
		return function (components) {
			return function (concentration) {
				return function (dc) {
					return function (dc_abi) {
						return function (description) {
							return function (higher_level) {
								return function (duration) {
									return function (level) {
										return function (name) {
											return function (prepared) {
												return function (range) {
													return function (resources) {
														return function (ritual) {
															return function (school) {
																return function (shortdesc) {
																	return function (summary) {
																		return function (to_hit) {
																			return function (rolls) {
																				return function (bonuses) {
																					return {aoe: aoe, bonuses: bonuses, casting_time: casting_time, components: components, concentration: concentration, dc: dc, dc_abi: dc_abi, description: description, duration: duration, higher_level: higher_level, level: level, name: name, prepared: prepared, range: range, resources: resources, ritual: ritual, rolls: rolls, school: school, shortdesc: shortdesc, summary: summary, to_hit: to_hit};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Types$M = function (a) {
	return {$: 'M', a: a};
};
var $author$project$Types$S = {$: 'S'};
var $author$project$Types$V = {$: 'V'};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $author$project$Decoder$CharacterSheet$componentDec = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$andThen,
			function (str) {
				switch (str) {
					case 'v':
						return $elm$json$Json$Decode$succeed($author$project$Types$V);
					case 's':
						return $elm$json$Json$Decode$succeed($author$project$Types$S);
					default:
						return $elm$json$Json$Decode$fail('Expected either \"v\" or \"s\"');
				}
			},
			$elm$json$Json$Decode$string),
			A2(
			$elm$json$Json$Decode$map,
			$author$project$Types$M,
			A2(
				$elm$json$Json$Decode$map,
				$elm$core$String$concat,
				A2(
					$elm$json$Json$Decode$field,
					'args',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string))))
		]));
var $author$project$Util$exactMatchDec = F2(
	function (dec, val) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (other) {
				var _v0 = _Utils_eq(val, other);
				if (_v0) {
					return $elm$json$Json$Decode$succeed(val);
				} else {
					return $elm$json$Json$Decode$fail('mismatch in exactMatchDec');
				}
			},
			dec);
	});
var $author$project$Decoder$CharacterSheet$preparedDec = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$map,
			function (_v0) {
				return true;
			},
			A2($author$project$Util$exactMatchDec, $elm$json$Json$Decode$string, 'always')),
			A2(
			$elm$json$Json$Decode$map,
			function (_v1) {
				return false;
			},
			A2($author$project$Util$exactMatchDec, $elm$json$Json$Decode$string, 'maybe'))
		]));
var $author$project$Types$NotRitual = {$: 'NotRitual'};
var $author$project$Types$OnlyRitual = {$: 'OnlyRitual'};
var $author$project$Types$Ritual = {$: 'Ritual'};
var $author$project$Decoder$CharacterSheet$ritualDec = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$map,
			function (_v0) {
				return $author$project$Types$Ritual;
			},
			$author$project$Util$matchStringDec('yes')),
			A2(
			$elm$json$Json$Decode$map,
			function (_v1) {
				return $author$project$Types$NotRitual;
			},
			$author$project$Util$matchStringDec('no')),
			A2(
			$elm$json$Json$Decode$map,
			function (_v2) {
				return $author$project$Types$OnlyRitual;
			},
			$author$project$Util$matchStringDec('only'))
		]));
var $author$project$Types$SpellBonus = F2(
	function (origin, bonus) {
		return {bonus: bonus, origin: origin};
	});
var $author$project$Decoder$CharacterSheet$spellBonusDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2($elm$json$Json$Decode$field, 'bonus', $elm$json$Json$Decode$string),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'origin', $elm$json$Json$Decode$string),
		$elm$json$Json$Decode$succeed($author$project$Types$SpellBonus)));
var $author$project$Util$yesNoDec = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$map,
			function (_v0) {
				return true;
			},
			$author$project$Util$matchStringDec('yes')),
			A2(
			$elm$json$Json$Decode$map,
			function (_v1) {
				return false;
			},
			$author$project$Util$matchStringDec('no'))
		]));
var $author$project$Decoder$CharacterSheet$spellDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'bonuses',
		$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$spellBonusDec)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2(
			$elm$json$Json$Decode$field,
			'rolls',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2(
				$elm$json$Json$Decode$field,
				'to_hit',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2(
						$elm$json$Json$Decode$field,
						'shortdesc',
						$elm$json$Json$Decode$nullable(
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string))),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'school', $elm$json$Json$Decode$string),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2($elm$json$Json$Decode$field, 'ritual', $author$project$Decoder$CharacterSheet$ritualDec),
							A2(
								$elm_community$json_extra$Json$Decode$Extra$andMap,
								A2(
									$elm$json$Json$Decode$field,
									'resources',
									$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
								A2(
									$elm_community$json_extra$Json$Decode$Extra$andMap,
									A2($elm$json$Json$Decode$field, 'range', $elm$json$Json$Decode$string),
									A2(
										$elm_community$json_extra$Json$Decode$Extra$andMap,
										A2($elm$json$Json$Decode$field, 'prepared', $author$project$Decoder$CharacterSheet$preparedDec),
										A2(
											$elm_community$json_extra$Json$Decode$Extra$andMap,
											A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
											A2(
												$elm_community$json_extra$Json$Decode$Extra$andMap,
												A2($elm$json$Json$Decode$field, 'level', $elm$json$Json$Decode$int),
												A2(
													$elm_community$json_extra$Json$Decode$Extra$andMap,
													A2($elm$json$Json$Decode$field, 'duration', $elm$json$Json$Decode$string),
													A2(
														$elm_community$json_extra$Json$Decode$Extra$andMap,
														A2(
															$elm$json$Json$Decode$field,
															'higher_level',
															$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
														A2(
															$elm_community$json_extra$Json$Decode$Extra$andMap,
															A2(
																$elm$json$Json$Decode$field,
																'description',
																$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
															A2(
																$elm_community$json_extra$Json$Decode$Extra$andMap,
																A2(
																	$elm$json$Json$Decode$field,
																	'dc_abi',
																	$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
																A2(
																	$elm_community$json_extra$Json$Decode$Extra$andMap,
																	A2(
																		$elm$json$Json$Decode$field,
																		'dc',
																		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
																	A2(
																		$elm_community$json_extra$Json$Decode$Extra$andMap,
																		A2($elm$json$Json$Decode$field, 'concentration', $author$project$Util$yesNoDec),
																		A2(
																			$elm_community$json_extra$Json$Decode$Extra$andMap,
																			A2(
																				$elm$json$Json$Decode$field,
																				'components',
																				$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$componentDec)),
																			A2(
																				$elm_community$json_extra$Json$Decode$Extra$andMap,
																				A2($elm$json$Json$Decode$field, 'casting_time', $elm$json$Json$Decode$string),
																				A2(
																					$elm_community$json_extra$Json$Decode$Extra$andMap,
																					A2(
																						$elm$json$Json$Decode$field,
																						'aoe',
																						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
																					$elm$json$Json$Decode$succeed($author$project$Types$Spell))))))))))))))))))))));
var $author$project$Decoder$CharacterSheet$spellcastingSectionDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'spells',
		$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$spellDec)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'spellcasting_ability_mod', $elm$json$Json$Decode$int),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'spellcasting_ability', $elm$json$Json$Decode$string),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'spell_save_dc', $elm$json$Json$Decode$int),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'spell_attack_mod', $elm$json$Json$Decode$int),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'origin', $elm$json$Json$Decode$string),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2(
								$elm$json$Json$Decode$field,
								'max_prepared_spells',
								$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
							$elm$json$Json$Decode$succeed($author$project$Types$SpellcastingSection))))))));
var $author$project$Types$CharacterSummary = function (ac) {
	return function (_class) {
		return function (hd) {
			return function (initiative) {
				return function (level) {
					return function (maxhp) {
						return function (pp) {
							return function (prof_bon) {
								return function (race) {
									return function (speed) {
										return {ac: ac, _class: _class, hd: hd, initiative: initiative, level: level, maxhp: maxhp, pp: pp, prof_bon: prof_bon, race: race, speed: speed};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Decoder$CharacterSheet$summaryDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2($elm$json$Json$Decode$field, 'speed', $elm$json$Json$Decode$string),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2($elm$json$Json$Decode$field, 'race', $elm$json$Json$Decode$string),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2($elm$json$Json$Decode$field, 'prof_bon', $elm$json$Json$Decode$int),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'pp', $elm$json$Json$Decode$int),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2($elm$json$Json$Decode$field, 'maxhp', $elm$json$Json$Decode$int),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'level', $elm$json$Json$Decode$int),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2($elm$json$Json$Decode$field, 'initiative', $elm$json$Json$Decode$int),
							A2(
								$elm_community$json_extra$Json$Decode$Extra$andMap,
								A2($elm$json$Json$Decode$field, 'hd', $elm$json$Json$Decode$string),
								A2(
									$elm_community$json_extra$Json$Decode$Extra$andMap,
									A2($elm$json$Json$Decode$field, 'class', $elm$json$Json$Decode$string),
									A2(
										$elm_community$json_extra$Json$Decode$Extra$andMap,
										A2($elm$json$Json$Decode$field, 'ac', $elm$json$Json$Decode$int),
										$elm$json$Json$Decode$succeed($author$project$Types$CharacterSummary)))))))))));
var $author$project$Decoder$CharacterSheet$sheetDec = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'resources',
		$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$resourceDec)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2(
			$elm$json$Json$Decode$field,
			'spell_slots',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$int)),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2(
				$elm$json$Json$Decode$field,
				'spellcasting_sections',
				$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$spellcastingSectionDec)),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2($elm$json$Json$Decode$field, 'pact_magic', $author$project$Decoder$CharacterSheet$pactMagicDec),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2(
						$elm$json$Json$Decode$field,
						'attacks',
						$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$attackDec)),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2($elm$json$Json$Decode$field, 'notable_traits', $author$project$Decoder$CharacterSheet$notableTraitsDec),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2(
								$elm$json$Json$Decode$field,
								'tools',
								$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
							A2(
								$elm_community$json_extra$Json$Decode$Extra$andMap,
								A2(
									$elm$json$Json$Decode$field,
									'armor',
									$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
								A2(
									$elm_community$json_extra$Json$Decode$Extra$andMap,
									A2(
										$elm$json$Json$Decode$field,
										'weapons',
										$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
									A2(
										$elm_community$json_extra$Json$Decode$Extra$andMap,
										A2(
											$elm$json$Json$Decode$field,
											'languages',
											$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
										A2(
											$elm_community$json_extra$Json$Decode$Extra$andMap,
											A2($elm$json$Json$Decode$field, 'skill_table', $author$project$Decoder$AbilityTable$skillTableDec),
											A2(
												$elm_community$json_extra$Json$Decode$Extra$andMap,
												A2($elm$json$Json$Decode$field, 'ability_table', $author$project$Decoder$AbilityTable$abilityTableDec),
												A2(
													$elm_community$json_extra$Json$Decode$Extra$andMap,
													A2(
														$elm$json$Json$Decode$field,
														'hit_dice',
														$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$hitDiceDec)),
													A2(
														$elm_community$json_extra$Json$Decode$Extra$andMap,
														A2(
															$elm$json$Json$Decode$field,
															'ac_formulas',
															$elm$json$Json$Decode$list($author$project$Decoder$CharacterSheet$acFormulaDec)),
														A2(
															$elm_community$json_extra$Json$Decode$Extra$andMap,
															A2($elm$json$Json$Decode$field, 'summary', $author$project$Decoder$CharacterSheet$summaryDec),
															A2(
																$elm_community$json_extra$Json$Decode$Extra$andMap,
																A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
																$elm$json$Json$Decode$succeed($author$project$Types$CharacterSheet)))))))))))))))));
var $author$project$Page$PrintableCharSheet$load = $elm$http$Http$get(
	{
		expect: A2(
			$elm$http$Http$expectJson,
			$author$project$Types$mkHttpResponseMsg($author$project$Types$GotPrintableCharSheet),
			$author$project$Decoder$CharacterSheet$sheetDec),
		url: A2($author$project$Request$requestUrl, 'sheet', _List_Nil)
	});
var $author$project$Main$navigate = F2(
	function (model, route) {
		switch (route.$) {
			case 'CharacterRoute':
				var charName = route.a;
				return A2(
					$elm$core$Debug$log,
					'navigate (CharacterRoute ' + (charName + ')'),
					_Utils_Tuple2(
						_Utils_update(
							model,
							{page: $author$project$Types$Loading}),
						$author$project$Page$PrintableCharSheet$load));
			case 'SelectCharacterRoute':
				return A2(
					$elm$core$Debug$log,
					'navigate (SelectCharacterRoute)',
					_Utils_Tuple2(
						_Utils_update(
							model,
							{page: $author$project$Types$Loading}),
						$author$project$Main$loadSelectCharacterPage));
			case 'SheetRoute':
				return A2(
					$elm$core$Debug$log,
					'navigate (SheetRoute)',
					_Utils_Tuple2(
						_Utils_update(
							model,
							{page: $author$project$Types$Loading}),
						$author$project$Page$PrintableCharSheet$load));
			case 'EditRoute':
				return A2(
					$elm$core$Debug$log,
					'navigate (EditRoute)',
					_Utils_Tuple2(
						_Utils_update(
							model,
							{page: $author$project$Types$Loading}),
						$author$project$Page$EditCharacter$load));
			case 'EquipmentRoute':
				return A2(
					$elm$core$Debug$log,
					'navigate (EquipmentRoute)',
					_Utils_Tuple2(
						_Utils_update(
							model,
							{page: $author$project$Types$Loading}),
						$author$project$Page$Equipment$load));
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Types$ChoiceRegistered = {$: 'ChoiceRegistered'};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $author$project$Main$choiceToString = function (choice) {
	if (choice.$ === 'SingletonChoice') {
		var sc = choice.a;
		return sc;
	} else {
		var list = choice.a;
		return '[' + ($elm$core$String$concat(
			A2($elm$core$List$intersperse, ',', list)) + ']');
	}
};
var $author$project$Main$registerChoice = F3(
	function (origin, id, choice) {
		var _v0 = A2($elm$core$Debug$log, 'registerChoice', choice);
		return $elm$http$Http$get(
			{
				expect: A2(
					$elm$http$Http$expectJson,
					$author$project$Types$mkHttpResponseMsg(
						function (_v1) {
							return $author$project$Types$ChoiceRegistered;
						}),
					$elm$json$Json$Decode$succeed(_Utils_Tuple0)),
				url: A2(
					$author$project$Request$requestUrl,
					'choice',
					_List_fromArray(
						[
							_Utils_Tuple2('source', origin),
							_Utils_Tuple2('id', id),
							_Utils_Tuple2(
							'choice',
							$author$project$Main$choiceToString(choice))
						]))
			});
	});
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Page$CardsPage$update = F3(
	function (msg, model, sheet) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$remove, key, dict));
	});
var $author$project$Types$setSpellPreparedness = F4(
	function (origin, spell, prepared, old) {
		return A2(
			$elm$core$Debug$log,
			'setSpellPreparedness',
			A3(
				$elm$core$Dict$update,
				origin,
				$elm$core$Maybe$map(
					function (set) {
						if (!prepared) {
							return A2($elm$core$Set$remove, spell, set);
						} else {
							return A2($elm$core$Set$insert, spell, set);
						}
					}),
				old));
	});
var $author$project$Page$CharacterSheet$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'SetSpellPreparedness':
				var origin = msg.a;
				var spell = msg.b;
				var prepared = msg.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							preparedSpells: A4($author$project$Types$setSpellPreparedness, origin, spell, prepared, model.preparedSpells)
						}),
					$elm$core$Platform$Cmd$none);
			case 'SetShowOnlyPreparedSpells':
				var _new = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{showOnlyPreparedSpells: _new}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Types$LeveledUp = {$: 'LeveledUp'};
var $author$project$Types$UpdatedBaseAbilityScores = {$: 'UpdatedBaseAbilityScores'};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$Page$EditCharacter$applyBaseAbilityChanges = F2(
	function (abilityTable, setAbilitiesOnNextTick) {
		return A2(
			$elm$core$Dict$union,
			setAbilitiesOnNextTick,
			A2(
				$elm$core$Dict$map,
				F2(
					function (k, v) {
						return v.base;
					}),
				abilityTable));
	});
var $author$project$Page$EditCharacter$applyPageData = F2(
	function (model, data) {
		return _Utils_update(
			model,
			{
				page: $author$project$Types$EditCharacterPage(data)
			});
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			$elm$core$Dict$filter,
			F2(
				function (k, _v0) {
					return A2($elm$core$Dict$member, k, t2);
				}),
			t1);
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Page$EditCharacter$update = F3(
	function (msg, model, oldData) {
		_v0$12:
		while (true) {
			switch (msg.$) {
				case 'HttpResponse':
					if (msg.a.$ === 'Ok') {
						switch (msg.a.a.$) {
							case 'GotCharacterOptions':
								var _v1 = msg.a.a;
								var newAbilityTable = _v1.a;
								var newOptions = _v1.b;
								var newTraitsAndBonuses = _v1.c;
								var charLevel = A2(
									$elm$core$Maybe$withDefault,
									1,
									$elm$core$List$maximum(
										$elm$core$Dict$keys(newOptions)));
								return A2(
									$author$project$Types$applyPage,
									model,
									_Utils_Tuple2(
										$author$project$Types$EditCharacterPage(
											{
												abilityTable: newAbilityTable,
												charLevel: charLevel,
												desc: $elm$core$Maybe$Nothing,
												optionsPerLevel: newOptions,
												selectedLevel: $elm$core$Maybe$Just(
													A2($elm$core$Maybe$withDefault, charLevel, oldData.selectedLevel)),
												setAbilitiesOnNextTick: function () {
													var newBaseAbilities = A2(
														$elm$core$Dict$map,
														F2(
															function (_v2, v) {
																return v.base;
															}),
														newAbilityTable);
													var changedAbilities = A2($elm$core$Dict$intersect, newBaseAbilities, oldData.setAbilitiesOnNextTick);
													return _Utils_eq(changedAbilities, oldData.setAbilitiesOnNextTick) ? $elm$core$Dict$empty : oldData.setAbilitiesOnNextTick;
												}(),
												traitsAndBonusesPerLevel: newTraitsAndBonuses
											}),
										$elm$core$Platform$Cmd$none));
							case 'ChoiceRegistered':
								var _v3 = msg.a.a;
								return _Utils_Tuple2(model, $author$project$Page$EditCharacter$load);
							case 'LeveledUp':
								var _v4 = msg.a.a;
								return _Utils_Tuple2(model, $author$project$Page$EditCharacter$load);
							case 'UpdatedBaseAbilityScores':
								var _v5 = msg.a.a;
								return _Utils_Tuple2(model, $author$project$Page$EditCharacter$load);
							default:
								break _v0$12;
						}
					} else {
						break _v0$12;
					}
				case 'Tick':
					return _Utils_Tuple2(
						model,
						$elm$core$Dict$isEmpty(oldData.setAbilitiesOnNextTick) ? $elm$core$Platform$Cmd$none : $elm$http$Http$get(
							{
								expect: A2(
									$elm$http$Http$expectJson,
									$author$project$Types$mkHttpResponseMsg(
										function (_v6) {
											return $author$project$Types$UpdatedBaseAbilityScores;
										}),
									$elm$json$Json$Decode$succeed(_Utils_Tuple0)),
								url: A2(
									$author$project$Request$requestUrl,
									'set_base_abilities',
									$elm$core$Dict$toList(
										A2(
											$elm$core$Dict$map,
											F2(
												function (_v7, v) {
													return $elm$core$String$fromInt(v);
												}),
											A2($author$project$Page$EditCharacter$applyBaseAbilityChanges, oldData.abilityTable, oldData.setAbilitiesOnNextTick))))
							}));
				case 'EditCharacterLevel':
					var newLevel = msg.a;
					return _Utils_Tuple2(
						A2(
							$author$project$Page$EditCharacter$applyPageData,
							model,
							_Utils_update(
								oldData,
								{
									selectedLevel: $elm$core$Maybe$Just(newLevel)
								})),
						$elm$core$Platform$Cmd$none);
				case 'GotoSheet':
					return A2(
						$author$project$Types$applyPage,
						model,
						_Utils_Tuple2(
							$author$project$Types$Loading,
							A2($elm$browser$Browser$Navigation$pushUrl, model.key, '/sheet')));
				case 'GotoLevelUp':
					return _Utils_Tuple2(
						A2(
							$author$project$Page$EditCharacter$applyPageData,
							model,
							_Utils_update(
								oldData,
								{desc: $elm$core$Maybe$Nothing, selectedLevel: $elm$core$Maybe$Nothing})),
						$elm$core$Platform$Cmd$none);
				case 'LevelUpAs':
					var _class = msg.a;
					return _Utils_Tuple2(
						model,
						$elm$http$Http$get(
							{
								expect: A2(
									$elm$http$Http$expectJson,
									$author$project$Types$mkHttpResponseMsg(
										function (_v8) {
											return $author$project$Types$LeveledUp;
										}),
									$elm$json$Json$Decode$succeed(_Utils_Tuple0)),
								url: A2(
									$author$project$Request$requestUrl,
									'gain_level',
									_List_fromArray(
										[
											_Utils_Tuple2('class', _class)
										]))
							}));
				case 'SetBaseAbilityScore':
					var ability = msg.a;
					var newScore = msg.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Page$EditCharacter$applyPageData,
							model,
							_Utils_update(
								oldData,
								{
									setAbilitiesOnNextTick: A3($elm$core$Dict$insert, ability, newScore, oldData.setAbilitiesOnNextTick)
								})),
						$elm$core$Platform$Cmd$none);
				case 'OrSCChooseDir':
					var origin = msg.a;
					var id = msg.b;
					var dir = msg.c;
					var newOptions = A2(
						$elm$core$Dict$map,
						function (_v10) {
							return $elm$core$List$map(
								function (opt) {
									if (!_Utils_eq(
										_Utils_Tuple2(origin, id),
										_Utils_Tuple2(opt.origin, opt.id))) {
										return opt;
									} else {
										var _v11 = opt.spec;
										if (_v11.$ === 'OrSC') {
											var left = _v11.b;
											var right = _v11.c;
											return _Utils_update(
												opt,
												{
													spec: A3(
														$author$project$Types$OrSC,
														$elm$core$Maybe$Just(dir),
														left,
														right)
												});
										} else {
											return opt;
										}
									}
								});
						},
						oldData.optionsPerLevel);
					var _v9 = A2(
						$elm$core$Debug$log,
						'',
						_Utils_Tuple3(origin, id, dir));
					return _Utils_Tuple2(
						A2(
							$author$project$Page$EditCharacter$applyPageData,
							model,
							_Utils_update(
								oldData,
								{desc: $elm$core$Maybe$Nothing, optionsPerLevel: newOptions})),
						$elm$core$Platform$Cmd$none);
				case 'SetEditCharacterPageDesc':
					var desc = msg.a;
					return _Utils_Tuple2(
						A2(
							$author$project$Page$EditCharacter$applyPageData,
							model,
							_Utils_update(
								oldData,
								{desc: desc})),
						$elm$core$Platform$Cmd$none);
				default:
					break _v0$12;
			}
		}
		var _v12 = A2($elm$core$Debug$log, '', msg);
		return A2(
			$author$project$Types$errorPage,
			model,
			'Page.EditCharacter.update called with ' + $elm$core$Debug$toString(msg));
	});
var $author$project$Page$Equipment$applyPageData = F2(
	function (model, data) {
		return _Utils_update(
			model,
			{
				page: $author$project$Types$EquipmentPage(data)
			});
	});
var $author$project$Page$Equipment$update = F3(
	function (msg, model, oldEquipment) {
		_v0$3:
		while (true) {
			switch (msg.$) {
				case 'UnequipWeapon':
					var base_weapon = msg.a.base_weapon;
					var enchantment = msg.a.enchantment;
					return _Utils_Tuple2(
						model,
						$elm$http$Http$get(
							{
								expect: $author$project$Page$Equipment$expectGotEquipment,
								url: A2(
									$author$project$Request$requestUrl,
									'unequip_weapon',
									_List_fromArray(
										[
											_Utils_Tuple2('base_weapon', base_weapon),
											_Utils_Tuple2(
											'enchantment',
											$elm$core$String$fromInt(enchantment))
										]))
							}));
				case 'EquipWeapon':
					var w = msg.a;
					return _Utils_Tuple2(
						model,
						$elm$http$Http$get(
							{
								expect: $author$project$Page$Equipment$expectGotEquipment,
								url: A2(
									$author$project$Request$requestUrl,
									'equip_weapon',
									_List_fromArray(
										[
											_Utils_Tuple2('weapon', w)
										]))
							}));
				case 'HttpResponse':
					if ((msg.a.$ === 'Ok') && (msg.a.a.$ === 'GotEquipment')) {
						var newEquipment = msg.a.a.a;
						return _Utils_Tuple2(
							A2($author$project$Page$Equipment$applyPageData, model, newEquipment),
							$elm$core$Platform$Cmd$none);
					} else {
						break _v0$3;
					}
				default:
					break _v0$3;
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Page$PrintableCharSheet$update = F2(
	function (msg, model) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Types$CharacterLoaded = function (a) {
	return {$: 'CharacterLoaded', a: a};
};
var $author$project$Main$loadCharacter = function (charName) {
	return $elm$http$Http$get(
		{
			expect: A2(
				$elm$http$Http$expectJson,
				$author$project$Types$mkHttpResponseMsg(
					function (_v0) {
						return $author$project$Types$CharacterLoaded(charName);
					}),
				$elm$json$Json$Decode$succeed(_Utils_Tuple0)),
			url: A2(
				$author$project$Request$requestUrl,
				'load_character',
				_List_fromArray(
					[
						_Utils_Tuple2('name', charName)
					]))
		});
};
var $author$project$Main$newCharacter = function (charName) {
	return $elm$http$Http$get(
		{
			expect: A2(
				$elm$http$Http$expectJson,
				$author$project$Types$mkHttpResponseMsg(
					function (_v0) {
						return $author$project$Types$CharacterLoaded(charName);
					}),
				$elm$json$Json$Decode$succeed(_Utils_Tuple0)),
			url: A2(
				$author$project$Request$requestUrl,
				'new_character',
				_List_fromArray(
					[
						_Utils_Tuple2('name', charName)
					]))
		});
};
var $author$project$Main$updateCharacterSelectionPage = F2(
	function (msg, pageData) {
		switch (msg.$) {
			case 'NewCharacterName':
				var name = msg.a;
				return _Utils_Tuple2(
					$author$project$Types$CharacterSelectionPage(
						_Utils_update(
							pageData,
							{newCharacterName: name})),
					$elm$core$Platform$Cmd$none);
			case 'SelectCharacter':
				var name = msg.a;
				return _Utils_Tuple2(
					$author$project$Types$Loading,
					$author$project$Main$loadCharacter(name));
			case 'CreateNewCharacter':
				return _Utils_Tuple2(
					$author$project$Types$Loading,
					$author$project$Main$newCharacter(pageData.newCharacterName));
			default:
				return _Utils_Tuple2(
					$author$project$Types$Error(
						'Main.updateCharacterSelectionPage: unrecognized msg ' + $elm$core$Debug$toString(msg)),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$NotFound = {$: 'NotFound'};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Main$CharacterRoute = function (a) {
	return {$: 'CharacterRoute', a: a};
};
var $author$project$Main$EditRoute = {$: 'EditRoute'};
var $author$project$Main$EquipmentRoute = {$: 'EquipmentRoute'};
var $author$project$Main$SelectCharacterRoute = {$: 'SelectCharacterRoute'};
var $author$project$Main$SheetRoute = {$: 'SheetRoute'};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return $elm$url$Url$Parser$Parser(
			function (_v0) {
				var visited = _v0.visited;
				var unvisited = _v0.unvisited;
				var params = _v0.params;
				var frag = _v0.frag;
				var value = _v0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _v2 = stringToSomething(next);
					if (_v2.$ === 'Just') {
						var nextValue = _v2.a;
						return _List_fromArray(
							[
								A5(
								$elm$url$Url$Parser$State,
								A2($elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var $elm$url$Url$Parser$string = A2($elm$url$Url$Parser$custom, 'STRING', $elm$core$Maybe$Just);
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $author$project$Main$routeParser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2($elm$url$Url$Parser$map, $author$project$Main$SelectCharacterRoute, $elm$url$Url$Parser$top),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$SelectCharacterRoute,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('src'),
				$elm$url$Url$Parser$s('Main.elm'))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$SelectCharacterRoute,
			$elm$url$Url$Parser$s('list_characters')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$SheetRoute,
			$elm$url$Url$Parser$s('sheet')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$EditRoute,
			$elm$url$Url$Parser$s('edit')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$CharacterRoute,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('character'),
				$elm$url$Url$Parser$string)),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$EquipmentRoute,
			$elm$url$Url$Parser$s('equipment'))
		]));
var $author$project$Main$urlToRoute = function (url) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Main$NotFound,
		A2($elm$url$Url$Parser$parse, $author$project$Main$routeParser, url));
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Null':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'LinkClicked':
				var urlRequest = msg.a;
				if (urlRequest.$ === 'Internal') {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.key,
							$elm$url$Url$toString(url)));
				} else {
					var href = urlRequest.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(href));
				}
			case 'UrlChanged':
				var url = msg.a;
				return A2(
					$author$project$Main$navigate,
					model,
					$author$project$Main$urlToRoute(url));
			case 'EditCharacter':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{page: $author$project$Types$Loading}),
					$author$project$Page$EditCharacter$load);
			case 'GotoSelectCharacterPage':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{page: $author$project$Types$Loading}),
					$author$project$Main$loadSelectCharacterPage);
			case 'GotoCardsPage':
				var options = msg.a;
				var sheet = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							page: A2($author$project$Types$CardsPage, options, sheet)
						}),
					$elm$core$Platform$Cmd$none);
			case 'GotoSheet':
				return A2(
					$author$project$Types$applyPage,
					model,
					_Utils_Tuple2(
						$author$project$Types$Loading,
						A2($elm$browser$Browser$Navigation$pushUrl, model.key, '/sheet')));
			case 'GotoEquipmentPage':
				return A2(
					$author$project$Types$applyPage,
					model,
					_Utils_Tuple2(
						$author$project$Types$Loading,
						A2($elm$browser$Browser$Navigation$pushUrl, model.key, '/equipment')));
			case 'Choice':
				var origin = msg.a;
				var id = msg.b;
				var choice = msg.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{focusedDropdownId: $elm$core$Maybe$Nothing}),
					A3($author$project$Main$registerChoice, origin, id, choice));
			case 'SelectDropdownOption':
				var dropdownId = msg.a;
				var optionId = msg.b;
				var _v2 = A2($elm$core$Debug$log, '', 'Selected dropdown option ' + (optionId + (' from dropdown ' + dropdownId)));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{focusedDropdownId: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'ToggleDropdown':
				var dropdownId = msg.a;
				var newFocusedDropdownId = _Utils_eq(
					model.focusedDropdownId,
					$elm$core$Maybe$Just(dropdownId)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(dropdownId);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{focusedDropdownId: newFocusedDropdownId}),
					$elm$core$Platform$Cmd$none);
			case 'ClickOut':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{focusedDropdownId: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			default:
				var _v3 = model.page;
				switch (_v3.$) {
					case 'CharacterSelectionPage':
						var pageData = _v3.a;
						return A2(
							$author$project$Types$applyPage,
							model,
							A2($author$project$Main$updateCharacterSelectionPage, msg, pageData));
					case 'CharacterSheetPage':
						var sheet = _v3.a;
						return A2($author$project$Page$CharacterSheet$update, msg, model);
					case 'PrintableCharSheetPage':
						var sheet = _v3.a;
						return A2($author$project$Page$PrintableCharSheet$update, msg, model);
					case 'EditCharacterPage':
						var data = _v3.a;
						return A3($author$project$Page$EditCharacter$update, msg, model, data);
					case 'CardsPage':
						var options = _v3.a;
						var data = _v3.b;
						return A3($author$project$Page$CardsPage$update, msg, model, data);
					case 'EquipmentPage':
						var data = _v3.a;
						return A3($author$project$Page$Equipment$update, msg, model, data);
					case 'Loading':
						if ((msg.$ === 'HttpResponse') && (msg.a.$ === 'Ok')) {
							var responseMsg = msg.a.a;
							return A2($author$project$Main$handleHttpResponseMsg, responseMsg, model);
						} else {
							var error = 'Main.update: Expected HttpResponse, got ' + $elm$core$Debug$toString(msg);
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										page: $author$project$Types$Error(error)
									}),
								$elm$core$Platform$Cmd$none);
						}
					default:
						var error = _v3.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									page: $author$project$Types$Error(error)
								}),
							$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Main$verbose = true;
var $author$project$Main$updateOrTick = F2(
	function (msg, model) {
		if (msg.$ === 'Tick') {
			var time = msg.a;
			return A2(
				$author$project$Main$update,
				msg,
				_Utils_update(
					model,
					{lastTick: time}));
		} else {
			var _v1 = $author$project$Main$verbose ? A2($elm$core$Debug$log, 'Global update (old model)', model) : model;
			var _v2 = $author$project$Main$verbose ? A2($elm$core$Debug$log, 'Global update (msg)', msg) : msg;
			var _v3 = $author$project$Main$verbose ? A2($elm$core$Debug$log, '--------', '') : '';
			return A2($author$project$Main$update, msg, model);
		}
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $author$project$Types$CreateNewCharacter = {$: 'CreateNewCharacter'};
var $author$project$Types$NewCharacterName = function (a) {
	return {$: 'NewCharacterName', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$Node = F3(
	function (a, b, c) {
		return {$: 'Node', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$Node;
var $rtfeldman$elm_css$Html$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$node;
var $rtfeldman$elm_css$Html$Styled$button = $rtfeldman$elm_css$Html$Styled$node('button');
var $rtfeldman$elm_css$Html$Styled$div = $rtfeldman$elm_css$Html$Styled$node('div');
var $rtfeldman$elm_css$Html$Styled$h2 = $rtfeldman$elm_css$Html$Styled$node('h2');
var $rtfeldman$elm_css$Html$Styled$input = $rtfeldman$elm_css$Html$Styled$node('input');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$Attribute = F3(
	function (a, b, c) {
		return {$: 'Attribute', a: a, b: b, c: c};
	});
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $rtfeldman$elm_css$VirtualDom$Styled$on = F2(
	function (eventName, handler) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$on, eventName, handler),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Events$on = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $rtfeldman$elm_css$Html$Styled$Events$onClick = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $rtfeldman$elm_css$Html$Styled$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $rtfeldman$elm_css$Html$Styled$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $rtfeldman$elm_css$Html$Styled$Events$onInput = function (tagger) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$rtfeldman$elm_css$Html$Styled$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $rtfeldman$elm_css$Html$Styled$Events$targetValue)));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$property = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$property, key, value),
			false,
			'');
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$placeholder = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('placeholder');
var $author$project$Types$SelectCharacter = function (a) {
	return {$: 'SelectCharacter', a: a};
};
var $rtfeldman$elm_css$Html$Styled$li = $rtfeldman$elm_css$Html$Styled$node('li');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $rtfeldman$elm_css$VirtualDom$Styled$style = F2(
	function (key, val) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$style, key, val),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$style = $rtfeldman$elm_css$VirtualDom$Styled$style;
var $author$project$Main$linkButtonStyle = _List_fromArray(
	[
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'background', 'none'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'border', 'none'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding', '0'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'font-family', 'arial, sans-serif'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'text-decoration', 'underline'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'cursor', 'pointer')
	]);
var $rtfeldman$elm_css$VirtualDom$Styled$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $rtfeldman$elm_css$VirtualDom$Styled$text = function (str) {
	return $rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
		$elm$virtual_dom$VirtualDom$text(str));
};
var $rtfeldman$elm_css$Html$Styled$text = $rtfeldman$elm_css$VirtualDom$Styled$text;
var $author$project$Main$selectCharButton = function (_char) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$li,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$button,
				A2(
					$elm$core$List$cons,
					$rtfeldman$elm_css$Html$Styled$Events$onClick(
						$author$project$Types$SelectCharacter(_char)),
					$author$project$Main$linkButtonStyle),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(_char)
					]))
			]));
};
var $rtfeldman$elm_css$Html$Styled$Attributes$type_ = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('type');
var $rtfeldman$elm_css$Html$Styled$ul = $rtfeldman$elm_css$Html$Styled$node('ul');
var $author$project$Main$characterSelectionPage = F2(
	function (_v0, _v1) {
		var characters = _v0.characters;
		var newCharacterName = _v0.newCharacterName;
		var focusedDropdownId = _v1.focusedDropdownId;
		var characterList = A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$ul,
					_List_Nil,
					A2($elm$core$List$map, $author$project$Main$selectCharButton, characters))
				]));
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h2,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Create a new character')
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$input,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
						$rtfeldman$elm_css$Html$Styled$Attributes$placeholder('New character name'),
						$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Types$NewCharacterName)
					]),
				_List_Nil),
				A2(
				$rtfeldman$elm_css$Html$Styled$button,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Types$CreateNewCharacter)
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Create')
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$h2,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('... or select an existing one')
					])),
				characterList
			]);
	});
var $rtfeldman$elm_css$Css$Structure$ClassSelector = function (a) {
	return {$: 'ClassSelector', a: a};
};
var $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence = function (a) {
	return {$: 'UniversalSelectorSequence', a: a};
};
var $rtfeldman$elm_css$Css$Structure$Selector = F3(
	function (a, b, c) {
		return {$: 'Selector', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Preprocess$Snippet = function (a) {
	return {$: 'Snippet', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$makeSnippet = F2(
	function (styles, sequence) {
		var selector = A3($rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, $elm$core$Maybe$Nothing);
		return $rtfeldman$elm_css$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
					A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, selector, _List_Nil, styles))
				]));
	});
var $rtfeldman$elm_css$Css$Global$class = F2(
	function (str, styles) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
			styles,
			$rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$Structure$ClassSelector(str)
					])));
	});
var $rtfeldman$elm_css$Css$Structure$Descendant = {$: 'Descendant'};
var $rtfeldman$elm_css$Css$Preprocess$NestSnippet = F2(
	function (a, b) {
		return {$: 'NestSnippet', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Global$descendants = $rtfeldman$elm_css$Css$Preprocess$NestSnippet($rtfeldman$elm_css$Css$Structure$Descendant);
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $rtfeldman$elm_css$Css$Structure$compactHelp = F2(
	function (declaration, _v0) {
		var keyframesByName = _v0.a;
		var declarations = _v0.b;
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var _v2 = declaration.a;
				var properties = _v2.c;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'MediaRule':
				var styleBlocks = declaration.b;
				return A2(
					$elm$core$List$all,
					function (_v3) {
						var properties = _v3.c;
						return $elm$core$List$isEmpty(properties);
					},
					styleBlocks) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'SupportsRule':
				var otherDeclarations = declaration.b;
				return $elm$core$List$isEmpty(otherDeclarations) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'DocumentRule':
				return _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'PageRule':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'FontFace':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'Keyframes':
				var record = declaration.a;
				return $elm$core$String$isEmpty(record.declaration) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					A3($elm$core$Dict$insert, record.name, record.declaration, keyframesByName),
					declarations);
			case 'Viewport':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'CounterStyle':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			default:
				var tuples = declaration.a;
				return A2(
					$elm$core$List$all,
					function (_v4) {
						var properties = _v4.b;
						return $elm$core$List$isEmpty(properties);
					},
					tuples) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
		}
	});
var $rtfeldman$elm_css$Css$Structure$Keyframes = function (a) {
	return {$: 'Keyframes', a: a};
};
var $rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations = F2(
	function (keyframesByName, compactedDeclarations) {
		return A2(
			$elm$core$List$append,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var decl = _v0.b;
					return $rtfeldman$elm_css$Css$Structure$Keyframes(
						{declaration: decl, name: name});
				},
				$elm$core$Dict$toList(keyframesByName)),
			compactedDeclarations);
	});
var $rtfeldman$elm_css$Css$Structure$compactDeclarations = function (declarations) {
	var _v0 = A3(
		$elm$core$List$foldr,
		$rtfeldman$elm_css$Css$Structure$compactHelp,
		_Utils_Tuple2($elm$core$Dict$empty, _List_Nil),
		declarations);
	var keyframesByName = _v0.a;
	var compactedDeclarations = _v0.b;
	return A2($rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations, keyframesByName, compactedDeclarations);
};
var $rtfeldman$elm_css$Css$Structure$compactStylesheet = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var declarations = _v0.declarations;
	return {
		charset: charset,
		declarations: $rtfeldman$elm_css$Css$Structure$compactDeclarations(declarations),
		imports: imports,
		namespaces: namespaces
	};
};
var $rtfeldman$elm_css$Css$Structure$Output$charsetToString = function (charset) {
	return A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function (str) {
				return '@charset \"' + (str + '\"');
			},
			charset));
};
var $rtfeldman$elm_css$Css$String$mapJoinHelp = F4(
	function (map, sep, strs, result) {
		mapJoinHelp:
		while (true) {
			if (!strs.b) {
				return result;
			} else {
				if (!strs.b.b) {
					var first = strs.a;
					return result + (map(first) + '');
				} else {
					var first = strs.a;
					var rest = strs.b;
					var $temp$map = map,
						$temp$sep = sep,
						$temp$strs = rest,
						$temp$result = result + (map(first) + (sep + ''));
					map = $temp$map;
					sep = $temp$sep;
					strs = $temp$strs;
					result = $temp$result;
					continue mapJoinHelp;
				}
			}
		}
	});
var $rtfeldman$elm_css$Css$String$mapJoin = F3(
	function (map, sep, strs) {
		return A4($rtfeldman$elm_css$Css$String$mapJoinHelp, map, sep, strs, '');
	});
var $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString = function (expression) {
	return '(' + (expression.feature + (A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			$elm$core$Basics$append(': '),
			expression.value)) + ')'));
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString = function (mediaType) {
	switch (mediaType.$) {
		case 'Print':
			return 'print';
		case 'Screen':
			return 'screen';
		default:
			return 'speech';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString = function (mediaQuery) {
	var prefixWith = F3(
		function (str, mediaType, expressions) {
			return str + (' ' + A2(
				$elm$core$String$join,
				' and ',
				A2(
					$elm$core$List$cons,
					$rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString(mediaType),
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions))));
		});
	switch (mediaQuery.$) {
		case 'AllQuery':
			var expressions = mediaQuery.a;
			return A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, ' and ', expressions);
		case 'OnlyQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'only', mediaType, expressions);
		case 'NotQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'not', mediaType, expressions);
		default:
			var str = mediaQuery.a;
			return str;
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString = F2(
	function (name, mediaQuery) {
		return '@import \"' + (name + ($rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString(mediaQuery) + '\"'));
	});
var $rtfeldman$elm_css$Css$Structure$Output$importToString = function (_v0) {
	var name = _v0.a;
	var mediaQueries = _v0.b;
	return A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		$rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString(name),
		'\n',
		mediaQueries);
};
var $rtfeldman$elm_css$Css$Structure$Output$namespaceToString = function (_v0) {
	var prefix = _v0.a;
	var str = _v0.b;
	return '@namespace ' + (prefix + ('\"' + (str + '\"')));
};
var $rtfeldman$elm_css$Css$Structure$Output$emitProperties = function (properties) {
	return A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		function (_v0) {
			var prop = _v0.a;
			return prop + ';';
		},
		'',
		properties);
};
var $elm$core$String$append = _String_append;
var $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString = function (_v0) {
	var str = _v0.a;
	return '::' + str;
};
var $rtfeldman$elm_css$Css$Structure$Output$combinatorToString = function (combinator) {
	switch (combinator.$) {
		case 'AdjacentSibling':
			return '+';
		case 'GeneralSibling':
			return '~';
		case 'Child':
			return '>';
		default:
			return '';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString = function (repeatableSimpleSelector) {
	switch (repeatableSimpleSelector.$) {
		case 'ClassSelector':
			var str = repeatableSimpleSelector.a;
			return '.' + str;
		case 'IdSelector':
			var str = repeatableSimpleSelector.a;
			return '#' + str;
		case 'PseudoClassSelector':
			var str = repeatableSimpleSelector.a;
			return ':' + str;
		default:
			var str = repeatableSimpleSelector.a;
			return '[' + (str + ']');
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString = function (simpleSelectorSequence) {
	switch (simpleSelectorSequence.$) {
		case 'TypeSelectorSequence':
			var str = simpleSelectorSequence.a.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return _Utils_ap(
				str,
				A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors));
		case 'UniversalSelectorSequence':
			var repeatableSimpleSelectors = simpleSelectorSequence.a;
			return $elm$core$List$isEmpty(repeatableSimpleSelectors) ? '*' : A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors);
		default:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return _Utils_ap(
				str,
				A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors));
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString = function (_v0) {
	var combinator = _v0.a;
	var sequence = _v0.b;
	return $rtfeldman$elm_css$Css$Structure$Output$combinatorToString(combinator) + (' ' + $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(sequence));
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorToString = function (_v0) {
	var simpleSelectorSequence = _v0.a;
	var chain = _v0.b;
	var pseudoElement = _v0.c;
	var segments = A2(
		$elm$core$List$cons,
		$rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(simpleSelectorSequence),
		A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString, chain));
	var pseudoElementsString = A2(
		$elm$core$Maybe$withDefault,
		'',
		A2($elm$core$Maybe$map, $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString, pseudoElement));
	return A2(
		$elm$core$String$append,
		A2($elm$core$String$join, ' ', segments),
		pseudoElementsString);
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock = function (_v0) {
	var firstSelector = _v0.a;
	var otherSelectors = _v0.b;
	var properties = _v0.c;
	var selectorStr = A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		$rtfeldman$elm_css$Css$Structure$Output$selectorToString,
		',',
		A2($elm$core$List$cons, firstSelector, otherSelectors));
	return selectorStr + ('{' + ($rtfeldman$elm_css$Css$Structure$Output$emitProperties(properties) + '}'));
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration = function (decl) {
	switch (decl.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = decl.a;
			return $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = decl.a;
			var styleBlocks = decl.b;
			var query = A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString, ', ', mediaQueries);
			var blocks = A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock, '\n', styleBlocks);
			return '@media ' + (query + ('{' + (blocks + '}')));
		case 'SupportsRule':
			return 'TODO';
		case 'DocumentRule':
			return 'TODO';
		case 'PageRule':
			return 'TODO';
		case 'FontFace':
			return 'TODO';
		case 'Keyframes':
			var name = decl.a.name;
			var declaration = decl.a.declaration;
			return '@keyframes ' + (name + ('{' + (declaration + '}')));
		case 'Viewport':
			return 'TODO';
		case 'CounterStyle':
			return 'TODO';
		default:
			return 'TODO';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrint = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var declarations = _v0.declarations;
	return $rtfeldman$elm_css$Css$Structure$Output$charsetToString(charset) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$importToString, '\n', imports) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$namespaceToString, '\n', namespaces) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration, '\n', declarations) + '')));
};
var $rtfeldman$elm_css$Css$Structure$CounterStyle = function (a) {
	return {$: 'CounterStyle', a: a};
};
var $rtfeldman$elm_css$Css$Structure$FontFace = function (a) {
	return {$: 'FontFace', a: a};
};
var $rtfeldman$elm_css$Css$Structure$PageRule = function (a) {
	return {$: 'PageRule', a: a};
};
var $rtfeldman$elm_css$Css$Structure$Property = function (a) {
	return {$: 'Property', a: a};
};
var $rtfeldman$elm_css$Css$Structure$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$Css$Structure$SupportsRule = F2(
	function (a, b) {
		return {$: 'SupportsRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$Viewport = function (a) {
	return {$: 'Viewport', a: a};
};
var $rtfeldman$elm_css$Css$Structure$MediaRule = F2(
	function (a, b) {
		return {$: 'MediaRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$mapLast = F2(
	function (update, list) {
		if (!list.b) {
			return list;
		} else {
			if (!list.b.b) {
				var only = list.a;
				return _List_fromArray(
					[
						update(only)
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$mapLast, update, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$withPropertyAppended = F2(
	function (property, _v0) {
		var firstSelector = _v0.a;
		var otherSelectors = _v0.b;
		var properties = _v0.c;
		return A3(
			$rtfeldman$elm_css$Css$Structure$StyleBlock,
			firstSelector,
			otherSelectors,
			_Utils_ap(
				properties,
				_List_fromArray(
					[property])));
	});
var $rtfeldman$elm_css$Css$Structure$appendProperty = F2(
	function (property, declarations) {
		if (!declarations.b) {
			return declarations;
		} else {
			if (!declarations.b.b) {
				switch (declarations.a.$) {
					case 'StyleBlockDeclaration':
						var styleBlock = declarations.a.a;
						return _List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
								A2($rtfeldman$elm_css$Css$Structure$withPropertyAppended, property, styleBlock))
							]);
					case 'MediaRule':
						var _v1 = declarations.a;
						var mediaQueries = _v1.a;
						var styleBlocks = _v1.b;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Structure$MediaRule,
								mediaQueries,
								A2(
									$rtfeldman$elm_css$Css$Structure$mapLast,
									$rtfeldman$elm_css$Css$Structure$withPropertyAppended(property),
									styleBlocks))
							]);
					default:
						return declarations;
				}
			} else {
				var first = declarations.a;
				var rest = declarations.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendToLastSelector = F2(
	function (f, styleBlock) {
		if (!styleBlock.b.b) {
			var only = styleBlock.a;
			var properties = styleBlock.c;
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, only, _List_Nil, properties),
					A3(
					$rtfeldman$elm_css$Css$Structure$StyleBlock,
					f(only),
					_List_Nil,
					_List_Nil)
				]);
		} else {
			var first = styleBlock.a;
			var rest = styleBlock.b;
			var properties = styleBlock.c;
			var newRest = A2($elm$core$List$map, f, rest);
			var newFirst = f(first);
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, rest, properties),
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, newFirst, newRest, _List_Nil)
				]);
		}
	});
var $rtfeldman$elm_css$Css$Structure$applyPseudoElement = F2(
	function (pseudo, _v0) {
		var sequence = _v0.a;
		var selectors = _v0.b;
		return A3(
			$rtfeldman$elm_css$Css$Structure$Selector,
			sequence,
			selectors,
			$elm$core$Maybe$Just(pseudo));
	});
var $rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector = F2(
	function (pseudo, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$applyPseudoElement(pseudo),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Structure$CustomSelector = F2(
	function (a, b) {
		return {$: 'CustomSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$TypeSelectorSequence = F2(
	function (a, b) {
		return {$: 'TypeSelectorSequence', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatable = F2(
	function (selector, sequence) {
		switch (sequence.$) {
			case 'TypeSelectorSequence':
				var typeSelector = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
					typeSelector,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			case 'UniversalSelectorSequence':
				var list = sequence.a;
				return $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			default:
				var str = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$CustomSelector,
					str,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator = F2(
	function (selector, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var _v1 = list.a;
				var combinator = _v1.a;
				var sequence = _v1.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						combinator,
						A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, selector, sequence))
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, selector, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableSelector = F2(
	function (repeatableSimpleSelector, selector) {
		if (!selector.b.b) {
			var sequence = selector.a;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, repeatableSimpleSelector, sequence),
				_List_Nil,
				pseudoElement);
		} else {
			var firstSelector = selector.a;
			var tuples = selector.b;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				firstSelector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, repeatableSimpleSelector, tuples),
				pseudoElement);
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector = F2(
	function (selector, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$appendRepeatableSelector(selector),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (declarations.a.$ === 'StyleBlockDeclaration') {
				var _v1 = declarations.a.a;
				var firstSelector = _v1.a;
				var otherSelectors = _v1.b;
				var rest = declarations.b;
				return _Utils_ap(
					A2($elm$core$List$cons, firstSelector, otherSelectors),
					$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$DocumentRule = F5(
	function (a, b, c, d, e) {
		return {$: 'DocumentRule', a: a, b: b, c: c, d: d, e: e};
	});
var $rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock = F2(
	function (update, declarations) {
		_v0$12:
		while (true) {
			if (!declarations.b) {
				return declarations;
			} else {
				if (!declarations.b.b) {
					switch (declarations.a.$) {
						case 'StyleBlockDeclaration':
							var styleBlock = declarations.a.a;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration,
								update(styleBlock));
						case 'MediaRule':
							if (declarations.a.b.b) {
								if (!declarations.a.b.b.b) {
									var _v1 = declarations.a;
									var mediaQueries = _v1.a;
									var _v2 = _v1.b;
									var styleBlock = _v2.a;
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Structure$MediaRule,
											mediaQueries,
											update(styleBlock))
										]);
								} else {
									var _v3 = declarations.a;
									var mediaQueries = _v3.a;
									var _v4 = _v3.b;
									var first = _v4.a;
									var rest = _v4.b;
									var _v5 = A2(
										$rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock,
										update,
										_List_fromArray(
											[
												A2($rtfeldman$elm_css$Css$Structure$MediaRule, mediaQueries, rest)
											]));
									if ((_v5.b && (_v5.a.$ === 'MediaRule')) && (!_v5.b.b)) {
										var _v6 = _v5.a;
										var newMediaQueries = _v6.a;
										var newStyleBlocks = _v6.b;
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Css$Structure$MediaRule,
												newMediaQueries,
												A2($elm$core$List$cons, first, newStyleBlocks))
											]);
									} else {
										var newDeclarations = _v5;
										return newDeclarations;
									}
								}
							} else {
								break _v0$12;
							}
						case 'SupportsRule':
							var _v7 = declarations.a;
							var str = _v7.a;
							var nestedDeclarations = _v7.b;
							return _List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$Structure$SupportsRule,
									str,
									A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, nestedDeclarations))
								]);
						case 'DocumentRule':
							var _v8 = declarations.a;
							var str1 = _v8.a;
							var str2 = _v8.b;
							var str3 = _v8.c;
							var str4 = _v8.d;
							var styleBlock = _v8.e;
							return A2(
								$elm$core$List$map,
								A4($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4),
								update(styleBlock));
						case 'PageRule':
							return declarations;
						case 'FontFace':
							return declarations;
						case 'Keyframes':
							return declarations;
						case 'Viewport':
							return declarations;
						case 'CounterStyle':
							return declarations;
						default:
							return declarations;
					}
				} else {
					break _v0$12;
				}
			}
		}
		var first = declarations.a;
		var rest = declarations.b;
		return A2(
			$elm$core$List$cons,
			first,
			A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, rest));
	});
var $elm$core$String$cons = _String_cons;
var $robinheghan$murmur3$Murmur3$HashData = F4(
	function (shift, seed, hash, charsProcessed) {
		return {charsProcessed: charsProcessed, hash: hash, seed: seed, shift: shift};
	});
var $robinheghan$murmur3$Murmur3$c1 = 3432918353;
var $robinheghan$murmur3$Murmur3$c2 = 461845907;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $robinheghan$murmur3$Murmur3$multiplyBy = F2(
	function (b, a) {
		return ((a & 65535) * b) + ((((a >>> 16) * b) & 65535) << 16);
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $robinheghan$murmur3$Murmur3$rotlBy = F2(
	function (b, a) {
		return (a << b) | (a >>> (32 - b));
	});
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $robinheghan$murmur3$Murmur3$finalize = function (data) {
	var acc = (!(!data.hash)) ? (data.seed ^ A2(
		$robinheghan$murmur3$Murmur3$multiplyBy,
		$robinheghan$murmur3$Murmur3$c2,
		A2(
			$robinheghan$murmur3$Murmur3$rotlBy,
			15,
			A2($robinheghan$murmur3$Murmur3$multiplyBy, $robinheghan$murmur3$Murmur3$c1, data.hash)))) : data.seed;
	var h0 = acc ^ data.charsProcessed;
	var h1 = A2($robinheghan$murmur3$Murmur3$multiplyBy, 2246822507, h0 ^ (h0 >>> 16));
	var h2 = A2($robinheghan$murmur3$Murmur3$multiplyBy, 3266489909, h1 ^ (h1 >>> 13));
	return (h2 ^ (h2 >>> 16)) >>> 0;
};
var $elm$core$String$foldl = _String_foldl;
var $robinheghan$murmur3$Murmur3$mix = F2(
	function (h1, k1) {
		return A2(
			$robinheghan$murmur3$Murmur3$multiplyBy,
			5,
			A2(
				$robinheghan$murmur3$Murmur3$rotlBy,
				13,
				h1 ^ A2(
					$robinheghan$murmur3$Murmur3$multiplyBy,
					$robinheghan$murmur3$Murmur3$c2,
					A2(
						$robinheghan$murmur3$Murmur3$rotlBy,
						15,
						A2($robinheghan$murmur3$Murmur3$multiplyBy, $robinheghan$murmur3$Murmur3$c1, k1))))) + 3864292196;
	});
var $robinheghan$murmur3$Murmur3$hashFold = F2(
	function (c, data) {
		var res = data.hash | ((255 & $elm$core$Char$toCode(c)) << data.shift);
		var _v0 = data.shift;
		if (_v0 === 24) {
			return {
				charsProcessed: data.charsProcessed + 1,
				hash: 0,
				seed: A2($robinheghan$murmur3$Murmur3$mix, data.seed, res),
				shift: 0
			};
		} else {
			return {charsProcessed: data.charsProcessed + 1, hash: res, seed: data.seed, shift: data.shift + 8};
		}
	});
var $robinheghan$murmur3$Murmur3$hashString = F2(
	function (seed, str) {
		return $robinheghan$murmur3$Murmur3$finalize(
			A3(
				$elm$core$String$foldl,
				$robinheghan$murmur3$Murmur3$hashFold,
				A4($robinheghan$murmur3$Murmur3$HashData, 0, seed, 0, 0),
				str));
	});
var $rtfeldman$elm_css$Hash$initialSeed = 15739;
var $elm$core$String$fromList = _String_fromList;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return _Utils_chr('0');
			case 1:
				return _Utils_chr('1');
			case 2:
				return _Utils_chr('2');
			case 3:
				return _Utils_chr('3');
			case 4:
				return _Utils_chr('4');
			case 5:
				return _Utils_chr('5');
			case 6:
				return _Utils_chr('6');
			case 7:
				return _Utils_chr('7');
			case 8:
				return _Utils_chr('8');
			case 9:
				return _Utils_chr('9');
			case 10:
				return _Utils_chr('a');
			case 11:
				return _Utils_chr('b');
			case 12:
				return _Utils_chr('c');
			case 13:
				return _Utils_chr('d');
			case 14:
				return _Utils_chr('e');
			case 15:
				return _Utils_chr('f');
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2($elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var $rtfeldman$elm_hex$Hex$toString = function (num) {
	return $elm$core$String$fromList(
		(num < 0) ? A2(
			$elm$core$List$cons,
			_Utils_chr('-'),
			A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var $rtfeldman$elm_css$Hash$fromString = function (str) {
	return A2(
		$elm$core$String$cons,
		_Utils_chr('_'),
		$rtfeldman$elm_hex$Hex$toString(
			A2($robinheghan$murmur3$Murmur3$hashString, $rtfeldman$elm_css$Hash$initialSeed, str)));
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$last = function (list) {
	last:
	while (true) {
		if (!list.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!list.b.b) {
				var singleton = list.a;
				return $elm$core$Maybe$Just(singleton);
			} else {
				var rest = list.b;
				var $temp$list = rest;
				list = $temp$list;
				continue last;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration = function (declarations) {
	lastDeclaration:
	while (true) {
		if (!declarations.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!declarations.b.b) {
				var x = declarations.a;
				return $elm$core$Maybe$Just(
					_List_fromArray(
						[x]));
			} else {
				var xs = declarations.b;
				var $temp$declarations = xs;
				declarations = $temp$declarations;
				continue lastDeclaration;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf = function (maybes) {
	oneOf:
	while (true) {
		if (!maybes.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var maybe = maybes.a;
			var rest = maybes.b;
			if (maybe.$ === 'Nothing') {
				var $temp$maybes = rest;
				maybes = $temp$maybes;
				continue oneOf;
			} else {
				return maybe;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$FontFeatureValues = function (a) {
	return {$: 'FontFeatureValues', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues = function (tuples) {
	var expandTuples = function (tuplesToExpand) {
		if (!tuplesToExpand.b) {
			return _List_Nil;
		} else {
			var properties = tuplesToExpand.a;
			var rest = tuplesToExpand.b;
			return A2(
				$elm$core$List$cons,
				properties,
				expandTuples(rest));
		}
	};
	var newTuples = expandTuples(tuples);
	return _List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$FontFeatureValues(newTuples)
		]);
};
var $rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule = F2(
	function (mediaQueries, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var styleBlock = declaration.a;
			return A2(
				$rtfeldman$elm_css$Css$Structure$MediaRule,
				mediaQueries,
				_List_fromArray(
					[styleBlock]));
		} else {
			return declaration;
		}
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule = F5(
	function (str1, str2, str3, str4, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var structureStyleBlock = declaration.a;
			return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
		} else {
			return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule = F2(
	function (mediaQueries, declaration) {
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var structureStyleBlock = declaration.a;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					mediaQueries,
					_List_fromArray(
						[structureStyleBlock]));
			case 'MediaRule':
				var newMediaQueries = declaration.a;
				var structureStyleBlocks = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					_Utils_ap(mediaQueries, newMediaQueries),
					structureStyleBlocks);
			case 'SupportsRule':
				var str = declaration.a;
				var declarations = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$SupportsRule,
					str,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
						declarations));
			case 'DocumentRule':
				var str1 = declaration.a;
				var str2 = declaration.b;
				var str3 = declaration.c;
				var str4 = declaration.d;
				var structureStyleBlock = declaration.e;
				return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
			case 'PageRule':
				return declaration;
			case 'FontFace':
				return declaration;
			case 'Keyframes':
				return declaration;
			case 'Viewport':
				return declaration;
			case 'CounterStyle':
				return declaration;
			default:
				return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet = function (_v0) {
	var declarations = _v0.a;
	return declarations;
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast = F4(
	function (nestedStyles, rest, f, declarations) {
		var withoutParent = function (decls) {
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail(decls));
		};
		var nextResult = A2(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
			rest,
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		var newDeclarations = function () {
			var _v14 = _Utils_Tuple2(
				$elm$core$List$head(nextResult),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$last(declarations));
			if ((_v14.a.$ === 'Just') && (_v14.b.$ === 'Just')) {
				var nextResultParent = _v14.a.a;
				var originalParent = _v14.b.a;
				return _Utils_ap(
					A2(
						$elm$core$List$take,
						$elm$core$List$length(declarations) - 1,
						declarations),
					_List_fromArray(
						[
							(!_Utils_eq(originalParent, nextResultParent)) ? nextResultParent : originalParent
						]));
			} else {
				return declarations;
			}
		}();
		var insertStylesToNestedDecl = function (lastDecl) {
			return $elm$core$List$concat(
				A2(
					$rtfeldman$elm_css$Css$Structure$mapLast,
					$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles(nestedStyles),
					A2(
						$elm$core$List$map,
						$elm$core$List$singleton,
						A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, f, lastDecl))));
		};
		var initialResult = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				insertStylesToNestedDecl,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		return _Utils_ap(
			newDeclarations,
			_Utils_ap(
				withoutParent(initialResult),
				withoutParent(nextResult)));
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles = F2(
	function (styles, declarations) {
		if (!styles.b) {
			return declarations;
		} else {
			switch (styles.a.$) {
				case 'AppendProperty':
					var property = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, declarations));
				case 'ExtendSelector':
					var _v4 = styles.a;
					var selector = _v4.a;
					var nestedStyles = _v4.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector(selector),
						declarations);
				case 'NestSnippet':
					var _v5 = styles.a;
					var selectorCombinator = _v5.a;
					var snippets = _v5.b;
					var rest = styles.b;
					var chain = F2(
						function (_v9, _v10) {
							var originalSequence = _v9.a;
							var originalTuples = _v9.b;
							var originalPseudoElement = _v9.c;
							var newSequence = _v10.a;
							var newTuples = _v10.b;
							var newPseudoElement = _v10.c;
							return A3(
								$rtfeldman$elm_css$Css$Structure$Selector,
								originalSequence,
								_Utils_ap(
									originalTuples,
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(selectorCombinator, newSequence),
										newTuples)),
								$rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf(
									_List_fromArray(
										[newPseudoElement, originalPseudoElement])));
						});
					var expandDeclaration = function (declaration) {
						switch (declaration.$) {
							case 'StyleBlockDeclaration':
								var _v7 = declaration.a;
								var firstSelector = _v7.a;
								var otherSelectors = _v7.b;
								var nestedStyles = _v7.c;
								var newSelectors = A2(
									$elm$core$List$concatMap,
									function (originalSelector) {
										return A2(
											$elm$core$List$map,
											chain(originalSelector),
											A2($elm$core$List$cons, firstSelector, otherSelectors));
									},
									$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations));
								var newDeclarations = function () {
									if (!newSelectors.b) {
										return _List_Nil;
									} else {
										var first = newSelectors.a;
										var remainder = newSelectors.b;
										return _List_fromArray(
											[
												$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
												A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, remainder, _List_Nil))
											]);
									}
								}();
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations);
							case 'MediaRule':
								var mediaQueries = declaration.a;
								var styleBlocks = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
							case 'SupportsRule':
								var str = declaration.a;
								var otherSnippets = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, otherSnippets);
							case 'DocumentRule':
								var str1 = declaration.a;
								var str2 = declaration.b;
								var str3 = declaration.c;
								var str4 = declaration.d;
								var styleBlock = declaration.e;
								return A2(
									$elm$core$List$map,
									A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
									$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
							case 'PageRule':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$PageRule(properties)
									]);
							case 'FontFace':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$FontFace(properties)
									]);
							case 'Viewport':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$Viewport(properties)
									]);
							case 'CounterStyle':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
									]);
							default:
								var tuples = declaration.a;
								return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
						}
					};
					return $elm$core$List$concat(
						_Utils_ap(
							_List_fromArray(
								[
									A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations)
								]),
							A2(
								$elm$core$List$map,
								expandDeclaration,
								A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets))));
				case 'WithPseudoElement':
					var _v11 = styles.a;
					var pseudoElement = _v11.a;
					var nestedStyles = _v11.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector(pseudoElement),
						declarations);
				case 'WithKeyframes':
					var str = styles.a.a;
					var rest = styles.b;
					var name = $rtfeldman$elm_css$Hash$fromString(str);
					var newProperty = $rtfeldman$elm_css$Css$Structure$Property('animation-name:' + name);
					var newDeclarations = A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, newProperty, declarations));
					return A2(
						$elm$core$List$append,
						newDeclarations,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$Keyframes(
								{declaration: str, name: name})
							]));
				case 'WithMedia':
					var _v12 = styles.a;
					var mediaQueries = _v12.a;
					var nestedStyles = _v12.b;
					var rest = styles.b;
					var extraDeclarations = function () {
						var _v13 = $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations);
						if (!_v13.b) {
							return _List_Nil;
						} else {
							var firstSelector = _v13.a;
							var otherSelectors = _v13.b;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule(mediaQueries),
								A2(
									$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
									nestedStyles,
									$elm$core$List$singleton(
										$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
											A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil)))));
						}
					}();
					return _Utils_ap(
						A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations),
						extraDeclarations);
				default:
					var otherStyles = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						_Utils_ap(otherStyles, rest),
						declarations);
			}
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock = function (_v2) {
	var firstSelector = _v2.a;
	var otherSelectors = _v2.b;
	var styles = _v2.c;
	return A2(
		$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
		styles,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
				A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil))
			]));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$extract = function (snippetDeclarations) {
	if (!snippetDeclarations.b) {
		return _List_Nil;
	} else {
		var first = snippetDeclarations.a;
		var rest = snippetDeclarations.b;
		return _Utils_ap(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations(first),
			$rtfeldman$elm_css$Css$Preprocess$Resolve$extract(rest));
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule = F2(
	function (mediaQueries, styleBlocks) {
		var handleStyleBlock = function (styleBlock) {
			return A2(
				$elm$core$List$map,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		};
		return A2($elm$core$List$concatMap, handleStyleBlock, styleBlocks);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule = F2(
	function (str, snippets) {
		var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
			A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
		return _List_fromArray(
			[
				A2($rtfeldman$elm_css$Css$Structure$SupportsRule, str, declarations)
			]);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations = function (snippetDeclaration) {
	switch (snippetDeclaration.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = snippetDeclaration.a;
			var styleBlocks = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
		case 'SupportsRule':
			var str = snippetDeclaration.a;
			var snippets = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
		case 'DocumentRule':
			var str1 = snippetDeclaration.a;
			var str2 = snippetDeclaration.b;
			var str3 = snippetDeclaration.c;
			var str4 = snippetDeclaration.d;
			var styleBlock = snippetDeclaration.e;
			return A2(
				$elm$core$List$map,
				A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		case 'PageRule':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$PageRule(properties)
				]);
		case 'FontFace':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$FontFace(properties)
				]);
		case 'Viewport':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$Viewport(properties)
				]);
		case 'CounterStyle':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
				]);
		default:
			var tuples = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var snippets = _v0.snippets;
	var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
		A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
	return {charset: charset, declarations: declarations, imports: imports, namespaces: namespaces};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$compile = function (sheet) {
	return $rtfeldman$elm_css$Css$Structure$Output$prettyPrint(
		$rtfeldman$elm_css$Css$Structure$compactStylesheet(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure(sheet)));
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $rtfeldman$elm_css$Css$Preprocess$stylesheet = function (snippets) {
	return {charset: $elm$core$Maybe$Nothing, imports: _List_Nil, namespaces: _List_Nil, snippets: snippets};
};
var $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode = $rtfeldman$elm_css$VirtualDom$Styled$Unstyled;
var $rtfeldman$elm_css$Css$Global$global = function (snippets) {
	return $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode(
		A3(
			$elm$virtual_dom$VirtualDom$node,
			'span',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'style', 'display: none;'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'class', 'elm-css-style-wrapper')
				]),
			$elm$core$List$singleton(
				A3(
					$elm$virtual_dom$VirtualDom$node,
					'style',
					_List_Nil,
					$elm$core$List$singleton(
						$elm$virtual_dom$VirtualDom$text(
							$rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
								$rtfeldman$elm_css$Css$Preprocess$stylesheet(snippets))))))));
};
var $rtfeldman$elm_css$Css$Preprocess$AppendProperty = function (a) {
	return {$: 'AppendProperty', a: a};
};
var $rtfeldman$elm_css$Css$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(
			$rtfeldman$elm_css$Css$Structure$Property(key + (':' + value)));
	});
var $rtfeldman$elm_css$Css$prop1 = F2(
	function (key, arg) {
		return A2($rtfeldman$elm_css$Css$property, key, arg.value);
	});
var $rtfeldman$elm_css$Css$marginBottom = $rtfeldman$elm_css$Css$prop1('margin-bottom');
var $rtfeldman$elm_css$Css$marginTop = $rtfeldman$elm_css$Css$prop1('margin-top');
var $rtfeldman$elm_css$Css$paddingBottom = $rtfeldman$elm_css$Css$prop1('padding-bottom');
var $rtfeldman$elm_css$Css$paddingLeft = $rtfeldman$elm_css$Css$prop1('padding-left');
var $rtfeldman$elm_css$Css$paddingTop = $rtfeldman$elm_css$Css$prop1('padding-top');
var $rtfeldman$elm_css$Css$PxUnits = {$: 'PxUnits'};
var $rtfeldman$elm_css$Css$Structure$Compatible = {$: 'Compatible'};
var $elm$core$String$fromFloat = _String_fromNumber;
var $rtfeldman$elm_css$Css$Internal$lengthConverter = F3(
	function (units, unitLabel, numericValue) {
		return {
			absoluteLength: $rtfeldman$elm_css$Css$Structure$Compatible,
			calc: $rtfeldman$elm_css$Css$Structure$Compatible,
			flexBasis: $rtfeldman$elm_css$Css$Structure$Compatible,
			fontSize: $rtfeldman$elm_css$Css$Structure$Compatible,
			length: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
			lineHeight: $rtfeldman$elm_css$Css$Structure$Compatible,
			numericValue: numericValue,
			textIndent: $rtfeldman$elm_css$Css$Structure$Compatible,
			unitLabel: unitLabel,
			units: units,
			value: _Utils_ap(
				$elm$core$String$fromFloat(numericValue),
				unitLabel)
		};
	});
var $rtfeldman$elm_css$Css$px = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PxUnits, 'px');
var $rtfeldman$elm_css$Css$Structure$TypeSelector = function (a) {
	return {$: 'TypeSelector', a: a};
};
var $rtfeldman$elm_css$Css$Global$typeSelector = F2(
	function (selectorStr, styles) {
		var sequence = A2(
			$rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
			$rtfeldman$elm_css$Css$Structure$TypeSelector(selectorStr),
			_List_Nil);
		var sel = A3($rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, $elm$core$Maybe$Nothing);
		return $rtfeldman$elm_css$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
					A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, sel, _List_Nil, styles))
				]));
	});
var $rtfeldman$elm_css$Css$UnitlessInteger = {$: 'UnitlessInteger'};
var $rtfeldman$elm_css$Css$zero = {length: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible, number: $rtfeldman$elm_css$Css$Structure$Compatible, numericValue: 0, outline: $rtfeldman$elm_css$Css$Structure$Compatible, unitLabel: '', units: $rtfeldman$elm_css$Css$UnitlessInteger, value: '0'};
var $author$project$Main$globalCss = $rtfeldman$elm_css$Css$Global$global(
	_List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Css$Global$class,
			'card-description',
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Global$descendants(
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Css$Global$typeSelector,
							'p',
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$marginBottom(
									$rtfeldman$elm_css$Css$px(2)),
									$rtfeldman$elm_css$Css$marginTop($rtfeldman$elm_css$Css$zero)
								])),
							A2(
							$rtfeldman$elm_css$Css$Global$typeSelector,
							'ul',
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$paddingLeft(
									$rtfeldman$elm_css$Css$px(10))
								])),
							A2(
							$rtfeldman$elm_css$Css$Global$typeSelector,
							'td',
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$marginBottom($rtfeldman$elm_css$Css$zero),
									$rtfeldman$elm_css$Css$marginTop($rtfeldman$elm_css$Css$zero),
									$rtfeldman$elm_css$Css$paddingTop($rtfeldman$elm_css$Css$zero),
									$rtfeldman$elm_css$Css$paddingBottom($rtfeldman$elm_css$Css$zero)
								]))
						]))
				]))
		]));
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles = function (a) {
	return {$: 'UnscopedStyles', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles = F2(
	function (_v0, styles) {
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				return styles;
			} else {
				return A3(
					$elm$core$Dict$insert,
					cssTemplate,
					$rtfeldman$elm_css$Hash$fromString(cssTemplate),
					styles);
			}
		} else {
			return styles;
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute = F2(
	function (styles, _v0) {
		var val = _v0.a;
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				var classname = _v1.a;
				return A2(
					$elm$virtual_dom$VirtualDom$property,
					'className',
					$elm$json$Json$Encode$string(classname));
			} else {
				return A2(
					$elm$virtual_dom$VirtualDom$property,
					'className',
					$elm$json$Json$Encode$string('_unstyled'));
			}
		} else {
			return val;
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS = F2(
	function (styles, _v0) {
		var val = _v0.a;
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				var classname = _v1.a;
				return A2($elm$virtual_dom$VirtualDom$attribute, 'class', classname);
			} else {
				return A2($elm$virtual_dom$VirtualDom$attribute, 'class', '_unstyled');
			}
		} else {
			return val;
		}
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$keyedNodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_keyedNodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $elm$virtual_dom$VirtualDom$nodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_nodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml = F2(
	function (_v6, _v7) {
		var key = _v6.a;
		var html = _v6.b;
		var pairs = _v7.a;
		var styles = _v7.b;
		switch (html.$) {
			case 'Unstyled':
				var vdom = html.a;
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v9 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v9.a;
				var finalStyles = _v9.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v10 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v10.a;
				var finalStyles = _v10.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v11 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v11.a;
				var finalStyles = _v11.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v12 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v12.a;
				var finalStyles = _v12.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml = F2(
	function (html, _v0) {
		var nodes = _v0.a;
		var styles = _v0.b;
		switch (html.$) {
			case 'Unstyled':
				var vdomNode = html.a;
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v2 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v2.a;
				var finalStyles = _v2.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v3 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v3.a;
				var finalStyles = _v3.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v4 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v4.a;
				var finalStyles = _v4.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v5 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v5.a;
				var finalStyles = _v5.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin = '\u0007';
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$styleToDeclaration = F3(
	function (template, classname, declaration) {
		return declaration + ('\n' + A3($elm$core$String$replace, $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin, classname, template));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toDeclaration = function (dict) {
	return A3($elm$core$Dict$foldl, $rtfeldman$elm_css$VirtualDom$Styled$styleToDeclaration, '', dict);
};
var $rtfeldman$elm_css$VirtualDom$Styled$toScopedDeclaration = F2(
	function (scopingPrefix, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (template, classname, declaration) {
					return declaration + ('\n' + A3($elm$core$String$replace, '.' + $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin, '#' + (scopingPrefix + ('.' + classname)), template));
				}),
			'',
			dict);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode = F2(
	function (maybeNonce, accumulatedStyles) {
		var cssText = function () {
			if (accumulatedStyles.$ === 'UnscopedStyles') {
				var allStyles = accumulatedStyles.a;
				return $rtfeldman$elm_css$VirtualDom$Styled$toDeclaration(allStyles);
			} else {
				var scope = accumulatedStyles.a.a;
				var rootStyles = accumulatedStyles.b;
				var descendantStyles = accumulatedStyles.c;
				return A2($rtfeldman$elm_css$VirtualDom$Styled$toScopedDeclaration, scope, rootStyles) + ('\n' + A2($rtfeldman$elm_css$VirtualDom$Styled$toScopedDeclaration, scope + ' ', descendantStyles));
			}
		}();
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			'span',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'style', 'display: none;'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'class', 'elm-css-style-wrapper')
				]),
			_List_fromArray(
				[
					A3(
					$elm$virtual_dom$VirtualDom$node,
					'style',
					function () {
						if (maybeNonce.$ === 'Just') {
							var nonce = maybeNonce.a.a;
							return _List_fromArray(
								[
									A2($elm$virtual_dom$VirtualDom$attribute, 'nonce', nonce)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					$elm$core$List$singleton(
						$elm$virtual_dom$VirtualDom$text(cssText)))
				]));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyle = F4(
	function (maybeNonce, elemType, properties, children) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = A2(
			$rtfeldman$elm_css$VirtualDom$Styled$toStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles));
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(styles),
			properties);
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$containsKey = F2(
	function (key, pairs) {
		containsKey:
		while (true) {
			if (!pairs.b) {
				return false;
			} else {
				var _v1 = pairs.a;
				var str = _v1.a;
				var rest = pairs.b;
				if (_Utils_eq(key, str)) {
					return true;
				} else {
					var $temp$key = key,
						$temp$pairs = rest;
					key = $temp$key;
					pairs = $temp$pairs;
					continue containsKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey = F2(
	function (_default, pairs) {
		getUnusedKey:
		while (true) {
			if (!pairs.b) {
				return _default;
			} else {
				var _v1 = pairs.a;
				var firstKey = _v1.a;
				var rest = pairs.b;
				var newKey = '_' + firstKey;
				if (A2($rtfeldman$elm_css$VirtualDom$Styled$containsKey, newKey, rest)) {
					var $temp$default = newKey,
						$temp$pairs = rest;
					_default = $temp$default;
					pairs = $temp$pairs;
					continue getUnusedKey;
				} else {
					return newKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode = F3(
	function (maybeNonce, accumulatedStyles, keyedChildNodes) {
		var styleNodeKey = A2($rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey, '_', keyedChildNodes);
		var finalNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toStyleNode, maybeNonce, accumulatedStyles);
		return _Utils_Tuple2(styleNodeKey, finalNode);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed = F4(
	function (maybeNonce, elemType, properties, keyedChildren) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A3(
			$rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles),
			keyedChildNodes);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(styles),
			properties);
		return A3(
			$elm$virtual_dom$VirtualDom$keyedNode,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS = F5(
	function (maybeNonce, ns, elemType, properties, keyedChildren) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A3(
			$rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles),
			keyedChildNodes);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(styles),
			properties);
		return A4(
			$elm$virtual_dom$VirtualDom$keyedNodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleNS = F5(
	function (maybeNonce, ns, elemType, properties, children) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = A2(
			$rtfeldman$elm_css$VirtualDom$Styled$toStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles));
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(styles),
			properties);
		return A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled = function (vdom) {
	switch (vdom.$) {
		case 'Unstyled':
			var plainNode = vdom.a;
			return plainNode;
		case 'Node':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyle, $elm$core$Maybe$Nothing, elemType, properties, children);
		case 'NodeNS':
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A5($rtfeldman$elm_css$VirtualDom$Styled$unstyleNS, $elm$core$Maybe$Nothing, ns, elemType, properties, children);
		case 'KeyedNode':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed, $elm$core$Maybe$Nothing, elemType, properties, children);
		default:
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A5($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS, $elm$core$Maybe$Nothing, ns, elemType, properties, children);
	}
};
var $rtfeldman$elm_css$Html$Styled$toUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled;
var $rtfeldman$elm_css$Css$displayFlex = A2($rtfeldman$elm_css$Css$property, 'display', 'flex');
var $rtfeldman$elm_css$Css$flexDirection = $rtfeldman$elm_css$Css$prop1('flex-direction');
var $rtfeldman$elm_css$Css$flexWrap = $rtfeldman$elm_css$Css$prop1('flex-wrap');
var $rtfeldman$elm_css$Css$row = {flexDirection: $rtfeldman$elm_css$Css$Structure$Compatible, flexDirectionOrWrap: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'row'};
var $rtfeldman$elm_css$Css$wrap = {flexDirectionOrWrap: $rtfeldman$elm_css$Css$Structure$Compatible, flexWrap: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'wrap'};
var $author$project$Page$CardsPage$cardsStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$displayFlex,
		$rtfeldman$elm_css$Css$flexDirection($rtfeldman$elm_css$Css$row),
		$rtfeldman$elm_css$Css$flexWrap($rtfeldman$elm_css$Css$wrap),
		A2($rtfeldman$elm_css$Css$property, 'break-inside', 'avoid')
	]);
var $author$project$Util$split = F2(
	function (n, list) {
		var _v0 = _Utils_Tuple2(n, list);
		if (!_v0.b.b) {
			return _Utils_Tuple2(_List_Nil, _List_Nil);
		} else {
			if (!_v0.a) {
				return _Utils_Tuple2(_List_Nil, list);
			} else {
				var _v1 = _v0.b;
				var x = _v1.a;
				var xs = _v1.b;
				var _v2 = A2($author$project$Util$split, n - 1, xs);
				var ys = _v2.a;
				var zs = _v2.b;
				return _Utils_Tuple2(
					A2($elm$core$List$cons, x, ys),
					zs);
			}
		}
	});
var $author$project$Util$chunks = F2(
	function (chunkSize, list) {
		var _v0 = A2($author$project$Util$split, chunkSize, list);
		if (!_v0.b.b) {
			var hd = _v0.a;
			return _List_fromArray(
				[hd]);
		} else {
			var hd = _v0.a;
			var tl = _v0.b;
			return A2(
				$elm$core$List$cons,
				hd,
				A2($author$project$Util$chunks, chunkSize, tl));
		}
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$class = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('className');
var $rtfeldman$elm_css$VirtualDom$Styled$templateSelector = $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
	_List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$ClassSelector($rtfeldman$elm_css$VirtualDom$Styled$classnameStandin)
		]));
var $rtfeldman$elm_css$VirtualDom$Styled$getCssTemplate = function (styles) {
	if (!styles.b) {
		return '';
	} else {
		var otherwise = styles;
		return $rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
			$rtfeldman$elm_css$Css$Preprocess$stylesheet(
				_List_fromArray(
					[
						A2($rtfeldman$elm_css$VirtualDom$Styled$makeSnippet, styles, $rtfeldman$elm_css$VirtualDom$Styled$templateSelector)
					])));
	}
};
var $rtfeldman$elm_css$Html$Styled$Internal$css = function (styles) {
	var cssTemplate = $rtfeldman$elm_css$VirtualDom$Styled$getCssTemplate(styles);
	var classProperty = A2($elm$virtual_dom$VirtualDom$attribute, '', '');
	return A3($rtfeldman$elm_css$VirtualDom$Styled$Attribute, classProperty, true, cssTemplate);
};
var $rtfeldman$elm_css$Html$Styled$Attributes$css = $rtfeldman$elm_css$Html$Styled$Internal$css;
var $author$project$Types$EditCharacter = {$: 'EditCharacter'};
var $author$project$Elements$BottomLeft = {$: 'BottomLeft'};
var $rtfeldman$elm_css$Html$Styled$img = $rtfeldman$elm_css$Html$Styled$node('img');
var $rtfeldman$elm_css$Css$border = $rtfeldman$elm_css$Css$prop1('border');
var $rtfeldman$elm_css$Css$center = $rtfeldman$elm_css$Css$prop1('center');
var $rtfeldman$elm_css$Css$cursor = $rtfeldman$elm_css$Css$prop1('cursor');
var $rtfeldman$elm_css$Css$fontSize = $rtfeldman$elm_css$Css$prop1('font-size');
var $rtfeldman$elm_css$Css$height = $rtfeldman$elm_css$Css$prop1('height');
var $rtfeldman$elm_css$Css$lineHeight = $rtfeldman$elm_css$Css$prop1('line-height');
var $rtfeldman$elm_css$Css$MMUnits = {$: 'MMUnits'};
var $rtfeldman$elm_css$Css$mm = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$MMUnits, 'mm');
var $rtfeldman$elm_css$Css$pointer = {cursor: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'pointer'};
var $rtfeldman$elm_css$Css$PtUnits = {$: 'PtUnits'};
var $rtfeldman$elm_css$Css$pt = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PtUnits, 'pt');
var $rtfeldman$elm_css$Css$Preprocess$ApplyStyles = function (a) {
	return {$: 'ApplyStyles', a: a};
};
var $rtfeldman$elm_css$Css$Internal$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(
			$rtfeldman$elm_css$Css$Structure$Property(key + (':' + value)));
	});
var $rtfeldman$elm_css$Css$Internal$getOverloadedProperty = F3(
	function (functionName, desiredKey, style) {
		getOverloadedProperty:
		while (true) {
			switch (style.$) {
				case 'AppendProperty':
					var str = style.a.a;
					var key = A2(
						$elm$core$Maybe$withDefault,
						'',
						$elm$core$List$head(
							A2($elm$core$String$split, ':', str)));
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, key);
				case 'ExtendSelector':
					var selector = style.a;
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-selector'));
				case 'NestSnippet':
					var combinator = style.a;
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-combinator'));
				case 'WithPseudoElement':
					var pseudoElement = style.a;
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-pseudo-element setter'));
				case 'WithMedia':
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-media-query'));
				case 'WithKeyframes':
					return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-inapplicable-Style-for-keyframes'));
				default:
					if (!style.a.b) {
						return A2($rtfeldman$elm_css$Css$Internal$property, desiredKey, 'elm-css-error-cannot-apply-' + (functionName + '-with-empty-Style'));
					} else {
						if (!style.a.b.b) {
							var _v1 = style.a;
							var only = _v1.a;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = only;
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						} else {
							var _v2 = style.a;
							var first = _v2.a;
							var rest = _v2.b;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = $rtfeldman$elm_css$Css$Preprocess$ApplyStyles(rest);
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						}
					}
			}
		}
	});
var $rtfeldman$elm_css$Css$Internal$IncompatibleUnits = {$: 'IncompatibleUnits'};
var $rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty = A3($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$Internal$IncompatibleUnits, '', 0);
var $rtfeldman$elm_css$Css$textAlign = function (fn) {
	return A3(
		$rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'textAlign',
		'text-align',
		fn($rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var $rtfeldman$elm_css$Css$width = $rtfeldman$elm_css$Css$prop1('width');
var $author$project$Elements$navButtonStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
		$rtfeldman$elm_css$Css$fontSize(
		$rtfeldman$elm_css$Css$pt(12)),
		$rtfeldman$elm_css$Css$lineHeight(
		$rtfeldman$elm_css$Css$pt(12)),
		$rtfeldman$elm_css$Css$height(
		$rtfeldman$elm_css$Css$mm(6)),
		$rtfeldman$elm_css$Css$width(
		$rtfeldman$elm_css$Css$mm(6)),
		$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer)
	]);
var $rtfeldman$elm_css$Html$Styled$Attributes$src = function (url) {
	return A2($rtfeldman$elm_css$Html$Styled$Attributes$stringProperty, 'src', url);
};
var $rtfeldman$elm_css$Css$absolute = {position: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'absolute'};
var $rtfeldman$elm_css$Css$backgroundColor = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'background-color', c.value);
};
var $rtfeldman$elm_css$Css$prop3 = F4(
	function (key, argA, argB, argC) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + argC.value))));
	});
var $rtfeldman$elm_css$Css$borderBottom3 = $rtfeldman$elm_css$Css$prop3('border-bottom');
var $rtfeldman$elm_css$Css$borderRadius = $rtfeldman$elm_css$Css$prop1('border-radius');
var $rtfeldman$elm_css$Css$color = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'color', c.value);
};
var $rtfeldman$elm_css$Css$display = $rtfeldman$elm_css$Css$prop1('display');
var $rtfeldman$elm_css$Css$dotted = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationStyle: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'dotted'};
var $rtfeldman$elm_css$Css$stringsToValue = function (list) {
	return $elm$core$List$isEmpty(list) ? {value: 'none'} : {
		value: A2($elm$core$String$join, ', ', list)
	};
};
var $rtfeldman$elm_css$Css$fontFamilies = A2(
	$elm$core$Basics$composeL,
	$rtfeldman$elm_css$Css$prop1('font-family'),
	$rtfeldman$elm_css$Css$stringsToValue);
var $rtfeldman$elm_css$Css$withPrecedingHash = function (str) {
	return A2($elm$core$String$startsWith, '#', str) ? str : A2(
		$elm$core$String$cons,
		_Utils_chr('#'),
		str);
};
var $rtfeldman$elm_css$Css$erroneousHex = function (str) {
	return {
		alpha: 1,
		blue: 0,
		color: $rtfeldman$elm_css$Css$Structure$Compatible,
		green: 0,
		red: 0,
		value: $rtfeldman$elm_css$Css$withPrecedingHash(str)
	};
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Basics$pow = _Basics_pow;
var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return $elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2($elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return $elm$core$Result$Err(
							$elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var $rtfeldman$elm_hex$Hex$fromString = function (str) {
	if ($elm$core$String$isEmpty(str)) {
		return $elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2($elm$core$String$startsWith, '-', str)) {
				var list = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$tail(
						$elm$core$String$toList(str)));
				return A2(
					$elm$core$Result$map,
					$elm$core$Basics$negate,
					A3(
						$rtfeldman$elm_hex$Hex$fromStringHelp,
						$elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					$rtfeldman$elm_hex$Hex$fromStringHelp,
					$elm$core$String$length(str) - 1,
					$elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2($elm$core$Result$mapError, formatError, result);
	}
};
var $elm$core$String$toLower = _String_toLower;
var $rtfeldman$elm_css$Css$validHex = F5(
	function (str, _v0, _v1, _v2, _v3) {
		var r1 = _v0.a;
		var r2 = _v0.b;
		var g1 = _v1.a;
		var g2 = _v1.b;
		var b1 = _v2.a;
		var b2 = _v2.b;
		var a1 = _v3.a;
		var a2 = _v3.b;
		var toResult = A2(
			$elm$core$Basics$composeR,
			$elm$core$String$fromList,
			A2($elm$core$Basics$composeR, $elm$core$String$toLower, $rtfeldman$elm_hex$Hex$fromString));
		var results = _Utils_Tuple2(
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[r1, r2])),
				toResult(
					_List_fromArray(
						[g1, g2]))),
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[b1, b2])),
				toResult(
					_List_fromArray(
						[a1, a2]))));
		if ((((results.a.a.$ === 'Ok') && (results.a.b.$ === 'Ok')) && (results.b.a.$ === 'Ok')) && (results.b.b.$ === 'Ok')) {
			var _v5 = results.a;
			var red = _v5.a.a;
			var green = _v5.b.a;
			var _v6 = results.b;
			var blue = _v6.a.a;
			var alpha = _v6.b.a;
			return {
				alpha: alpha / 255,
				blue: blue,
				color: $rtfeldman$elm_css$Css$Structure$Compatible,
				green: green,
				red: red,
				value: $rtfeldman$elm_css$Css$withPrecedingHash(str)
			};
		} else {
			return $rtfeldman$elm_css$Css$erroneousHex(str);
		}
	});
var $rtfeldman$elm_css$Css$hex = function (str) {
	var withoutHash = A2($elm$core$String$startsWith, '#', str) ? A2($elm$core$String$dropLeft, 1, str) : str;
	var _v0 = $elm$core$String$toList(withoutHash);
	_v0$4:
	while (true) {
		if ((_v0.b && _v0.b.b) && _v0.b.b.b) {
			if (!_v0.b.b.b.b) {
				var r = _v0.a;
				var _v1 = _v0.b;
				var g = _v1.a;
				var _v2 = _v1.b;
				var b = _v2.a;
				return A5(
					$rtfeldman$elm_css$Css$validHex,
					str,
					_Utils_Tuple2(r, r),
					_Utils_Tuple2(g, g),
					_Utils_Tuple2(b, b),
					_Utils_Tuple2(
						_Utils_chr('f'),
						_Utils_chr('f')));
			} else {
				if (!_v0.b.b.b.b.b) {
					var r = _v0.a;
					var _v3 = _v0.b;
					var g = _v3.a;
					var _v4 = _v3.b;
					var b = _v4.a;
					var _v5 = _v4.b;
					var a = _v5.a;
					return A5(
						$rtfeldman$elm_css$Css$validHex,
						str,
						_Utils_Tuple2(r, r),
						_Utils_Tuple2(g, g),
						_Utils_Tuple2(b, b),
						_Utils_Tuple2(a, a));
				} else {
					if (_v0.b.b.b.b.b.b) {
						if (!_v0.b.b.b.b.b.b.b) {
							var r1 = _v0.a;
							var _v6 = _v0.b;
							var r2 = _v6.a;
							var _v7 = _v6.b;
							var g1 = _v7.a;
							var _v8 = _v7.b;
							var g2 = _v8.a;
							var _v9 = _v8.b;
							var b1 = _v9.a;
							var _v10 = _v9.b;
							var b2 = _v10.a;
							return A5(
								$rtfeldman$elm_css$Css$validHex,
								str,
								_Utils_Tuple2(r1, r2),
								_Utils_Tuple2(g1, g2),
								_Utils_Tuple2(b1, b2),
								_Utils_Tuple2(
									_Utils_chr('f'),
									_Utils_chr('f')));
						} else {
							if (_v0.b.b.b.b.b.b.b.b && (!_v0.b.b.b.b.b.b.b.b.b)) {
								var r1 = _v0.a;
								var _v11 = _v0.b;
								var r2 = _v11.a;
								var _v12 = _v11.b;
								var g1 = _v12.a;
								var _v13 = _v12.b;
								var g2 = _v13.a;
								var _v14 = _v13.b;
								var b1 = _v14.a;
								var _v15 = _v14.b;
								var b2 = _v15.a;
								var _v16 = _v15.b;
								var a1 = _v16.a;
								var _v17 = _v16.b;
								var a2 = _v17.a;
								return A5(
									$rtfeldman$elm_css$Css$validHex,
									str,
									_Utils_Tuple2(r1, r2),
									_Utils_Tuple2(g1, g2),
									_Utils_Tuple2(b1, b2),
									_Utils_Tuple2(a1, a2));
							} else {
								break _v0$4;
							}
						}
					} else {
						break _v0$4;
					}
				}
			}
		} else {
			break _v0$4;
		}
	}
	return $rtfeldman$elm_css$Css$erroneousHex(str);
};
var $rtfeldman$elm_css$Css$hidden = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, overflow: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'hidden', visibility: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$Preprocess$ExtendSelector = F2(
	function (a, b) {
		return {$: 'ExtendSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$PseudoClassSelector = function (a) {
	return {$: 'PseudoClassSelector', a: a};
};
var $rtfeldman$elm_css$Css$pseudoClass = function (_class) {
	return $rtfeldman$elm_css$Css$Preprocess$ExtendSelector(
		$rtfeldman$elm_css$Css$Structure$PseudoClassSelector(_class));
};
var $rtfeldman$elm_css$Css$hover = $rtfeldman$elm_css$Css$pseudoClass('hover');
var $rtfeldman$elm_css$Css$inlineBlock = {display: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'inline-block'};
var $rtfeldman$elm_css$Css$int = function (val) {
	return {
		fontWeight: $rtfeldman$elm_css$Css$Structure$Compatible,
		intOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
		number: $rtfeldman$elm_css$Css$Structure$Compatible,
		numberOrInfinite: $rtfeldman$elm_css$Css$Structure$Compatible,
		numericValue: val,
		unitLabel: '',
		units: $rtfeldman$elm_css$Css$UnitlessInteger,
		value: $elm$core$String$fromInt(val)
	};
};
var $rtfeldman$elm_css$Css$left = $rtfeldman$elm_css$Css$prop1('left');
var $rtfeldman$elm_css$Css$prop2 = F3(
	function (key, argA, argB) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + argB.value));
	});
var $rtfeldman$elm_css$Css$padding2 = $rtfeldman$elm_css$Css$prop2('padding');
var $rtfeldman$elm_css$Css$PercentageUnits = {$: 'PercentageUnits'};
var $rtfeldman$elm_css$Css$pct = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PercentageUnits, '%');
var $rtfeldman$elm_css$Css$position = $rtfeldman$elm_css$Css$prop1('position');
var $rtfeldman$elm_css$Css$relative = {position: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'relative'};
var $rtfeldman$elm_css$Css$right = $rtfeldman$elm_css$Css$prop1('right');
var $rtfeldman$elm_css$Css$sansSerif = {fontFamily: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'sans-serif'};
var $rtfeldman$elm_css$Css$Global$selector = F2(
	function (selectorStr, styles) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
			styles,
			A2($rtfeldman$elm_css$Css$Structure$CustomSelector, selectorStr, _List_Nil));
	});
var $rtfeldman$elm_css$Html$Styled$span = $rtfeldman$elm_css$Html$Styled$node('span');
var $rtfeldman$elm_css$Css$top = $rtfeldman$elm_css$Css$prop1('top');
var $rtfeldman$elm_css$Css$visibility = $rtfeldman$elm_css$Css$prop1('visibility');
var $rtfeldman$elm_css$Css$visible = {overflow: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'visible', visibility: $rtfeldman$elm_css$Css$Structure$Compatible};
var $rtfeldman$elm_css$Css$zIndex = $rtfeldman$elm_css$Css$prop1('z-index');
var $author$project$Elements$tooltip = F4(
	function (width, dir, trigger, content) {
		var dirAttrs = function () {
			switch (dir.$) {
				case 'Bottom':
					return _List_fromArray(
						[
							$rtfeldman$elm_css$Css$top(
							$rtfeldman$elm_css$Css$pct(105)),
							$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero)
						]);
				case 'Right':
					return _List_fromArray(
						[
							$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
							$rtfeldman$elm_css$Css$left(
							$rtfeldman$elm_css$Css$pct(105))
						]);
				default:
					return _List_fromArray(
						[
							$rtfeldman$elm_css$Css$top(
							$rtfeldman$elm_css$Css$pct(105)),
							$rtfeldman$elm_css$Css$right(
							$rtfeldman$elm_css$Css$pct(95))
						]);
			}
		}();
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('tooltip'),
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$inlineBlock),
							$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
							$rtfeldman$elm_css$Css$hover(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$Global$descendants(
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Global$selector,
											'.tooltiptext',
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$visibility($rtfeldman$elm_css$Css$visible)
												]))
										]))
								])),
							A3(
							$rtfeldman$elm_css$Css$borderBottom3,
							$rtfeldman$elm_css$Css$px(1),
							$rtfeldman$elm_css$Css$dotted,
							$rtfeldman$elm_css$Css$hex('000000'))
						]))
				]),
			_List_fromArray(
				[
					trigger,
					A2(
					$rtfeldman$elm_css$Html$Styled$span,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('tooltiptext'),
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_Utils_ap(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$visibility($rtfeldman$elm_css$Css$hidden),
										$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
										$rtfeldman$elm_css$Css$backgroundColor(
										$rtfeldman$elm_css$Css$hex('000000')),
										$rtfeldman$elm_css$Css$width(
										$rtfeldman$elm_css$Css$mm(width)),
										$rtfeldman$elm_css$Css$color(
										$rtfeldman$elm_css$Css$hex('ffffff')),
										$rtfeldman$elm_css$Css$fontFamilies(
										_List_fromArray(
											[
												'Dosis',
												function ($) {
												return $.value;
											}($rtfeldman$elm_css$Css$sansSerif)
											])),
										$rtfeldman$elm_css$Css$zIndex(
										$rtfeldman$elm_css$Css$int(1)),
										$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
										A2(
										$rtfeldman$elm_css$Css$padding2,
										$rtfeldman$elm_css$Css$px(5),
										$rtfeldman$elm_css$Css$px(0)),
										$rtfeldman$elm_css$Css$borderRadius(
										$rtfeldman$elm_css$Css$px(6))
									]),
								dirAttrs))
						]),
					_List_fromArray(
						[content]))
				]));
	});
var $author$project$Elements$viewNavButton = F3(
	function (msg, symbol, tooltipText) {
		return A4(
			$author$project$Elements$tooltip,
			50,
			$author$project$Elements$BottomLeft,
			A2(
				$rtfeldman$elm_css$Html$Styled$button,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Elements$navButtonStyle),
						$rtfeldman$elm_css$Html$Styled$Events$onClick(msg)
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$img,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$width(
										$rtfeldman$elm_css$Css$px(16))
									])),
								$rtfeldman$elm_css$Html$Styled$Attributes$src('/icons/' + symbol)
							]),
						_List_Nil)
					])),
			$rtfeldman$elm_css$Html$Styled$text(tooltipText));
	});
var $author$project$Elements$viewEditCharacterButton = A3($author$project$Elements$viewNavButton, $author$project$Types$EditCharacter, 'edit.png', 'Edit this character');
var $author$project$Types$GotoSheet = {$: 'GotoSheet'};
var $author$project$Elements$viewGotoSheetButton = A3($author$project$Elements$viewNavButton, $author$project$Types$GotoSheet, 'sheet.png', 'View/print character sheet');
var $rtfeldman$elm_css$Css$fixed = {backgroundAttachment: $rtfeldman$elm_css$Css$Structure$Compatible, position: $rtfeldman$elm_css$Css$Structure$Compatible, tableLayout: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'fixed'};
var $rtfeldman$elm_css$Css$marginRight = $rtfeldman$elm_css$Css$prop1('margin-right');
var $author$project$Elements$navButtonsStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
		$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$right($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$zIndex(
		$rtfeldman$elm_css$Css$int(3)),
		$rtfeldman$elm_css$Css$marginTop(
		$rtfeldman$elm_css$Css$mm(1.2)),
		$rtfeldman$elm_css$Css$marginRight(
		$rtfeldman$elm_css$Css$mm(1.2))
	]);
var $author$project$Elements$viewNavButtons = function (buttons) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Elements$navButtonsStyle)
			]),
		buttons);
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $rtfeldman$elm_css$Css$border3 = $rtfeldman$elm_css$Css$prop3('border');
var $rtfeldman$elm_css$Css$borderBox = {backgroundClip: $rtfeldman$elm_css$Css$Structure$Compatible, boxSizing: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'border-box'};
var $rtfeldman$elm_css$Css$boxSizing = $rtfeldman$elm_css$Css$prop1('box-sizing');
var $rtfeldman$elm_css$Css$column = _Utils_update(
	$rtfeldman$elm_css$Css$row,
	{value: 'column'});
var $elm$core$String$endsWith = _String_endsWith;
var $rtfeldman$elm_css$Css$makeImportant = function (_v0) {
	var str = _v0.a;
	return A2(
		$elm$core$String$endsWith,
		' !important',
		$elm$core$String$toLower(str)) ? $rtfeldman$elm_css$Css$Structure$Property(str) : $rtfeldman$elm_css$Css$Structure$Property(str + ' !important');
};
var $rtfeldman$elm_css$Css$Preprocess$mapAllProperties = F2(
	function (update, styles) {
		if (!styles.b) {
			return styles;
		} else {
			if (!styles.b.b) {
				var only = styles.a;
				return _List_fromArray(
					[
						A2($rtfeldman$elm_css$Css$Preprocess$mapProperties, update, only)
					]);
			} else {
				var first = styles.a;
				var rest = styles.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Preprocess$mapAllProperties, update, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$mapProperties = F2(
	function (update, style) {
		switch (style.$) {
			case 'AppendProperty':
				var property = style.a;
				return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(
					update(property));
			case 'ExtendSelector':
				var selector = style.a;
				var styles = style.b;
				return A2(
					$rtfeldman$elm_css$Css$Preprocess$ExtendSelector,
					selector,
					A2($rtfeldman$elm_css$Css$Preprocess$mapAllProperties, update, styles));
			case 'NestSnippet':
				return style;
			case 'WithPseudoElement':
				return style;
			case 'WithMedia':
				return style;
			case 'WithKeyframes':
				return style;
			default:
				var otherStyles = style.a;
				return $rtfeldman$elm_css$Css$Preprocess$ApplyStyles(
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Css$Preprocess$mapProperties(update),
						otherStyles));
		}
	});
var $rtfeldman$elm_css$Css$important = $rtfeldman$elm_css$Css$Preprocess$mapProperties($rtfeldman$elm_css$Css$makeImportant);
var $rtfeldman$elm_css$Css$minHeight = $rtfeldman$elm_css$Css$prop1('min-height');
var $rtfeldman$elm_css$Css$minWidth = $rtfeldman$elm_css$Css$prop1('min-width');
var $rtfeldman$elm_css$Css$padding = $rtfeldman$elm_css$Css$prop1('padding');
var $rtfeldman$elm_css$Css$cssFunction = F2(
	function (funcName, args) {
		return funcName + ('(' + (A2($elm$core$String$join, ',', args) + ')'));
	});
var $rtfeldman$elm_css$Css$rgb = F3(
	function (r, g, b) {
		return {
			alpha: 1,
			blue: b,
			color: $rtfeldman$elm_css$Css$Structure$Compatible,
			green: g,
			red: r,
			value: A2(
				$rtfeldman$elm_css$Css$cssFunction,
				'rgb',
				A2(
					$elm$core$List$map,
					$elm$core$String$fromInt,
					_List_fromArray(
						[r, g, b])))
		};
	});
var $rtfeldman$elm_css$Css$solid = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationStyle: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'solid'};
var $author$project$Page$CardsPage$cardStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
		A3(
		$rtfeldman$elm_css$Css$border3,
		$rtfeldman$elm_css$Css$px(1),
		$rtfeldman$elm_css$Css$solid,
		A3($rtfeldman$elm_css$Css$rgb, 0, 0, 0)),
		$rtfeldman$elm_css$Css$width(
		$rtfeldman$elm_css$Css$mm(63)),
		$rtfeldman$elm_css$Css$minWidth(
		$rtfeldman$elm_css$Css$mm(63)),
		$rtfeldman$elm_css$Css$height(
		$rtfeldman$elm_css$Css$mm(88)),
		$rtfeldman$elm_css$Css$minHeight(
		$rtfeldman$elm_css$Css$mm(88)),
		$rtfeldman$elm_css$Css$boxSizing($rtfeldman$elm_css$Css$borderBox),
		$rtfeldman$elm_css$Css$important(
		$rtfeldman$elm_css$Css$backgroundColor(
			$rtfeldman$elm_css$Css$hex('e8e8e8'))),
		A2($rtfeldman$elm_css$Css$property, 'print-color-adjust', 'exact'),
		$rtfeldman$elm_css$Css$fontFamilies(
		_List_fromArray(
			['Verdana', 'Dosis'])),
		$rtfeldman$elm_css$Css$padding(
		$rtfeldman$elm_css$Css$px(4)),
		$rtfeldman$elm_css$Css$displayFlex,
		$rtfeldman$elm_css$Css$flexDirection($rtfeldman$elm_css$Css$column)
	]);
var $author$project$Page$CardsPage$cardTitleSectionStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
		$rtfeldman$elm_css$Css$important(
		$rtfeldman$elm_css$Css$backgroundColor(
			$rtfeldman$elm_css$Css$hex('ffffff'))),
		A2($rtfeldman$elm_css$Css$property, 'print-color-adjust', 'exact'),
		$rtfeldman$elm_css$Css$borderRadius(
		$rtfeldman$elm_css$Css$px(8)),
		A3(
		$rtfeldman$elm_css$Css$border3,
		$rtfeldman$elm_css$Css$px(2),
		$rtfeldman$elm_css$Css$solid,
		A3($rtfeldman$elm_css$Css$rgb, 0, 0, 0)),
		$rtfeldman$elm_css$Css$displayFlex,
		$rtfeldman$elm_css$Css$flexDirection($rtfeldman$elm_css$Css$column)
	]);
var $rtfeldman$elm_css$Css$capitalize = {textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'capitalize'};
var $rtfeldman$elm_css$Css$textTransform = $rtfeldman$elm_css$Css$prop1('text-transform');
var $author$project$Page$CardsPage$cardTitleStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$fontSize(
		$rtfeldman$elm_css$Css$mm(4.2)),
		$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$capitalize)
	]);
var $rtfeldman$elm_css$Css$prop4 = F5(
	function (key, argA, argB, argC, argD) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + (argC.value + (' ' + argD.value))))));
	});
var $rtfeldman$elm_css$Css$borderRadius4 = $rtfeldman$elm_css$Css$prop4('border-radius');
var $rtfeldman$elm_css$Css$bottom = $rtfeldman$elm_css$Css$prop1('bottom');
var $rtfeldman$elm_css$Css$fontWeight = function (_v0) {
	var value = _v0.value;
	return A2($rtfeldman$elm_css$Css$property, 'font-weight', value);
};
var $rtfeldman$elm_css$Css$uppercase = {textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'uppercase'};
var $author$project$Page$CardsPage$cardTypeStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
		$rtfeldman$elm_css$Css$right($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$bottom($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$fontSize(
		$rtfeldman$elm_css$Css$px(6)),
		$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('000000')),
		$rtfeldman$elm_css$Css$color(
		$rtfeldman$elm_css$Css$hex('ffffff')),
		A4(
		$rtfeldman$elm_css$Css$borderRadius4,
		$rtfeldman$elm_css$Css$mm(1),
		$rtfeldman$elm_css$Css$zero,
		$rtfeldman$elm_css$Css$zero,
		$rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$paddingLeft(
		$rtfeldman$elm_css$Css$mm(0.4)),
		$rtfeldman$elm_css$Css$paddingTop(
		$rtfeldman$elm_css$Css$mm(0.4)),
		$rtfeldman$elm_css$Css$fontWeight(
		$rtfeldman$elm_css$Css$int(900))
	]);
var $rtfeldman$elm_css$Css$justify = $rtfeldman$elm_css$Css$prop1('justify');
var $author$project$Page$CardsPage$descriptionStyle = function (fontSize) {
	return _List_fromArray(
		[
			$rtfeldman$elm_css$Css$fontSize(
			$rtfeldman$elm_css$Css$px(fontSize)),
			$rtfeldman$elm_css$Css$lineHeight(
			$rtfeldman$elm_css$Css$px(fontSize)),
			$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$justify),
			$rtfeldman$elm_css$Css$backgroundColor(
			$rtfeldman$elm_css$Css$hex('ffffff')),
			A2($rtfeldman$elm_css$Css$property, 'print-color-adjust', 'exact'),
			$rtfeldman$elm_css$Css$borderRadius(
			$rtfeldman$elm_css$Css$px(8)),
			$rtfeldman$elm_css$Css$padding(
			$rtfeldman$elm_css$Css$mm(1)),
			$rtfeldman$elm_css$Css$marginBottom(
			$rtfeldman$elm_css$Css$mm(2))
		]);
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Page$CardsPage$lookupLargestLeq = F2(
	function (x, l) {
		if (l.b) {
			var _v1 = l.a;
			var y = _v1.a;
			var val = _v1.b;
			var ys = l.b;
			return (_Utils_cmp(x, y) > -1) ? $elm$core$Maybe$Just(
				A2(
					$elm$core$Maybe$withDefault,
					val,
					A2($author$project$Page$CardsPage$lookupLargestLeq, x, ys))) : $elm$core$Maybe$Nothing;
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Page$CardsPage$estimateFontSize = function (len) {
	return A2(
		$elm$core$Maybe$withDefault,
		6,
		A2(
			$author$project$Page$CardsPage$lookupLargestLeq,
			len,
			_List_fromArray(
				[
					_Utils_Tuple2(0, 14),
					_Utils_Tuple2(500, 12),
					_Utils_Tuple2(700, 10),
					_Utils_Tuple2(1000, 8),
					_Utils_Tuple2(1600, 7),
					_Utils_Tuple2(1800, 6)
				])));
};
var $rtfeldman$elm_css$Css$flexGrow = $rtfeldman$elm_css$Css$prop1('flex-grow');
var $rtfeldman$elm_css$Html$Styled$fromUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode;
var $rtfeldman$elm_css$Css$UnitlessFloat = {$: 'UnitlessFloat'};
var $rtfeldman$elm_css$Css$num = function (val) {
	return {
		lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
		lineHeight: $rtfeldman$elm_css$Css$Structure$Compatible,
		number: $rtfeldman$elm_css$Css$Structure$Compatible,
		numberOrInfinite: $rtfeldman$elm_css$Css$Structure$Compatible,
		numericValue: val,
		unitLabel: '',
		units: $rtfeldman$elm_css$Css$UnitlessFloat,
		value: $elm$core$String$fromFloat(val)
	};
};
var $elm_explorations$markdown$Markdown$toHtmlWith = _Markdown_toHtml;
var $author$project$Page$CardsPage$viewNotableTrait = F3(
	function (options, category, _v0) {
		var name = _v0.name;
		var desc = _v0.desc;
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardStyle)
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardTitleSectionStyle)
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardTitleStyle)
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(name)
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$flexGrow(
									$rtfeldman$elm_css$Css$num(1)),
									$rtfeldman$elm_css$Css$minHeight($rtfeldman$elm_css$Css$zero)
								]))
						]),
					_List_Nil),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							$author$project$Page$CardsPage$descriptionStyle(
								A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										A2($elm$core$Basics$composeL, $author$project$Page$CardsPage$estimateFontSize, $elm$core$String$length),
										desc)))),
							$rtfeldman$elm_css$Html$Styled$Attributes$class('card-description')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$fromUnstyled(
							A3(
								$elm_explorations$markdown$Markdown$toHtmlWith,
								{
									defaultHighlighting: $elm$core$Maybe$Nothing,
									githubFlavored: $elm$core$Maybe$Just(
										{breaks: false, tables: true}),
									sanitize: false,
									smartypants: true
								},
								_List_Nil,
								A2($elm$core$Maybe$withDefault, '', desc)))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardTypeStyle)
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(category + ' feature')
						]))
				]));
	});
var $author$project$Page$CardsPage$viewNotableTraitCategory = F2(
	function (options, _v0) {
		var category = _v0.category;
		var traits = _v0.traits;
		return A2(
			$elm$core$List$map,
			A2($author$project$Page$CardsPage$viewNotableTrait, options, category),
			A2(
				$elm$core$List$filter,
				function (trait) {
					return !_Utils_eq(trait.desc, $elm$core$Maybe$Nothing);
				},
				traits));
	});
var $author$project$Types$GotoSelectCharacterPage = {$: 'GotoSelectCharacterPage'};
var $author$project$Elements$viewSelectCharacterButton = A3($author$project$Elements$viewNavButton, $author$project$Types$GotoSelectCharacterPage, 'close.png', 'Return to character selection');
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $author$project$Page$CardsPage$shouldIncludeSpell = F3(
	function (_v0, preparedSpells, _v1) {
		var showSpells = _v0.showSpells;
		var name = _v1.name;
		var prepared = _v1.prepared;
		switch (showSpells.$) {
			case 'AllSpells':
				return true;
			case 'OnlyPreparedSpells':
				return prepared || A2($elm$core$Set$member, name, preparedSpells);
			default:
				return false;
		}
	});
var $author$project$Page$CardsPage$cardBoxStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$important(
		$rtfeldman$elm_css$Css$backgroundColor(
			$rtfeldman$elm_css$Css$hex('ffffff'))),
		A2($rtfeldman$elm_css$Css$property, 'print-color-adjust', 'exact'),
		$rtfeldman$elm_css$Css$borderRadius(
		$rtfeldman$elm_css$Css$px(6)),
		$rtfeldman$elm_css$Css$fontSize(
		$rtfeldman$elm_css$Css$px(8)),
		$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center),
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
		A2($rtfeldman$elm_css$Css$property, 'display', 'grid'),
		A2($rtfeldman$elm_css$Css$property, 'grid-template-columns', '1fr 5fr'),
		$rtfeldman$elm_css$Css$padding(
		$rtfeldman$elm_css$Css$mm(0.7))
	]);
var $author$project$Page$CardsPage$icon = function (iconName) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$img,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$src('/icons/' + (iconName + '.png')),
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$width(
						$rtfeldman$elm_css$Css$px(10)),
						$rtfeldman$elm_css$Css$paddingLeft(
						$rtfeldman$elm_css$Css$mm(1))
					]))
			]),
		_List_Nil);
};
var $author$project$Page$CardsPage$cardBox = F2(
	function (iconName, value) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardBoxStyle)
				]),
			_List_fromArray(
				[
					$author$project$Page$CardsPage$icon(iconName),
					$rtfeldman$elm_css$Html$Styled$text(value)
				]));
	});
var $author$project$Page$CardsPage$cardBoxesSectionStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$marginTop(
		$rtfeldman$elm_css$Css$mm(1.5)),
		A2($rtfeldman$elm_css$Css$property, 'display', 'grid'),
		A2($rtfeldman$elm_css$Css$property, 'grid-template-columns', '1fr 1fr 1fr'),
		A2($rtfeldman$elm_css$Css$property, 'gap', '0.6mm')
	]);
var $author$project$Util$ordinal = function (n) {
	switch (n) {
		case 1:
			return '1st';
		case 2:
			return '2nd';
		case 3:
			return '3rd';
		default:
			return $elm$core$String$fromInt(n) + 'th';
	}
};
var $author$project$Page$CardsPage$cardSubtitle = function (spell) {
	var levelAndSchool = (!spell.level) ? (spell.school + ' cantrip') : ($author$project$Util$ordinal(spell.level) + (' level ' + spell.school));
	var _v0 = spell.ritual;
	switch (_v0.$) {
		case 'NotRitual':
			return levelAndSchool;
		case 'Ritual':
			return levelAndSchool + ' (ritual)';
		default:
			return levelAndSchool + ' (only ritual)';
	}
};
var $rtfeldman$elm_css$Css$bold = {fontWeight: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'bold'};
var $author$project$Page$CardsPage$cardSubtitleStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
		$rtfeldman$elm_css$Css$fontSize(
		$rtfeldman$elm_css$Css$px(7)),
		$rtfeldman$elm_css$Css$fontWeight($rtfeldman$elm_css$Css$bold)
	]);
var $author$project$Page$CardsPage$getSpellDescriptionText = function (spell) {
	return A2(
		$elm$core$Maybe$withDefault,
		spell.description,
		A2(
			$elm$core$Maybe$map,
			function (d) {
				return A2($elm$core$List$cons, '(Summary:)', d);
			},
			spell.shortdesc));
};
var $author$project$Page$CardsPage$showComponent = function (c) {
	switch (c.$) {
		case 'V':
			return 'V';
		case 'S':
			return 'S';
		default:
			return 'M';
	}
};
var $author$project$Page$CardsPage$showComponents = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$map($author$project$Page$CardsPage$showComponent),
	A2(
		$elm$core$Basics$composeR,
		$elm$core$List$intersperse(', '),
		$elm$core$String$concat));
var $rtfeldman$elm_css$Css$paddingRight = $rtfeldman$elm_css$Css$prop1('padding-right');
var $author$project$Page$CardsPage$concentrationBadgeStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
		$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$bottom($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$fontSize(
		$rtfeldman$elm_css$Css$px(6)),
		$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$uppercase),
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('000000')),
		$rtfeldman$elm_css$Css$color(
		$rtfeldman$elm_css$Css$hex('ffffff')),
		A4(
		$rtfeldman$elm_css$Css$borderRadius4,
		$rtfeldman$elm_css$Css$zero,
		$rtfeldman$elm_css$Css$mm(1),
		$rtfeldman$elm_css$Css$zero,
		$rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$paddingRight(
		$rtfeldman$elm_css$Css$mm(0.4)),
		$rtfeldman$elm_css$Css$paddingTop(
		$rtfeldman$elm_css$Css$mm(0.4)),
		$rtfeldman$elm_css$Css$fontWeight(
		$rtfeldman$elm_css$Css$int(900))
	]);
var $author$project$Page$CardsPage$viewConcentrationBadge = function (concentration) {
	return concentration ? _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$concentrationBadgeStyle)
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('concentration')
				]))
		]) : _List_Nil;
};
var $author$project$Page$CardsPage$descriptionContainsTable = $elm$core$List$any(
	$elm$core$String$contains('|---|'));
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $author$project$Page$CardsPage$spellDescriptionLength = F2(
	function (paragraphs, higherLevel) {
		return $elm$core$List$sum(
			A2(
				$elm$core$List$cons,
				A2(
					$elm$core$Maybe$withDefault,
					0,
					A2($elm$core$Maybe$map, $elm$core$String$length, higherLevel)),
				A2($elm$core$List$map, $elm$core$String$length, paragraphs)));
	});
var $author$project$Page$CardsPage$estimateSpellDescFontSize = F2(
	function (paragraphs, higherLevel) {
		return $author$project$Page$CardsPage$descriptionContainsTable(paragraphs) ? 6 : $author$project$Page$CardsPage$estimateFontSize(
			A2($author$project$Page$CardsPage$spellDescriptionLength, paragraphs, higherLevel));
	});
var $rtfeldman$elm_css$Html$Styled$b = $rtfeldman$elm_css$Html$Styled$node('b');
var $rtfeldman$elm_css$Html$Styled$p = $rtfeldman$elm_css$Html$Styled$node('p');
var $author$project$Page$CardsPage$viewHigherLevelP = function (hl) {
	if (hl.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var hldesc = hl.a;
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$p,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$b,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('At higher levels. ')
							])),
						$rtfeldman$elm_css$Html$Styled$text(hldesc)
					]))
			]);
	}
};
var $author$project$Page$CardsPage$formatSpellBonus = function (_v0) {
	var origin = _v0.origin;
	var bonus = _v0.bonus;
	return bonus + (' (from ' + (origin + ')'));
};
var $author$project$Page$CardsPage$viewSpellBonuses = function (bonuses) {
	if (!bonuses.b) {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$p,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$b,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Bonuses: ')
							])),
						$rtfeldman$elm_css$Html$Styled$text(
						$elm$core$String$concat(
							A2(
								$elm$core$List$intersperse,
								'; ',
								A2($elm$core$List$map, $author$project$Page$CardsPage$formatSpellBonus, bonuses))))
					]))
			]);
	}
};
var $author$project$Page$CardsPage$viewSpellResources = F2(
	function (resources, spellLevel) {
		var _v0 = _Utils_Tuple2(spellLevel, resources);
		if (!_v0.a) {
			return _List_Nil;
		} else {
			if (!_v0.b.b) {
				return _List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$b,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Cast at will.')
									]))
							]))
					]);
			} else {
				return _List_Nil;
			}
		}
	});
var $author$project$Page$CardsPage$viewSpellDescription = F5(
	function (paragraphs, higherLevel, bonuses, resources, spellLevel) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					$author$project$Page$CardsPage$descriptionStyle(
						A2($author$project$Page$CardsPage$estimateSpellDescFontSize, paragraphs, higherLevel))),
					$rtfeldman$elm_css$Html$Styled$Attributes$class('card-description')
				]),
			A2(
				$elm$core$List$cons,
				$rtfeldman$elm_css$Html$Styled$fromUnstyled(
					A3(
						$elm_explorations$markdown$Markdown$toHtmlWith,
						{
							defaultHighlighting: $elm$core$Maybe$Nothing,
							githubFlavored: $elm$core$Maybe$Just(
								{breaks: false, tables: true}),
							sanitize: false,
							smartypants: true
						},
						_List_Nil,
						$elm$core$String$concat(
							A2($elm$core$List$intersperse, '\n\n', paragraphs)))),
				_Utils_ap(
					$author$project$Page$CardsPage$viewHigherLevelP(higherLevel),
					_Utils_ap(
						$author$project$Page$CardsPage$viewSpellBonuses(bonuses),
						A2($author$project$Page$CardsPage$viewSpellResources, resources, spellLevel)))));
	});
var $author$project$Page$CardsPage$viewSpellCard = F2(
	function (origin, spell) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardStyle)
				]),
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardTitleSectionStyle)
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardTitleStyle)
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(spell.name)
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardSubtitleStyle)
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(
										$author$project$Page$CardsPage$cardSubtitle(spell))
									]))
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardBoxesSectionStyle)
							]),
						_List_fromArray(
							[
								A2($author$project$Page$CardsPage$cardBox, 'action-cost', spell.casting_time),
								A2(
								$author$project$Page$CardsPage$cardBox,
								'components',
								$author$project$Page$CardsPage$showComponents(spell.components)),
								A2(
								$author$project$Page$CardsPage$cardBox,
								'rolls',
								A2($elm$core$Maybe$withDefault, '-', spell.rolls)),
								A2($author$project$Page$CardsPage$cardBox, 'hourglass', spell.duration),
								A2($author$project$Page$CardsPage$cardBox, 'range', spell.range),
								A2(
								$author$project$Page$CardsPage$cardBox,
								'aoe',
								A2($elm$core$Maybe$withDefault, '-', spell.aoe))
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$flexGrow(
										$rtfeldman$elm_css$Css$num(1)),
										$rtfeldman$elm_css$Css$minHeight($rtfeldman$elm_css$Css$zero)
									]))
							]),
						_List_Nil),
						A5(
						$author$project$Page$CardsPage$viewSpellDescription,
						$author$project$Page$CardsPage$getSpellDescriptionText(spell),
						spell.higher_level,
						spell.bonuses,
						spell.resources,
						spell.level)
					]),
				_Utils_ap(
					$author$project$Page$CardsPage$viewConcentrationBadge(spell.concentration),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardTypeStyle)
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(
									_Utils_ap(
										origin,
										function () {
											var _v0 = _Utils_Tuple2(spell.level, spell.prepared);
											_v0$0:
											while (true) {
												if (!_v0.b) {
													if (!_v0.a) {
														break _v0$0;
													} else {
														return ' spell';
													}
												} else {
													if (!_v0.a) {
														break _v0$0;
													} else {
														return ' spell - always prepared';
													}
												}
											}
											return ' cantrip';
										}()))
								]))
						]))));
	});
var $author$project$Page$CardsPage$viewSpellcastingSection = F3(
	function (options, preparedSpells, section) {
		return A2(
			$elm$core$List$map,
			$author$project$Page$CardsPage$viewSpellCard(section.origin),
			A2(
				$elm$core$List$filter,
				A2(
					$author$project$Page$CardsPage$shouldIncludeSpell,
					options,
					A2(
						$elm$core$Maybe$withDefault,
						$elm$core$Set$empty,
						A2($elm$core$Dict$get, section.origin, preparedSpells))),
				section.spells));
	});
var $author$project$Page$CardsPage$view = F3(
	function (options, sheet, preparedSpells) {
		return A2(
			$elm$core$List$cons,
			A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('dont-print')
					]),
				_List_fromArray(
					[
						$author$project$Elements$viewNavButtons(
						_List_fromArray(
							[$author$project$Elements$viewGotoSheetButton, $author$project$Elements$viewEditCharacterButton, $author$project$Elements$viewSelectCharacterButton]))
					])),
			A2(
				$elm$core$List$map,
				$rtfeldman$elm_css$Html$Styled$div(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$CardsPage$cardsStyle)
						])),
				A2(
					$author$project$Util$chunks,
					8,
					_Utils_ap(
						A2(
							$elm$core$List$concatMap,
							A2($author$project$Page$CardsPage$viewSpellcastingSection, options, preparedSpells),
							sheet.spellcasting_sections),
						A2(
							$elm$core$List$concatMap,
							$author$project$Page$CardsPage$viewNotableTraitCategory(options),
							sheet.notable_traits)))));
	});
var $author$project$Types$AllSpells = {$: 'AllSpells'};
var $author$project$Types$GotoCardsPage = F2(
	function (a, b) {
		return {$: 'GotoCardsPage', a: a, b: b};
	});
var $author$project$Types$OnlyPreparedSpells = {$: 'OnlyPreparedSpells'};
var $rtfeldman$elm_css$Html$Styled$article = $rtfeldman$elm_css$Html$Styled$node('article');
var $rtfeldman$elm_css$Css$float = function (fn) {
	return A3(
		$rtfeldman$elm_css$Css$Internal$getOverloadedProperty,
		'float',
		'float',
		fn($rtfeldman$elm_css$Css$Internal$lengthForOverloadedProperty));
};
var $rtfeldman$elm_css$Html$Styled$h1 = $rtfeldman$elm_css$Html$Styled$node('h1');
var $rtfeldman$elm_css$Html$Styled$header = $rtfeldman$elm_css$Html$Styled$node('header');
var $author$project$Types$Ability$abilities = _List_fromArray(
	['str', 'dex', 'con', 'wis', 'int', 'cha']);
var $rtfeldman$elm_css$Html$Styled$caption = $rtfeldman$elm_css$Html$Styled$node('caption');
var $rtfeldman$elm_css$Html$Styled$h4 = $rtfeldman$elm_css$Html$Styled$node('h4');
var $author$project$Util$simple = F2(
	function (f, str) {
		return A2(
			f,
			_List_Nil,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(str)
				]));
	});
var $rtfeldman$elm_css$Html$Styled$table = $rtfeldman$elm_css$Html$Styled$node('table');
var $author$project$Page$CharacterSheet$captionedTable = F3(
	function (captionText, attrs, tableRows) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$caption,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h4, captionText)
						])),
					A2($rtfeldman$elm_css$Html$Styled$table, attrs, tableRows)
				]));
	});
var $rtfeldman$elm_css$Html$Styled$th = $rtfeldman$elm_css$Html$Styled$node('th');
var $author$project$Page$CharacterSheet$thAttrs = _List_fromArray(
	[
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'text-align', 'right'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding', '8px'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'border', '1px solid lightgrey'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'background-color', 'lightgrey')
	]);
var $author$project$Page$CharacterSheet$simpleTh = function (str) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$th,
		$author$project$Page$CharacterSheet$thAttrs,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text(str)
			]));
};
var $author$project$Page$CharacterSheet$tableAttrs = _List_fromArray(
	[
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'font-family', 'Fira Code, sans-serif'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'border-collapse', 'collapse'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'border', '1px solid lightgrey')
	]);
var $rtfeldman$elm_css$Html$Styled$tr = $rtfeldman$elm_css$Html$Styled$node('tr');
var $author$project$Util$formatModifier = function (mod) {
	var _v0 = A2($elm$core$Basics$compare, mod, 0);
	if (_v0.$ === 'LT') {
		return $elm$core$String$fromInt(mod);
	} else {
		return '+' + $elm$core$String$fromInt(mod);
	}
};
var $rtfeldman$elm_css$Html$Styled$td = $rtfeldman$elm_css$Html$Styled$node('td');
var $author$project$Page$CharacterSheet$tdAttrs = _List_fromArray(
	[
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'text-align', 'right'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding', '6px'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'border', '1px solid lightgrey')
	]);
var $author$project$Page$CharacterSheet$simpleTd = function (str) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$td,
		$author$project$Page$CharacterSheet$tdAttrs,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text(str)
			]));
};
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Page$CharacterSheet$viewAbilityTableRow = F2(
	function (abilityTable, abi) {
		var _v0 = A2($elm$core$Dict$get, abi, abilityTable);
		if (_v0.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var score = _v0.a.score;
			var mod = _v0.a.mod;
			var st = _v0.a.st;
			return _List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					A2(
						$elm$core$List$cons,
						$author$project$Page$CharacterSheet$simpleTh(
							$elm$core$String$toUpper(abi)),
						_List_fromArray(
							[
								$author$project$Page$CharacterSheet$simpleTd(
								$elm$core$String$fromInt(score)),
								$author$project$Page$CharacterSheet$simpleTd(
								$author$project$Util$formatModifier(mod)),
								$author$project$Page$CharacterSheet$simpleTd(
								$author$project$Util$formatModifier(st))
							])))
				]);
		}
	});
var $author$project$Page$CharacterSheet$viewAbilityTable = function (abilityTable) {
	return A3(
		$author$project$Page$CharacterSheet$captionedTable,
		'abilities',
		$author$project$Page$CharacterSheet$tableAttrs,
		A2(
			$elm$core$List$cons,
			A2(
				$rtfeldman$elm_css$Html$Styled$tr,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Page$CharacterSheet$simpleTh(''),
						$author$project$Page$CharacterSheet$simpleTh('Score'),
						$author$project$Page$CharacterSheet$simpleTh('Mod'),
						$author$project$Page$CharacterSheet$simpleTh('ST')
					])),
			$elm$core$List$concat(
				A2(
					$elm$core$List$map,
					$author$project$Page$CharacterSheet$viewAbilityTableRow(abilityTable),
					$author$project$Types$Ability$abilities))));
};
var $author$project$Page$CharacterSheet$simpleRow = function (strs) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$tr,
		_List_Nil,
		A2($elm$core$List$map, $author$project$Page$CharacterSheet$simpleTd, strs));
};
var $author$project$Page$CharacterSheet$viewAttacks = function (attacks) {
	return A3(
		$author$project$Page$CharacterSheet$captionedTable,
		'Attacks',
		$author$project$Page$CharacterSheet$tableAttrs,
		A2(
			$elm$core$List$cons,
			A2(
				$rtfeldman$elm_css$Html$Styled$tr,
				_List_Nil,
				A2(
					$elm$core$List$map,
					$author$project$Page$CharacterSheet$simpleTh,
					_List_fromArray(
						['', 'Range', 'To Hit', 'Damage', 'Notes']))),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.name;
					var range = _v0.range;
					var to_hit_or_dc = _v0.to_hit_or_dc;
					var damage = _v0.damage;
					var notes = _v0.notes;
					return $author$project$Page$CharacterSheet$simpleRow(
						_List_fromArray(
							[name, range, to_hit_or_dc, damage, notes]));
				},
				attacks)));
};
var $author$project$Elements$Right = {$: 'Right'};
var $author$project$Page$CharacterSheet$tooltipSize = 24;
var $author$project$Page$CharacterSheet$viewTrait = function (_v0) {
	var name = _v0.name;
	var desc = _v0.desc;
	if (desc.$ === 'Nothing') {
		return $rtfeldman$elm_css$Html$Styled$text(name);
	} else {
		var actualDesc = desc.a;
		return A4(
			$author$project$Elements$tooltip,
			$author$project$Page$CharacterSheet$tooltipSize,
			$author$project$Elements$Right,
			$rtfeldman$elm_css$Html$Styled$text(name),
			$rtfeldman$elm_css$Html$Styled$text(actualDesc));
	}
};
var $author$project$Page$CharacterSheet$viewNotableTraitCategory = function (_v0) {
	var category = _v0.category;
	var traits = _v0.traits;
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$h4,
			_List_Nil,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('From ' + (category + ':'))
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$ul,
			_List_Nil,
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeL,
					A2(
						$elm$core$Basics$composeL,
						$rtfeldman$elm_css$Html$Styled$li(_List_Nil),
						$elm$core$List$singleton),
					$author$project$Page$CharacterSheet$viewTrait),
				traits))
		]);
};
var $author$project$Page$CharacterSheet$viewNotableTraits = function (traits) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_Nil,
		A2($elm$core$List$concatMap, $author$project$Page$CharacterSheet$viewNotableTraitCategory, traits));
};
var $author$project$Page$CharacterSheet$viewProficienciesListItem = F2(
	function (hdr, proficiencies) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$li,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$b,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(hdr)
						])),
					$rtfeldman$elm_css$Html$Styled$text(
					A2($elm$core$String$join, ', ', proficiencies))
				]));
	});
var $author$project$Page$CharacterSheet$viewProficiencies = F4(
	function (languages, weapons, armor, tools) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$ul,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Page$CharacterSheet$viewProficienciesListItem, 'Languages: ', languages),
							A2($author$project$Page$CharacterSheet$viewProficienciesListItem, 'Weapons: ', weapons),
							A2($author$project$Page$CharacterSheet$viewProficienciesListItem, 'Armor: ', armor),
							A2($author$project$Page$CharacterSheet$viewProficienciesListItem, 'Tools: ', tools)
						]))
				]));
	});
var $author$project$Types$Ability$skillsPerAbility = _List_fromArray(
	[
		_Utils_Tuple2(
		'str',
		_List_fromArray(
			['athletics'])),
		_Utils_Tuple2(
		'dex',
		_List_fromArray(
			['acrobatics', 'sleight of hand', 'stealth'])),
		_Utils_Tuple2('con', _List_Nil),
		_Utils_Tuple2(
		'wis',
		_List_fromArray(
			['animal handling', 'insight', 'medicine', 'perception', 'survival'])),
		_Utils_Tuple2(
		'int',
		_List_fromArray(
			['arcana', 'history', 'investigation', 'nature', 'religion'])),
		_Utils_Tuple2(
		'cha',
		_List_fromArray(
			['deception', 'intimidation', 'performance', 'persuasion']))
	]);
var $rtfeldman$elm_css$VirtualDom$Styled$attribute = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$attribute, key, value),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$rowspan = function (n) {
	return A2(
		$rtfeldman$elm_css$VirtualDom$Styled$attribute,
		'rowspan',
		$elm$core$String$fromInt(n));
};
var $author$project$Page$CharacterSheet$viewSkillTableRowContent = F2(
	function (skillTable, skill) {
		return _List_fromArray(
			[
				$author$project$Page$CharacterSheet$simpleTd(skill),
				$author$project$Page$CharacterSheet$simpleTd(
				A2(
					$elm$core$Maybe$withDefault,
					'ERROR',
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.score;
							},
							$author$project$Util$formatModifier),
						A2($elm$core$Dict$get, skill, skillTable))))
			]);
	});
var $author$project$Page$CharacterSheet$viewSkillTableSection = F2(
	function (skillTable, _v0) {
		var ability = _v0.a;
		var skills = _v0.b;
		if (!skills.b) {
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('viewSkillTableSection: empty list of skills')
				]);
		} else {
			var first = skills.a;
			var rest = skills.b;
			return A2(
				$elm$core$List$cons,
				A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$td,
							A2(
								$elm$core$List$cons,
								$rtfeldman$elm_css$Html$Styled$Attributes$rowspan(
									$elm$core$List$length(skills)),
								$author$project$Page$CharacterSheet$tdAttrs),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$b,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(
											$elm$core$String$toUpper(ability))
										]))
								])),
						A2($author$project$Page$CharacterSheet$viewSkillTableRowContent, skillTable, first))),
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						$rtfeldman$elm_css$Html$Styled$tr(_List_Nil),
						$author$project$Page$CharacterSheet$viewSkillTableRowContent(skillTable)),
					rest));
		}
	});
var $author$project$Page$CharacterSheet$viewSkillTable = function (skillTable) {
	return A3(
		$author$project$Page$CharacterSheet$captionedTable,
		'skills',
		$author$project$Page$CharacterSheet$tableAttrs,
		A2(
			$elm$core$List$cons,
			A2(
				$rtfeldman$elm_css$Html$Styled$tr,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Page$CharacterSheet$simpleTh(''),
						$author$project$Page$CharacterSheet$simpleTh('Skill'),
						$author$project$Page$CharacterSheet$simpleTh('Score')
					])),
			A2(
				$elm$core$List$concatMap,
				$author$project$Page$CharacterSheet$viewSkillTableSection(skillTable),
				$author$project$Types$Ability$skillsPerAbility)));
};
var $author$project$Page$CharacterSheet$viewPactSlotTable = $rtfeldman$elm_css$Html$Styled$text('TODO: pact slot table');
var $author$project$Page$CharacterSheet$viewSpellSlotTable = function (spellSlots) {
	return A3(
		$author$project$Page$CharacterSheet$captionedTable,
		'Spell slots',
		$author$project$Page$CharacterSheet$tableAttrs,
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$tr,
				_List_Nil,
				A2(
					$elm$core$List$cons,
					$author$project$Page$CharacterSheet$simpleTh('Level'),
					A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeL, $author$project$Page$CharacterSheet$simpleTh, $elm$core$String$fromInt),
						A2(
							$elm$core$List$range,
							1,
							$elm$core$List$length(spellSlots))))),
				A2(
				$rtfeldman$elm_css$Html$Styled$tr,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (n) {
						return A2(
							$rtfeldman$elm_css$Html$Styled$td,
							$author$project$Page$CharacterSheet$tdAttrs,
							A2(
								$elm$core$List$repeat,
								n,
								A2(
									$rtfeldman$elm_css$Html$Styled$input,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox')
										]),
									_List_Nil)));
					},
					A2($elm$core$List$cons, 0, spellSlots)))
			]));
};
var $author$project$Types$SetShowOnlyPreparedSpells = function (a) {
	return {$: 'SetShowOnlyPreparedSpells', a: a};
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$checked = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('checked');
var $rtfeldman$elm_css$Html$Styled$Attributes$for = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('htmlFor');
var $rtfeldman$elm_css$Html$Styled$h3 = $rtfeldman$elm_css$Html$Styled$node('h3');
var $rtfeldman$elm_css$Html$Styled$Attributes$id = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('id');
var $rtfeldman$elm_css$Html$Styled$label = $rtfeldman$elm_css$Html$Styled$node('label');
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$core$Set$size = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$size(dict);
};
var $author$project$Page$CharacterSheet$viewPreparedSpells = F2(
	function (maxPreparedSpells, currentlyPreparedSpells) {
		if (maxPreparedSpells.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var maxPreparedSpells_ = maxPreparedSpells.a;
			return _List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$li,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$color(
									(_Utils_cmp(
										$elm$core$Set$size(currentlyPreparedSpells),
										maxPreparedSpells_) > 0) ? $rtfeldman$elm_css$Css$hex('ff0000') : $rtfeldman$elm_css$Css$hex('000000'))
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							'Prepared spells: ' + ($elm$core$String$fromInt(
								$elm$core$Set$size(currentlyPreparedSpells)) + ('/' + $elm$core$String$fromInt(maxPreparedSpells_))))
						]))
				]);
		}
	});
var $author$project$Page$CharacterSheet$maybeSG = F2(
	function (mx, my) {
		if (mx.$ === 'Just') {
			var x = mx.a;
			return $elm$core$Maybe$Just(x);
		} else {
			if (my.$ === 'Just') {
				var y = my.a;
				return $elm$core$Maybe$Just(y);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}
	});
var $author$project$Page$CharacterSheet$showBool = function (bool) {
	return bool ? 'True' : 'False';
};
var $author$project$Page$CharacterSheet$viewComponent = function (component) {
	switch (component.$) {
		case 'V':
			return $rtfeldman$elm_css$Html$Styled$text('v');
		case 'S':
			return $rtfeldman$elm_css$Html$Styled$text('s');
		default:
			var desc = component.a;
			return A4(
				$author$project$Elements$tooltip,
				$author$project$Page$CharacterSheet$tooltipSize,
				$author$project$Elements$Right,
				$rtfeldman$elm_css$Html$Styled$text('m'),
				$rtfeldman$elm_css$Html$Styled$text(desc));
	}
};
var $author$project$Types$SetSpellPreparedness = F3(
	function (a, b, c) {
		return {$: 'SetSpellPreparedness', a: a, b: b, c: c};
	});
var $author$project$Page$CharacterSheet$viewSpellPrepared = F4(
	function (alwaysPrepared, origin, spell, nowPrepared) {
		if (alwaysPrepared) {
			return $rtfeldman$elm_css$Html$Styled$text('✓');
		} else {
			return A2(
				$rtfeldman$elm_css$Html$Styled$input,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox'),
						$rtfeldman$elm_css$Html$Styled$Events$onClick(
						A3($author$project$Types$SetSpellPreparedness, origin, spell, !nowPrepared)),
						$rtfeldman$elm_css$Html$Styled$Attributes$checked(nowPrepared)
					]),
				_List_Nil);
		}
	});
var $author$project$Page$CharacterSheet$viewSpellTableRow = F4(
	function (showOnlyPreparedSpells, origin, spell, currentlyPrepared) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$tr,
			_List_Nil,
			_Utils_ap(
				A2(
					$elm$core$List$filter,
					function (_v0) {
						return !showOnlyPreparedSpells;
					},
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$td,
							$author$project$Page$CharacterSheet$tdAttrs,
							_List_fromArray(
								[
									A4($author$project$Page$CharacterSheet$viewSpellPrepared, spell.prepared, origin, spell.name, currentlyPrepared)
								]))
						])),
				_List_fromArray(
					[
						$author$project$Page$CharacterSheet$simpleTd(
						$elm$core$String$fromInt(spell.level)),
						A2(
						$rtfeldman$elm_css$Html$Styled$td,
						$author$project$Page$CharacterSheet$tdAttrs,
						$elm$core$List$singleton(
							A4(
								$author$project$Elements$tooltip,
								$author$project$Page$CharacterSheet$tooltipSize,
								$author$project$Elements$Right,
								$rtfeldman$elm_css$Html$Styled$text(spell.name),
								A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_Nil,
									A2(
										$elm$core$List$map,
										function (paragraph) {
											return A2(
												$rtfeldman$elm_css$Html$Styled$p,
												_List_Nil,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(paragraph)
													]));
										},
										spell.description))))),
						$author$project$Page$CharacterSheet$simpleTd(spell.casting_time),
						$author$project$Page$CharacterSheet$simpleTd(spell.range),
						A2(
						$rtfeldman$elm_css$Html$Styled$td,
						$author$project$Page$CharacterSheet$tdAttrs,
						A2($elm$core$List$map, $author$project$Page$CharacterSheet$viewComponent, spell.components)),
						$author$project$Page$CharacterSheet$simpleTd(spell.duration),
						$author$project$Page$CharacterSheet$simpleTd(
						$author$project$Page$CharacterSheet$showBool(spell.concentration)),
						$author$project$Page$CharacterSheet$simpleTd(
						A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$author$project$Page$CharacterSheet$maybeSG,
								A2($elm$core$Maybe$map, $elm$core$String$fromInt, spell.dc),
								A2($elm$core$Maybe$map, $elm$core$String$fromInt, spell.to_hit)))),
						$author$project$Page$CharacterSheet$simpleTd(spell.summary),
						$author$project$Page$CharacterSheet$simpleTd(
						$elm$core$String$concat(
							A2($elm$core$List$intersperse, '; ', spell.resources)))
					])));
	});
var $author$project$Page$CharacterSheet$viewSpellcastingSection = F3(
	function (showOnlyPreparedSpells, section, currentlyPreparedSpells) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h3,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(section.origin + ' spells')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$ul,
					_List_Nil,
					_Utils_ap(
						A2($author$project$Page$CharacterSheet$viewPreparedSpells, section.max_prepared_spells, currentlyPreparedSpells),
						_List_fromArray(
							[
								A2(
								$author$project$Util$simple,
								$rtfeldman$elm_css$Html$Styled$li,
								'Spell attack mod: ' + $author$project$Util$formatModifier(section.spell_attack_mod)),
								A2(
								$author$project$Util$simple,
								$rtfeldman$elm_css$Html$Styled$li,
								'Spell save DC: ' + $elm$core$String$fromInt(section.spell_save_dc)),
								A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$li, 'Spellcasting ability: ' + section.spellcasting_ability),
								A2(
								$author$project$Util$simple,
								$rtfeldman$elm_css$Html$Styled$li,
								'Spellcasting ability mod: ' + $author$project$Util$formatModifier(section.spellcasting_ability_mod))
							]))),
					A2(
					$rtfeldman$elm_css$Html$Styled$input,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox'),
							$rtfeldman$elm_css$Html$Styled$Attributes$checked(showOnlyPreparedSpells),
							$rtfeldman$elm_css$Html$Styled$Attributes$id('showOnlyPreparedSpellsToggle'),
							$rtfeldman$elm_css$Html$Styled$Events$onClick(
							$author$project$Types$SetShowOnlyPreparedSpells(!showOnlyPreparedSpells))
						]),
					_List_Nil),
					A2(
					$rtfeldman$elm_css$Html$Styled$label,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$for('showOnlyPreparedSpellsToggle')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Show only prepared spells')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$table,
					$author$project$Page$CharacterSheet$tableAttrs,
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$tr,
							_List_Nil,
							_Utils_ap(
								A2(
									$elm$core$List$filter,
									function (_v0) {
										return !showOnlyPreparedSpells;
									},
									_List_fromArray(
										[
											$author$project$Page$CharacterSheet$simpleTh('Prep\'d')
										])),
								A2(
									$elm$core$List$map,
									$author$project$Page$CharacterSheet$simpleTh,
									_List_fromArray(
										['Lvl', 'Spell', 'CT', 'Rng', 'Cpts', 'Dur', 'Conc', 'To Hit/DC', 'Effect (summary)', 'Res'])))),
						A2(
							$elm$core$List$map,
							function (spell) {
								return A4(
									$author$project$Page$CharacterSheet$viewSpellTableRow,
									showOnlyPreparedSpells,
									section.origin,
									spell,
									A2($elm$core$Set$member, spell.name, currentlyPreparedSpells));
							},
							A2(
								$elm$core$List$filter,
								function (spell) {
									return (!showOnlyPreparedSpells) || (A2($elm$core$Set$member, spell.name, currentlyPreparedSpells) || spell.prepared);
								},
								section.spells))))
				]));
	});
var $author$project$Page$CharacterSheet$viewSpellcastingSections = F4(
	function (currentlyPreparedSpells, showOnlyPreparedSpells, sections, spellSlots) {
		if (!sections.b) {
			return A2($rtfeldman$elm_css$Html$Styled$div, _List_Nil, _List_Nil);
		} else {
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				A2(
					$elm$core$List$cons,
					A2(
						$rtfeldman$elm_css$Html$Styled$h2,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Spellcasting')
							])),
					A2(
						$elm$core$List$cons,
						$author$project$Page$CharacterSheet$viewSpellSlotTable(spellSlots),
						A2(
							$elm$core$List$cons,
							$author$project$Page$CharacterSheet$viewPactSlotTable,
							A2(
								$elm$core$List$map,
								function (section) {
									return A3(
										$author$project$Page$CharacterSheet$viewSpellcastingSection,
										showOnlyPreparedSpells,
										section,
										A2(
											$elm$core$Maybe$withDefault,
											$elm$core$Set$empty,
											A2($elm$core$Dict$get, section.origin, currentlyPreparedSpells)));
								},
								sections)))));
		}
	});
var $author$project$Page$CharacterSheet$viewSummaryTableRow = F2(
	function (header, value) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$th,
					$author$project$Page$CharacterSheet$thAttrs,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(header)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$td,
					$author$project$Page$CharacterSheet$tdAttrs,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(value)
						]))
				]));
	});
var $author$project$Page$CharacterSheet$viewSummaryTable = function (sm) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$table,
		_Utils_ap(
			$author$project$Page$CharacterSheet$tableAttrs,
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding', '4px'),
					A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'float', 'right')
				])),
		_List_fromArray(
			[
				A2($author$project$Page$CharacterSheet$viewSummaryTableRow, 'Race', sm.race),
				A2($author$project$Page$CharacterSheet$viewSummaryTableRow, 'Class', sm._class),
				A2(
				$author$project$Page$CharacterSheet$viewSummaryTableRow,
				'Level',
				$elm$core$String$fromInt(sm.level)),
				A2(
				$author$project$Page$CharacterSheet$viewSummaryTableRow,
				'Max HP',
				$elm$core$String$fromInt(sm.maxhp)),
				A2(
				$author$project$Page$CharacterSheet$viewSummaryTableRow,
				'AC',
				$elm$core$String$fromInt(sm.ac)),
				A2(
				$author$project$Page$CharacterSheet$viewSummaryTableRow,
				'Initiative',
				$elm$core$String$fromInt(sm.initiative)),
				A2($author$project$Page$CharacterSheet$viewSummaryTableRow, 'Speed', sm.speed),
				A2($author$project$Page$CharacterSheet$viewSummaryTableRow, 'HD', sm.hd),
				A2(
				$author$project$Page$CharacterSheet$viewSummaryTableRow,
				'PP',
				$elm$core$String$fromInt(sm.pp)),
				A2(
				$author$project$Page$CharacterSheet$viewSummaryTableRow,
				'Prof Bon',
				$elm$core$String$fromInt(sm.prof_bon))
			]));
};
var $author$project$Page$CharacterSheet$view = F3(
	function (currentlyPreparedSpells, showOnlyPreparedSpells, sheet) {
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'width', '100%'),
						A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'font-family', 'Liberation, sans-serif')
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$header,
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding', '1em'),
								A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'color', 'black'),
								A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'background-color', 'lightgrey'),
								A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'clear', 'left'),
								A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$float($rtfeldman$elm_css$Css$right),
												$rtfeldman$elm_css$Css$displayFlex,
												$rtfeldman$elm_css$Css$flexDirection($rtfeldman$elm_css$Css$column)
											]))
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$button,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Types$EditCharacter)
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('edit')
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$button,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Events$onClick(
												A2(
													$author$project$Types$GotoCardsPage,
													{showSpells: $author$project$Types$AllSpells},
													sheet))
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('cards')
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$button,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Events$onClick(
												A2(
													$author$project$Types$GotoCardsPage,
													{showSpells: $author$project$Types$OnlyPreparedSpells},
													sheet))
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('cards (only prepared spells)')
											]))
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$h1,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(sheet.name)
									]))
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$article,
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding', '1em')
							]),
						_List_fromArray(
							[
								$author$project$Page$CharacterSheet$viewSummaryTable(sheet.summary),
								$author$project$Page$CharacterSheet$viewAbilityTable(sheet.ability_table),
								$author$project$Page$CharacterSheet$viewSkillTable(sheet.skill_table),
								A2(
								$rtfeldman$elm_css$Html$Styled$h2,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Proficiencies')
									])),
								A4($author$project$Page$CharacterSheet$viewProficiencies, sheet.languages, sheet.weapons, sheet.armor, sheet.tools),
								A2(
								$rtfeldman$elm_css$Html$Styled$h2,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Notable traits')
									])),
								$author$project$Page$CharacterSheet$viewNotableTraits(sheet.notable_traits),
								$author$project$Page$CharacterSheet$viewAttacks(sheet.attacks),
								A4($author$project$Page$CharacterSheet$viewSpellcastingSections, currentlyPreparedSpells, showOnlyPreparedSpells, sheet.spellcasting_sections, sheet.spell_slots)
							]))
					]))
			]);
	});
var $author$project$Types$GotoEquipmentPage = {$: 'GotoEquipmentPage'};
var $author$project$Elements$viewGotoEquipmentButton = A3($author$project$Elements$viewNavButton, $author$project$Types$GotoEquipmentPage, 'equipment.png', 'Edit equipment');
var $rtfeldman$elm_css$Css$marginLeft = $rtfeldman$elm_css$Css$prop1('margin-left');
var $author$project$Page$EditCharacter$sideNavWidth = 260;
var $author$project$Page$EditCharacter$mainSectionStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$marginLeft(
		$rtfeldman$elm_css$Css$px($author$project$Page$EditCharacter$sideNavWidth))
	]);
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$Page$EditCharacter$effectCategories = $elm$core$Set$fromList(
	_List_fromArray(
		['armor', 'language', 'resistance', 'saving_throw', 'sense', 'tool', 'spellcasting_focus', 'weapon', 'skill', 'channel_divinity', 'destroy_undead']));
var $author$project$Page$EditCharacter$categorizeEffect = function (_v0) {
	var effect = _v0.effect;
	if (effect.$ === 'Compound') {
		var cat = effect.a;
		return A2($elm$core$Set$member, cat, $author$project$Page$EditCharacter$effectCategories) ? cat : 'other';
	} else {
		return 'other';
	}
};
var $author$project$Util$maybeToList = function (mx) {
	if (mx.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var x = mx.a;
		return _List_fromArray(
			[x]);
	}
};
var $author$project$Util$adjustMultiDictEntry = F3(
	function (key, val, old) {
		return A3(
			$elm$core$Dict$update,
			key(val),
			function (entry) {
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$List$cons,
						val,
						$elm$core$List$concat(
							$author$project$Util$maybeToList(entry))));
			},
			old);
	});
var $author$project$Util$multiDictFromList = function (key) {
	return A2(
		$elm$core$List$foldl,
		$author$project$Util$adjustMultiDictEntry(key),
		$elm$core$Dict$empty);
};
var $author$project$Page$EditCharacter$categorizeEffects = $author$project$Util$multiDictFromList($author$project$Page$EditCharacter$categorizeEffect);
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Elements$Bottom = {$: 'Bottom'};
var $author$project$Types$defunctor = function (tm) {
	if (tm.$ === 'Atomic') {
		var atom = tm.a;
		return _List_fromArray(
			[
				$author$project$Types$Atomic(atom)
			]);
	} else {
		var args = tm.b;
		return args;
	}
};
var $author$project$Util$formatSnakeCase = A2(
	$elm$core$Basics$composeR,
	$elm$core$String$split('_'),
	A2(
		$elm$core$Basics$composeR,
		$elm$core$List$intersperse(' '),
		$elm$core$String$concat));
var $rtfeldman$elm_css$Css$padding4 = $rtfeldman$elm_css$Css$prop4('padding');
var $author$project$Page$EditCharacter$originCategoryStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('eeeeee')),
		$rtfeldman$elm_css$Css$borderRadius(
		$rtfeldman$elm_css$Css$px(10)),
		A4(
		$rtfeldman$elm_css$Css$padding4,
		$rtfeldman$elm_css$Css$px(1),
		$rtfeldman$elm_css$Css$px(0),
		$rtfeldman$elm_css$Css$px(16),
		$rtfeldman$elm_css$Css$px(20)),
		$rtfeldman$elm_css$Css$marginTop(
		$rtfeldman$elm_css$Css$px(8))
	]);
var $author$project$Util$prologTermInfixOperators = $elm$core$Set$fromList(
	_List_fromArray(
		['+', ':', 'by', 'ft', 'd', 'else']));
var $author$project$Util$showPrologTerm_ = F4(
	function (formatString, openParen, closeParen, t) {
		var spt = A3($author$project$Util$showPrologTerm_, formatString, openParen, closeParen);
		var showCompound = F2(
			function (f, args) {
				return _Utils_ap(
					formatString(f),
					_Utils_ap(
						openParen,
						_Utils_ap(
							$elm$core$String$concat(
								A2(
									$elm$core$List$intersperse,
									', ',
									A2($elm$core$List$map, spt, args))),
							closeParen)));
			});
		_v0$5:
		while (true) {
			_v0$7:
			while (true) {
				if (t.$ === 'Atomic') {
					var atom = t.a;
					return formatString(atom);
				} else {
					if (t.b.b) {
						if (t.b.b.b) {
							if (!t.b.b.b.b) {
								if (t.b.a.$ === 'Atomic') {
									switch (t.a) {
										case 'damage':
											var _v2 = t.b;
											var ty = _v2.a.a;
											var _v3 = _v2.b;
											var damage = _v3.a;
											return spt(damage) + (' ' + (ty + ' damage'));
										case 'dc':
											if (t.b.b.a.$ === 'Atomic') {
												var _v4 = t.b;
												var ability = _v4.a.a;
												var _v5 = _v4.b;
												var dc = _v5.a.a;
												return 'DC ' + (dc + (' ' + $elm$core$String$toUpper(ability)));
											} else {
												break _v0$5;
											}
										case '/':
											if (((t.b.a.a === '1') && (t.b.b.a.$ === 'Atomic')) && (t.b.b.a.a === '2')) {
												var _v6 = t.b;
												var _v7 = _v6.b;
												return '1 / 2';
											} else {
												break _v0$5;
											}
										default:
											break _v0$5;
									}
								} else {
									break _v0$5;
								}
							} else {
								break _v0$7;
							}
						} else {
							switch (t.a) {
								case 'in':
									var _v1 = t.b;
									var s = _v1.a;
									return 'in ' + spt(s);
								case 'cr':
									var _v10 = t.b;
									var val = _v10.a;
									return 'CR ' + spt(val);
								default:
									break _v0$7;
							}
						}
					} else {
						break _v0$7;
					}
				}
			}
			var f = t.a;
			var args = t.b;
			return A2(showCompound, f, args);
		}
		var binOp = t.a;
		var _v8 = t.b;
		var n = _v8.a;
		var _v9 = _v8.b;
		var m = _v9.a;
		return A2($elm$core$Set$member, binOp, $author$project$Util$prologTermInfixOperators) ? $elm$core$String$concat(
			_List_fromArray(
				[
					spt(n),
					' ',
					binOp,
					' ',
					spt(m)
				])) : A2(
			showCompound,
			binOp,
			_List_fromArray(
				[n, m]));
	});
var $author$project$Util$showPrologTerm = A3(
	$author$project$Util$showPrologTerm_,
	function (x) {
		return x;
	},
	'(',
	')');
var $author$project$Util$showPrologTermAlt = A3($author$project$Util$showPrologTerm_, $author$project$Util$formatSnakeCase, ' (', ')');
var $author$project$Page$EditCharacter$tooltipSize = 80;
var $author$project$Page$EditCharacter$viewEffectCategory = function (_v0) {
	var category = _v0.a;
	var effects = _v0.b;
	var showTool = function (toolName) {
		return toolName + '\'s tools';
	};
	var showEffect = function (f) {
		return A2(
			$elm$core$Basics$composeR,
			function ($) {
				return $.effect;
			},
			A2(
				$elm$core$Basics$composeR,
				$author$project$Types$defunctor,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$List$map(
						A2($elm$core$Basics$composeR, $author$project$Util$showPrologTerm, f)),
					$elm$core$String$concat)));
	};
	var viewEffects = function (f) {
		return $elm$core$List$map(
			function (effect) {
				var _v2 = effect.desc;
				if (!_v2.b) {
					return $rtfeldman$elm_css$Html$Styled$text(
						A2(showEffect, f, effect));
				} else {
					var desc = _v2;
					return A4(
						$author$project$Elements$tooltip,
						$author$project$Page$EditCharacter$tooltipSize,
						$author$project$Elements$Bottom,
						$rtfeldman$elm_css$Html$Styled$text(
							A2(showEffect, f, effect)),
						A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							A2(
								$elm$core$List$map,
								$author$project$Util$simple($rtfeldman$elm_css$Html$Styled$p),
								desc)));
				}
			});
	};
	var id = function (x) {
		return x;
	};
	var formatCategory = F2(
		function (name, members) {
			return A2(
				$elm$core$List$cons,
				A2(
					$rtfeldman$elm_css$Html$Styled$b,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(name + ': ')
						])),
				members);
		});
	var commaSeparatedArgs = function (f) {
		return A2(
			$elm$core$Basics$composeR,
			viewEffects(f),
			$elm$core$List$intersperse(
				$rtfeldman$elm_css$Html$Styled$text(', ')));
	};
	var content = function () {
		switch (category) {
			case 'armor':
				return A2(
					formatCategory,
					'Armor proficiencies',
					_Utils_ap(
						A2(commaSeparatedArgs, id, effects),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('.')
							])));
			case 'language':
				return A2(
					formatCategory,
					'Languages',
					A2(commaSeparatedArgs, id, effects));
			case 'resistance':
				return A2(
					formatCategory,
					'Resistances',
					A2(commaSeparatedArgs, id, effects));
			case 'saving_throw':
				return A2(
					formatCategory,
					'Saving throws',
					A2(commaSeparatedArgs, id, effects));
			case 'sense':
				return A2(
					formatCategory,
					'Senses',
					A2(commaSeparatedArgs, id, effects));
			case 'tool':
				return A2(
					formatCategory,
					'Tool proficiencies',
					A2(commaSeparatedArgs, showTool, effects));
			case 'spellcasting_focus':
				return A2(
					formatCategory,
					'You can use a(n) ',
					_Utils_ap(
						A2(commaSeparatedArgs, id, effects),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(' spellcasting focus')
							])));
			case 'weapon':
				return A2(
					formatCategory,
					'Weapon proficiencies',
					A2(commaSeparatedArgs, id, effects));
			case 'skill':
				return A2(
					formatCategory,
					'Skills',
					A2(commaSeparatedArgs, id, effects));
			case 'channel_divinity':
				return A2(
					formatCategory,
					'Channel divinity',
					A2(commaSeparatedArgs, id, effects));
			case 'destroy_undead':
				return A2(
					formatCategory,
					'Destroy undead',
					A2(commaSeparatedArgs, id, effects));
			default:
				return A2(
					formatCategory,
					$author$project$Util$formatSnakeCase(category),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							$elm$core$String$concat(
								A2(
									$elm$core$List$intersperse,
									', ',
									A2(
										$elm$core$List$map,
										A2(
											$elm$core$Basics$composeL,
											$author$project$Util$showPrologTermAlt,
											function ($) {
												return $.effect;
											}),
										effects))))
						]));
		}
	}();
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$originCategoryStyle)
			]),
		content);
};
var $author$project$Types$LevelUpAs = function (a) {
	return {$: 'LevelUpAs', a: a};
};
var $rtfeldman$elm_css$Html$Styled$Attributes$disabled = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('disabled');
var $rtfeldman$elm_css$Html$Styled$option = $rtfeldman$elm_css$Html$Styled$node('option');
var $rtfeldman$elm_css$Html$Styled$select = $rtfeldman$elm_css$Html$Styled$node('select');
var $rtfeldman$elm_css$Html$Styled$Attributes$selected = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('selected');
var $author$project$Page$EditCharacter$viewLevelUpPage = _List_fromArray(
	[
		A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h2, 'Level Up'),
		A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h3, 'Pick a class:'),
		A2(
		$rtfeldman$elm_css$Html$Styled$select,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Types$LevelUpAs)
			]),
		A2(
			$elm$core$List$cons,
			A2(
				$rtfeldman$elm_css$Html$Styled$option,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$disabled(true),
						$rtfeldman$elm_css$Html$Styled$Attributes$selected(true)
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('-- select an option --')
					])),
			A2(
				$elm$core$List$map,
				function (x) {
					return A2(
						$rtfeldman$elm_css$Html$Styled$option,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(x)
							]));
				},
				_List_fromArray(
					['barbarian', 'bard', 'cleric', 'druid', 'fighter', 'monk', 'paladin', 'ranger', 'rogue', 'sorcerer', 'warlock', 'wizard']))))
	]);
var $author$project$Types$Choice = F3(
	function (a, b, c) {
		return {$: 'Choice', a: a, b: b, c: c};
	});
var $author$project$Types$SingletonChoice = function (a) {
	return {$: 'SingletonChoice', a: a};
};
var $author$project$Page$EditCharacter$optionsSectionStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('dddddd')),
		A4(
		$rtfeldman$elm_css$Css$padding4,
		$rtfeldman$elm_css$Css$px(1),
		$rtfeldman$elm_css$Css$px(0),
		$rtfeldman$elm_css$Css$px(16),
		$rtfeldman$elm_css$Css$px(20)),
		$rtfeldman$elm_css$Css$marginTop(
		$rtfeldman$elm_css$Css$px(4)),
		$rtfeldman$elm_css$Css$marginRight(
		$rtfeldman$elm_css$Css$px(16)),
		$rtfeldman$elm_css$Css$borderRadius(
		$rtfeldman$elm_css$Css$px(10))
	]);
var $author$project$Types$ListChoice = function (a) {
	return {$: 'ListChoice', a: a};
};
var $author$project$Types$OrSCChooseDir = F3(
	function (a, b, c) {
		return {$: 'OrSCChooseDir', a: a, b: b, c: c};
	});
var $jjant$elm_comonad_zipper$Zipper$Zipper = F3(
	function (a, b, c) {
		return {$: 'Zipper', a: a, b: b, c: c};
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $jjant$elm_comonad_zipper$Zipper$iterate = F2(
	function (f, x) {
		var _v0 = f(x);
		if (_v0.$ === 'Just') {
			var x_ = _v0.a;
			return A2(
				$elm$core$List$cons,
				x,
				A2($jjant$elm_comonad_zipper$Zipper$iterate, f, x_));
		} else {
			return _List_fromArray(
				[x]);
		}
	});
var $jjant$elm_comonad_zipper$Zipper$leftMay = function (_v0) {
	var p = _v0.a;
	var a = _v0.b;
	var n = _v0.c;
	var _v1 = $elm$core$List$reverse(p);
	if (!_v1.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var x = _v1.a;
		var xs = _v1.b;
		return $elm$core$Maybe$Just(
			A3(
				$jjant$elm_comonad_zipper$Zipper$Zipper,
				$elm$core$List$reverse(xs),
				x,
				A2($elm$core$List$cons, a, n)));
	}
};
var $jjant$elm_comonad_zipper$Zipper$rightMay = function (_v0) {
	var p = _v0.a;
	var a = _v0.b;
	var n = _v0.c;
	if (!n.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var x = n.a;
		var xs = n.b;
		return $elm$core$Maybe$Just(
			A3(
				$jjant$elm_comonad_zipper$Zipper$Zipper,
				_Utils_ap(
					p,
					_List_fromArray(
						[a])),
				x,
				xs));
	}
};
var $jjant$elm_comonad_zipper$Zipper$duplicate = function (z) {
	var gather = function (f) {
		return A2(
			$elm$core$Basics$composeL,
			$elm$core$List$drop(1),
			$jjant$elm_comonad_zipper$Zipper$iterate(f));
	};
	var lefts = $elm$core$List$reverse(
		A2(gather, $jjant$elm_comonad_zipper$Zipper$leftMay, z));
	var rights = A2(gather, $jjant$elm_comonad_zipper$Zipper$rightMay, z);
	return A3($jjant$elm_comonad_zipper$Zipper$Zipper, lefts, z, rights);
};
var $jjant$elm_comonad_zipper$Zipper$map = F2(
	function (f, _v0) {
		var p = _v0.a;
		var a = _v0.b;
		var n = _v0.c;
		return A3(
			$jjant$elm_comonad_zipper$Zipper$Zipper,
			A2($elm$core$List$map, f, p),
			f(a),
			A2($elm$core$List$map, f, n));
	});
var $jjant$elm_comonad_zipper$Zipper$extend = function (f) {
	return A2(
		$elm$core$Basics$composeL,
		$jjant$elm_comonad_zipper$Zipper$map(f),
		$jjant$elm_comonad_zipper$Zipper$duplicate);
};
var $jjant$elm_comonad_zipper$Zipper$toList = function (_v0) {
	var p = _v0.a;
	var a = _v0.b;
	var n = _v0.c;
	return _Utils_ap(
		p,
		A2($elm$core$List$cons, a, n));
};
var $author$project$Page$EditCharacter$choiceEditFunctions = F3(
	function (origin, id, choices) {
		if (!choices.b) {
			return _List_fromArray(
				[
					A2(
					$elm$core$Basics$composeL,
					A2(
						$elm$core$Basics$composeL,
						A2($author$project$Types$Choice, origin, id),
						$author$project$Types$ListChoice),
					$elm$core$List$singleton)
				]);
		} else {
			var c = choices.a;
			var cs = choices.b;
			var zipper = A3($jjant$elm_comonad_zipper$Zipper$Zipper, _List_Nil, c, cs);
			var overwriteFocused = function (_v1) {
				var pre = _v1.a;
				var post = _v1.c;
				return function (x) {
					return A3(
						$author$project$Types$Choice,
						origin,
						id,
						$author$project$Types$ListChoice(
							$jjant$elm_comonad_zipper$Zipper$toList(
								A3($jjant$elm_comonad_zipper$Zipper$Zipper, pre, x, post))));
				};
			};
			return $jjant$elm_comonad_zipper$Zipper$toList(
				A2($jjant$elm_comonad_zipper$Zipper$extend, overwriteFocused, zipper));
		}
	});
var $author$project$Types$extractChoicesList = function (spec) {
	switch (spec.$) {
		case 'ListSC':
			if (spec.a.$ === 'Just') {
				var choice = spec.a.a;
				return _List_fromArray(
					[choice]);
			} else {
				var _v1 = spec.a;
				return _List_Nil;
			}
		case 'OrSC':
			var _v2 = spec.b;
			var left = _v2.b;
			var _v3 = spec.c;
			var right = _v3.b;
			return _Utils_ap(
				$author$project$Types$extractChoicesList(left),
				$author$project$Types$extractChoicesList(right));
		default:
			var specs = spec.c;
			return A2($elm$core$List$concatMap, $author$project$Types$extractChoicesList, specs);
	}
};
var $elm$core$List$map3 = _List_map3;
var $elm$core$List$map4 = _List_map4;
var $rtfeldman$elm_css$Html$Styled$Attributes$name = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('name');
var $author$project$Types$Null = {$: 'Null'};
var $author$project$Types$SetEditCharacterPageDesc = function (a) {
	return {$: 'SetEditCharacterPageDesc', a: a};
};
var $author$project$Types$ToggleDropdown = function (a) {
	return {$: 'ToggleDropdown', a: a};
};
var $author$project$Dropdown$buttonColor = F3(
	function (isDisabled, isOptionSelected, isOpen) {
		var _v0 = _Utils_Tuple3(isDisabled, isOptionSelected, isOpen);
		_v0$0:
		while (true) {
			if (_v0.b) {
				if (!_v0.c) {
					if (_v0.a) {
						break _v0$0;
					} else {
						return A3($rtfeldman$elm_css$Css$rgb, 0, 180, 0);
					}
				} else {
					if (_v0.a) {
						break _v0$0;
					} else {
						return A3($rtfeldman$elm_css$Css$rgb, 0, 150, 0);
					}
				}
			} else {
				if (_v0.c) {
					if (_v0.a) {
						break _v0$0;
					} else {
						return $rtfeldman$elm_css$Css$hex('2989b9');
					}
				} else {
					if (_v0.a) {
						break _v0$0;
					} else {
						return $rtfeldman$elm_css$Css$hex('3498db');
					}
				}
			}
		}
		return A3($rtfeldman$elm_css$Css$rgb, 150, 150, 150);
	});
var $author$project$Dropdown$buttonStyle = F3(
	function (isDisabled, optionSelected, open) {
		return _List_fromArray(
			[
				$rtfeldman$elm_css$Css$backgroundColor(
				A3($author$project$Dropdown$buttonColor, isDisabled, optionSelected, open)),
				$rtfeldman$elm_css$Css$color(
				$rtfeldman$elm_css$Css$hex('ffffff')),
				A4(
				$rtfeldman$elm_css$Css$padding4,
				$rtfeldman$elm_css$Css$px(4),
				$rtfeldman$elm_css$Css$px(8),
				$rtfeldman$elm_css$Css$px(4),
				$rtfeldman$elm_css$Css$px(8)),
				$rtfeldman$elm_css$Css$fontSize(
				$rtfeldman$elm_css$Css$px(16)),
				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
				$rtfeldman$elm_css$Css$hover(
				isDisabled ? _List_Nil : _List_fromArray(
					[
						$rtfeldman$elm_css$Css$backgroundColor(
						A3($author$project$Dropdown$buttonColor, false, optionSelected, true))
					])),
				$rtfeldman$elm_css$Css$borderRadius(
				$rtfeldman$elm_css$Css$px(10)),
				$rtfeldman$elm_css$Css$minWidth(
				$rtfeldman$elm_css$Css$px(120)),
				$rtfeldman$elm_css$Css$minHeight(
				$rtfeldman$elm_css$Css$px(32))
			]);
	});
var $rtfeldman$elm_css$Css$block = {display: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'block'};
var $rtfeldman$elm_css$Css$prop5 = F6(
	function (key, argA, argB, argC, argD, argE) {
		return A2($rtfeldman$elm_css$Css$property, key, argA.value + (' ' + (argB.value + (' ' + (argC.value + (' ' + (argD.value + (' ' + argE.value))))))));
	});
var $rtfeldman$elm_css$Css$boxShadow5 = $rtfeldman$elm_css$Css$prop5('box-shadow');
var $rtfeldman$elm_css$Css$rgba = F4(
	function (r, g, b, alpha) {
		return {
			alpha: alpha,
			blue: b,
			color: $rtfeldman$elm_css$Css$Structure$Compatible,
			green: g,
			red: r,
			value: A2(
				$rtfeldman$elm_css$Css$cssFunction,
				'rgba',
				_Utils_ap(
					A2(
						$elm$core$List$map,
						$elm$core$String$fromInt,
						_List_fromArray(
							[r, g, b])),
					_List_fromArray(
						[
							$elm$core$String$fromFloat(alpha)
						])))
		};
	});
var $author$project$Dropdown$contentStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('f1f1f1')),
		$rtfeldman$elm_css$Css$minWidth(
		$rtfeldman$elm_css$Css$px(160)),
		A5(
		$rtfeldman$elm_css$Css$boxShadow5,
		$rtfeldman$elm_css$Css$zero,
		$rtfeldman$elm_css$Css$px(8),
		$rtfeldman$elm_css$Css$px(8),
		$rtfeldman$elm_css$Css$px(0),
		A4($rtfeldman$elm_css$Css$rgba, 0, 0, 0, 0.2)),
		$rtfeldman$elm_css$Css$zIndex(
		$rtfeldman$elm_css$Css$int(1))
	]);
var $author$project$Dropdown$dropdownStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
		$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$inlineBlock)
	]);
var $rtfeldman$elm_css$Css$none = {backgroundImage: $rtfeldman$elm_css$Css$Structure$Compatible, blockAxisOverflow: $rtfeldman$elm_css$Css$Structure$Compatible, borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, cursor: $rtfeldman$elm_css$Css$Structure$Compatible, display: $rtfeldman$elm_css$Css$Structure$Compatible, hoverCapability: $rtfeldman$elm_css$Css$Structure$Compatible, inlineAxisOverflow: $rtfeldman$elm_css$Css$Structure$Compatible, keyframes: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleType: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleTypeOrPositionOrImage: $rtfeldman$elm_css$Css$Structure$Compatible, none: $rtfeldman$elm_css$Css$Structure$Compatible, outline: $rtfeldman$elm_css$Css$Structure$Compatible, pointerDevice: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, resize: $rtfeldman$elm_css$Css$Structure$Compatible, scriptingSupport: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationLine: $rtfeldman$elm_css$Css$Structure$Compatible, textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, touchAction: $rtfeldman$elm_css$Css$Structure$Compatible, transform: $rtfeldman$elm_css$Css$Structure$Compatible, updateFrequency: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'none'};
var $rtfeldman$elm_css$Css$textDecoration = $rtfeldman$elm_css$Css$prop1('text-decoration');
var $rtfeldman$elm_css$Css$transparent = {color: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'transparent'};
var $author$project$Dropdown$hrefStyle = function (enabled) {
	return _Utils_ap(
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$color(
				enabled ? A3($rtfeldman$elm_css$Css$rgb, 0, 0, 0) : A3($rtfeldman$elm_css$Css$rgb, 150, 150, 150)),
				A2(
				$rtfeldman$elm_css$Css$padding2,
				$rtfeldman$elm_css$Css$px(12),
				$rtfeldman$elm_css$Css$px(16)),
				$rtfeldman$elm_css$Css$textDecoration($rtfeldman$elm_css$Css$none),
				$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
				$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
				$rtfeldman$elm_css$Css$backgroundColor($rtfeldman$elm_css$Css$transparent),
				$rtfeldman$elm_css$Css$minWidth(
				$rtfeldman$elm_css$Css$px(160))
			]),
		enabled ? _List_fromArray(
			[
				$rtfeldman$elm_css$Css$hover(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$backgroundColor(
						$rtfeldman$elm_css$Css$hex('dddddd'))
					])),
				$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer)
			]) : _List_Nil);
};
var $rtfeldman$elm_css$Html$Styled$Events$onMouseEnter = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$on,
		'mouseenter',
		$elm$json$Json$Decode$succeed(msg));
};
var $rtfeldman$elm_css$Html$Styled$Events$onMouseLeave = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Dropdown$dropdown = F5(
	function (isDisabled, id, currentlySelected, entries, open) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Dropdown$dropdownStyle),
					A2(
					$rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn,
					'click',
					$elm$json$Json$Decode$succeed(
						_Utils_Tuple2($author$project$Types$Null, true)))
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$button,
					A2(
						$elm$core$List$cons,
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
							A3(
								$author$project$Dropdown$buttonStyle,
								isDisabled,
								!_Utils_eq(currentlySelected, $elm$core$Maybe$Nothing),
								open)),
						isDisabled ? _List_Nil : _List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Events$onClick(
								$author$project$Types$ToggleDropdown(id))
							])),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							A2($elm$core$Maybe$withDefault, '...', currentlySelected))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							A2(
								$elm$core$List$cons,
								open ? $rtfeldman$elm_css$Css$visibility($rtfeldman$elm_css$Css$visible) : $rtfeldman$elm_css$Css$visibility($rtfeldman$elm_css$Css$hidden),
								$author$project$Dropdown$contentStyle))
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var entry = _v0.entry;
							var desc = _v0.desc;
							var enabled = _v0.enabled;
							var msg = _v0.msg;
							return A2(
								$rtfeldman$elm_css$Html$Styled$button,
								A2(
									$elm$core$List$cons,
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
										$author$project$Dropdown$hrefStyle(enabled)),
									A2(
										$elm$core$List$cons,
										$rtfeldman$elm_css$Html$Styled$Events$onMouseEnter(
											$author$project$Types$SetEditCharacterPageDesc(
												$elm$core$Maybe$Just(desc))),
										A2(
											$elm$core$List$cons,
											$rtfeldman$elm_css$Html$Styled$Events$onMouseLeave(
												$author$project$Types$SetEditCharacterPageDesc($elm$core$Maybe$Nothing)),
											enabled ? _List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Events$onClick(msg)
												]) : _List_Nil))),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(entry)
									]));
						},
						entries))
				]));
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Page$EditCharacter$viewListSC = F5(
	function (_v0, mkMsg, selected, isDisabled, options) {
		var disabledOptions = _v0.disabledOptions;
		var origin = _v0.origin;
		var id = _v0.id;
		var focusedDropdownId = _v0.focusedDropdownId;
		var dropdownIdSuffix = _v0.dropdownIdSuffix;
		var entries = A2(
			$elm$core$List$map,
			function (_v1) {
				var entry = _v1.a;
				var desc = _v1.b;
				return {
					desc: A2($elm$core$List$cons, entry, desc),
					enabled: !A2($elm$core$List$member, entry, disabledOptions),
					entry: entry,
					msg: mkMsg(entry)
				};
			},
			options);
		var dropdownId = $elm$core$String$concat(
			_List_fromArray(
				[origin, id, dropdownIdSuffix]));
		return A5(
			$author$project$Dropdown$dropdown,
			isDisabled,
			dropdownId,
			selected,
			entries,
			_Utils_eq(
				focusedDropdownId,
				$elm$core$Maybe$Just(dropdownId)));
	});
var $author$project$Page$EditCharacter$viewFromSC = F4(
	function (ctx, unique, n, subspecs) {
		var choicesList = A2($elm$core$List$concatMap, $author$project$Types$extractChoicesList, subspecs);
		var disabledOptions = unique ? choicesList : _List_Nil;
		var _v8 = ctx;
		var origin = _v8.origin;
		var id = _v8.id;
		var editFunctions = A3($author$project$Page$EditCharacter$choiceEditFunctions, origin, id, choicesList);
		var k = $elm$core$List$length(editFunctions);
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			_Utils_ap(
				A5(
					$elm$core$List$map4,
					function (i) {
						return $author$project$Page$EditCharacter$viewSpec(
							_Utils_update(
								ctx,
								{
									disabledOptions: disabledOptions,
									dropdownIdSuffix: ctx.dropdownIdSuffix + ('/' + $elm$core$String$fromInt(i))
								}));
					},
					A2($elm$core$List$range, 1, k),
					editFunctions,
					A2($elm$core$List$repeat, k, false),
					subspecs),
				A4(
					$elm$core$List$map3,
					function (i) {
						return A2(
							$author$project$Page$EditCharacter$viewSpec,
							_Utils_update(
								ctx,
								{
									disabledOptions: disabledOptions,
									dropdownIdSuffix: ctx.dropdownIdSuffix + ('/' + $elm$core$String$fromInt(i))
								}),
							function (opt) {
								return A3(
									$author$project$Types$Choice,
									origin,
									id,
									$author$project$Types$ListChoice(
										_Utils_ap(
											choicesList,
											_List_fromArray(
												[opt]))));
							});
					},
					A2($elm$core$List$range, k + 1, n),
					A2(
						$elm$core$List$cons,
						$elm$core$List$isEmpty(choicesList),
						A2($elm$core$List$repeat, n, true)),
					A2($elm$core$List$drop, k, subspecs))));
	});
var $author$project$Page$EditCharacter$viewOrSC = F4(
	function (ctx, dir, _v1, _v2) {
		var lname = _v1.a;
		var lspec = _v1.b;
		var rname = _v2.a;
		var rspec = _v2.b;
		var _v3 = ctx;
		var origin = _v3.origin;
		var id = _v3.id;
		var leftId = $elm$core$String$concat(
			A2(
				$elm$core$List$intersperse,
				'_',
				_List_fromArray(
					[origin, id, lname])));
		var name = origin + ('_' + id);
		var rightId = $elm$core$String$concat(
			A2(
				$elm$core$List$intersperse,
				'_',
				_List_fromArray(
					[origin, id, rname])));
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$input,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
							$rtfeldman$elm_css$Html$Styled$Attributes$checked(
							_Utils_eq(
								dir,
								$elm$core$Maybe$Just($author$project$Types$L))),
							$rtfeldman$elm_css$Html$Styled$Attributes$id(leftId),
							$rtfeldman$elm_css$Html$Styled$Attributes$name(name),
							$rtfeldman$elm_css$Html$Styled$Events$onInput(
							function (_v4) {
								return A3($author$project$Types$OrSCChooseDir, origin, id, $author$project$Types$L);
							})
						]),
					_List_Nil),
					A2(
					$rtfeldman$elm_css$Html$Styled$label,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$for(leftId)
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(lname)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$input,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
							$rtfeldman$elm_css$Html$Styled$Attributes$id(rightId),
							$rtfeldman$elm_css$Html$Styled$Attributes$name(name),
							$rtfeldman$elm_css$Html$Styled$Attributes$checked(
							_Utils_eq(
								dir,
								$elm$core$Maybe$Just($author$project$Types$R))),
							$rtfeldman$elm_css$Html$Styled$Events$onInput(
							function (_v5) {
								return A3($author$project$Types$OrSCChooseDir, origin, id, $author$project$Types$R);
							})
						]),
					_List_Nil),
					A2(
					$rtfeldman$elm_css$Html$Styled$label,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$for(rightId)
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(rname)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					function () {
						if (dir.$ === 'Nothing') {
							return _List_Nil;
						} else {
							var dir_ = dir.a;
							return _List_fromArray(
								[
									A4(
									$author$project$Page$EditCharacter$viewSpec,
									ctx,
									A2(
										$elm$core$Basics$composeL,
										A2($author$project$Types$Choice, origin, id),
										$author$project$Types$SingletonChoice),
									false,
									function () {
										if (dir_.$ === 'L') {
											return lspec;
										} else {
											return A2($elm$core$Debug$log, '', rspec);
										}
									}())
								]);
						}
					}())
				]));
	});
var $author$project$Page$EditCharacter$viewSpec = F4(
	function (ctx, mkMsg, isDisabled, spec) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$marginTop(
							$rtfeldman$elm_css$Css$px(4))
						]))
				]),
			_List_fromArray(
				[
					function () {
					switch (spec.$) {
						case 'ListSC':
							var selected = spec.a;
							var options = spec.b;
							return A5($author$project$Page$EditCharacter$viewListSC, ctx, mkMsg, selected, isDisabled, options);
						case 'FromSC':
							var unique = spec.a;
							var n = spec.b;
							var subspecs = spec.c;
							return A4($author$project$Page$EditCharacter$viewFromSC, ctx, unique, n, subspecs);
						default:
							var dir = spec.a;
							var left = spec.b;
							var right = spec.c;
							return A4($author$project$Page$EditCharacter$viewOrSC, ctx, dir, left, right);
					}
				}()
				]));
	});
var $author$project$Page$EditCharacter$viewOptions = F2(
	function (focusedDropdownId, _v0) {
		var origin = _v0.origin;
		var spec = _v0.spec;
		var id = _v0.id;
		var display_id = _v0.display_id;
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$optionsSectionStyle)
				]),
			_List_fromArray(
				[
					A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h3, display_id),
					A4(
					$author$project$Page$EditCharacter$viewSpec,
					{disabledOptions: _List_Nil, dropdownIdSuffix: '', focusedDropdownId: focusedDropdownId, id: id, origin: origin},
					A2(
						$elm$core$Basics$composeL,
						A2($author$project$Types$Choice, origin, id),
						$author$project$Types$SingletonChoice),
					false,
					spec)
				]));
	});
var $author$project$Page$EditCharacter$viewOriginCategoryOptions = F3(
	function (focusedDropdownId, _v0, optionsList) {
		var category = _v0.b;
		var headerMsg = function () {
			if (category === 'init') {
				return 'Choose your background, class, and race:';
			} else {
				return 'From ' + (category + ':');
			}
		}();
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$originCategoryStyle)
				]),
			A2(
				$elm$core$List$cons,
				A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h2, headerMsg),
				A2(
					$elm$core$List$map,
					$author$project$Page$EditCharacter$viewOptions(focusedDropdownId),
					optionsList)));
	});
var $author$project$Page$EditCharacter$viewMainContents = F4(
	function (focusedDropdownId, opts, tbs, selectedLevel) {
		if (selectedLevel.$ === 'Just') {
			var level = selectedLevel.a;
			var tbsHtml = A2(
				$elm$core$List$map,
				$author$project$Page$EditCharacter$viewEffectCategory,
				$elm$core$Dict$toList(
					$author$project$Page$EditCharacter$categorizeEffects(
						A2(
							$elm$core$Maybe$withDefault,
							_List_Nil,
							A2($elm$core$Dict$get, level, tbs)))));
			var optsHtml = $elm$core$Dict$values(
				A2(
					$elm$core$Dict$map,
					$author$project$Page$EditCharacter$viewOriginCategoryOptions(focusedDropdownId),
					A2(
						$author$project$Util$multiDictFromList,
						function (_v3) {
							var display_origin_category = _v3.display_origin_category;
							var origin_category_index = _v3.origin_category_index;
							return _Utils_Tuple2(origin_category_index, display_origin_category);
						},
						A2(
							$elm$core$Maybe$withDefault,
							_List_Nil,
							A2($elm$core$Dict$get, level, opts)))));
			return _Utils_ap(
				A2(
					$elm$core$List$filter,
					function (_v1) {
						return !$elm$core$List$isEmpty(tbsHtml);
					},
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$h1,
							_List_Nil,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('You gained:')
								])),
						tbsHtml)),
				A2(
					$elm$core$List$filter,
					function (_v2) {
						return !$elm$core$List$isEmpty(optsHtml);
					},
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$h1,
							_List_Nil,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('You need to make the following choices:')
								])),
						optsHtml)));
		} else {
			return $author$project$Page$EditCharacter$viewLevelUpPage;
		}
	});
var $author$project$Page$EditCharacter$topBarTdCss = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$textAlign($rtfeldman$elm_css$Css$center)
	]);
var $author$project$Page$EditCharacter$topBarTd = function (val) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$td,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$topBarTdCss)
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text(val)
			]));
};
var $author$project$Page$EditCharacter$topBarTh = function (val) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$th,
		_List_Nil,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text(val)
			]));
};
var $author$project$Page$EditCharacter$abilityBonusTableRow = function (bonuses) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$tr,
		_List_Nil,
		A2(
			$elm$core$List$cons,
			$author$project$Page$EditCharacter$topBarTh('total bonus'),
			A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeL, $author$project$Page$EditCharacter$topBarTd, $author$project$Util$formatModifier),
				bonuses)));
};
var $author$project$Page$EditCharacter$abilityModTableRow = function (modifiers) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$tr,
		_List_Nil,
		A2(
			$elm$core$List$cons,
			$author$project$Page$EditCharacter$topBarTh('mod'),
			A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeL, $author$project$Page$EditCharacter$topBarTd, $author$project$Util$formatModifier),
				modifiers)));
};
var $author$project$Page$EditCharacter$abilityNamesTableRow = A2(
	$rtfeldman$elm_css$Html$Styled$tr,
	_List_Nil,
	A2(
		$elm$core$List$cons,
		$author$project$Page$EditCharacter$topBarTh(''),
		A2($elm$core$List$map, $author$project$Page$EditCharacter$topBarTh, $author$project$Types$Ability$abilities)));
var $author$project$Page$EditCharacter$abilityScoreTableRow = function (scores) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$tr,
		_List_Nil,
		A2(
			$elm$core$List$cons,
			$author$project$Page$EditCharacter$topBarTh('score'),
			A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeL, $author$project$Page$EditCharacter$topBarTd, $elm$core$String$fromInt),
				scores)));
};
var $author$project$Types$SetBaseAbilityScore = F2(
	function (a, b) {
		return {$: 'SetBaseAbilityScore', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$maxWidth = $rtfeldman$elm_css$Css$prop1('max-width');
var $rtfeldman$elm_css$Html$Styled$Attributes$value = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('value');
var $author$project$Page$EditCharacter$baseAbilityValuesTableRow = F2(
	function (abilityTable, setAbilitiesOnNextTick) {
		var virtualBaseScoresDict = A2($author$project$Page$EditCharacter$applyBaseAbilityChanges, abilityTable, setAbilitiesOnNextTick);
		var virtualBaseScoresList = A2(
			$elm$core$List$map,
			function (abi) {
				return _Utils_Tuple2(
					abi,
					A2(
						$elm$core$Maybe$withDefault,
						0,
						A2($elm$core$Dict$get, abi, virtualBaseScoresDict)));
			},
			$author$project$Types$Ability$abilities);
		return A2(
			$rtfeldman$elm_css$Html$Styled$tr,
			_List_Nil,
			A2(
				$elm$core$List$cons,
				$author$project$Page$EditCharacter$topBarTh('base score'),
				A2(
					$elm$core$List$map,
					function (_v0) {
						var ability = _v0.a;
						var baseScore = _v0.b;
						return A2(
							$rtfeldman$elm_css$Html$Styled$td,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$topBarTdCss)
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$input,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$type_('number'),
											$rtfeldman$elm_css$Html$Styled$Attributes$value(
											$elm$core$String$fromInt(baseScore)),
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$rtfeldman$elm_css$Css$maxWidth(
													$rtfeldman$elm_css$Css$px(40))
												])),
											$rtfeldman$elm_css$Html$Styled$Events$onInput(
											A2(
												$elm$core$Basics$composeL,
												A2(
													$elm$core$Basics$composeL,
													$author$project$Types$SetBaseAbilityScore(ability),
													$elm$core$Maybe$withDefault(0)),
												$elm$core$String$toInt))
										]),
									_List_Nil)
								]));
					},
					virtualBaseScoresList)));
	});
var $rtfeldman$elm_css$Css$fontFamily = $rtfeldman$elm_css$Css$prop1('font-family');
var $author$project$Types$Ability$listFromAbilityTable = F2(
	function (extract, table) {
		return A2(
			$elm$core$List$map,
			function (abi) {
				return A2(
					$elm$core$Maybe$withDefault,
					-9999,
					A2(
						$elm$core$Maybe$map,
						extract,
						A2($elm$core$Dict$get, abi, table)));
			},
			$author$project$Types$Ability$abilities);
	});
var $rtfeldman$elm_css$Css$monospace = {fontFamily: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'monospace'};
var $rtfeldman$elm_css$Css$tableLayout = $rtfeldman$elm_css$Css$prop1('table-layout');
var $rtfeldman$elm_css$Css$overflow = $rtfeldman$elm_css$Css$prop1('overflow');
var $author$project$Page$EditCharacter$topBarStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
		$rtfeldman$elm_css$Css$width(
		$rtfeldman$elm_css$Css$pct(100)),
		$rtfeldman$elm_css$Css$overflow($rtfeldman$elm_css$Css$hidden),
		$rtfeldman$elm_css$Css$zIndex(
		$rtfeldman$elm_css$Css$int(2)),
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('#ffffff')),
		A3(
		$rtfeldman$elm_css$Css$borderBottom3,
		$rtfeldman$elm_css$Css$px(2),
		$rtfeldman$elm_css$Css$solid,
		$rtfeldman$elm_css$Css$hex('#000000')),
		A4(
		$rtfeldman$elm_css$Css$padding4,
		$rtfeldman$elm_css$Css$px(10),
		$rtfeldman$elm_css$Css$px(10),
		$rtfeldman$elm_css$Css$px(10),
		$rtfeldman$elm_css$Css$px(10))
	]);
var $author$project$Page$EditCharacter$viewTopBar = F2(
	function (abilityTable, setAbilitiesOnNextTick) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$topBarStyle)
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$table,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$maxWidth(
									$rtfeldman$elm_css$Css$px(600)),
									$rtfeldman$elm_css$Css$width(
									$rtfeldman$elm_css$Css$pct(100)),
									$rtfeldman$elm_css$Css$tableLayout($rtfeldman$elm_css$Css$fixed),
									$rtfeldman$elm_css$Css$fontFamily($rtfeldman$elm_css$Css$monospace)
								]))
						]),
					_List_fromArray(
						[
							$author$project$Page$EditCharacter$abilityNamesTableRow,
							A2($author$project$Page$EditCharacter$baseAbilityValuesTableRow, abilityTable, setAbilitiesOnNextTick),
							$author$project$Page$EditCharacter$abilityBonusTableRow(
							A2(
								$author$project$Types$Ability$listFromAbilityTable,
								function ($) {
									return $.totalBonus;
								},
								abilityTable)),
							$author$project$Page$EditCharacter$abilityScoreTableRow(
							A2(
								$author$project$Types$Ability$listFromAbilityTable,
								function ($) {
									return $.score;
								},
								abilityTable)),
							$author$project$Page$EditCharacter$abilityModTableRow(
							A2(
								$author$project$Types$Ability$listFromAbilityTable,
								function ($) {
									return $.mod;
								},
								abilityTable))
						]))
				]));
	});
var $author$project$Page$EditCharacter$viewMain = F2(
	function (focusedDropdownId, _v0) {
		var abilityTable = _v0.abilityTable;
		var optionsPerLevel = _v0.optionsPerLevel;
		var traitsAndBonusesPerLevel = _v0.traitsAndBonusesPerLevel;
		var selectedLevel = _v0.selectedLevel;
		var setAbilitiesOnNextTick = _v0.setAbilitiesOnNextTick;
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$mainSectionStyle)
				]),
			_List_fromArray(
				[
					A2($author$project$Page$EditCharacter$viewTopBar, abilityTable, setAbilitiesOnNextTick),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$padding2,
									$rtfeldman$elm_css$Css$px(150),
									$rtfeldman$elm_css$Css$px(25))
								]))
						]),
					A4($author$project$Page$EditCharacter$viewMainContents, focusedDropdownId, optionsPerLevel, traitsAndBonusesPerLevel, selectedLevel))
				]));
	});
var $author$project$Page$EditCharacter$descStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$color(
		$rtfeldman$elm_css$Css$hex('ffffff')),
		A2(
		$rtfeldman$elm_css$Css$padding2,
		$rtfeldman$elm_css$Css$px(0),
		$rtfeldman$elm_css$Css$px(10))
	]);
var $rtfeldman$elm_css$Css$overflowX = $rtfeldman$elm_css$Css$prop1('overflow-x');
var $author$project$Page$EditCharacter$sideNavStyle = _List_fromArray(
	[
		$rtfeldman$elm_css$Css$height(
		$rtfeldman$elm_css$Css$pct(100)),
		$rtfeldman$elm_css$Css$width(
		$rtfeldman$elm_css$Css$px($author$project$Page$EditCharacter$sideNavWidth)),
		$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$fixed),
		$rtfeldman$elm_css$Css$zIndex(
		$rtfeldman$elm_css$Css$int(1)),
		$rtfeldman$elm_css$Css$top($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$left($rtfeldman$elm_css$Css$zero),
		$rtfeldman$elm_css$Css$backgroundColor(
		$rtfeldman$elm_css$Css$hex('010101')),
		$rtfeldman$elm_css$Css$overflowX($rtfeldman$elm_css$Css$hidden)
	]);
var $author$project$Types$GotoLevelUp = {$: 'GotoLevelUp'};
var $author$project$Page$EditCharacter$sideNavButtonStyle = function (highlighted) {
	return _List_fromArray(
		[
			$rtfeldman$elm_css$Css$backgroundColor($rtfeldman$elm_css$Css$transparent),
			$rtfeldman$elm_css$Css$border($rtfeldman$elm_css$Css$zero),
			$rtfeldman$elm_css$Css$marginLeft(
			$rtfeldman$elm_css$Css$px(20)),
			$rtfeldman$elm_css$Css$marginTop(
			$rtfeldman$elm_css$Css$px(15)),
			$rtfeldman$elm_css$Css$padding($rtfeldman$elm_css$Css$zero),
			$rtfeldman$elm_css$Css$color(
			$rtfeldman$elm_css$Css$hex(
				highlighted ? 'ffffff' : '818181')),
			$rtfeldman$elm_css$Css$cursor($rtfeldman$elm_css$Css$pointer),
			$rtfeldman$elm_css$Css$fontSize(
			$rtfeldman$elm_css$Css$px(25)),
			$rtfeldman$elm_css$Css$display($rtfeldman$elm_css$Css$block),
			$rtfeldman$elm_css$Css$hover(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$color(
					$rtfeldman$elm_css$Css$hex('ffffff'))
				]))
		]);
};
var $author$project$Page$EditCharacter$viewLevelUpButton = function (selectedLevel) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				$author$project$Page$EditCharacter$sideNavButtonStyle(
					_Utils_eq(selectedLevel, $elm$core$Maybe$Nothing))),
				$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Types$GotoLevelUp)
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text('+')
			]));
};
var $author$project$Types$EditCharacterLevel = function (a) {
	return {$: 'EditCharacterLevel', a: a};
};
var $author$project$Page$EditCharacter$viewSideNavLevelButton = F2(
	function (selectedLevel, lvl) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$button,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					$author$project$Page$EditCharacter$sideNavButtonStyle(
						_Utils_eq(
							$elm$core$Maybe$Just(lvl),
							selectedLevel))),
					$rtfeldman$elm_css$Html$Styled$Events$onClick(
					$author$project$Types$EditCharacterLevel(lvl))
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(
					'Level ' + $elm$core$String$fromInt(lvl))
				]));
	});
var $author$project$Page$EditCharacter$viewSideNav = function (_v0) {
	var desc = _v0.desc;
	var optionsPerLevel = _v0.optionsPerLevel;
	var selectedLevel = _v0.selectedLevel;
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$sideNavStyle)
			]),
		function () {
			if ((desc.$ === 'Just') && desc.a.b) {
				var _v2 = desc.a;
				var title = _v2.a;
				var paragraphs = _v2.b;
				return A2(
					$elm$core$List$cons,
					A2(
						$rtfeldman$elm_css$Html$Styled$h2,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$Page$EditCharacter$descStyle)
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(title)
							])),
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeL,
							A2(
								$elm$core$Basics$composeL,
								$rtfeldman$elm_css$Html$Styled$p(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											A2(
												$elm$core$List$cons,
												$rtfeldman$elm_css$Css$fontSize(
													$rtfeldman$elm_css$Css$px(12)),
												$author$project$Page$EditCharacter$descStyle))
										])),
								$elm$core$List$singleton),
							$rtfeldman$elm_css$Html$Styled$text),
						paragraphs));
			} else {
				return A2(
					$elm$core$List$cons,
					$author$project$Page$EditCharacter$viewLevelUpButton(selectedLevel),
					A2(
						$elm$core$List$map,
						$author$project$Page$EditCharacter$viewSideNavLevelButton(selectedLevel),
						$elm$core$List$reverse(
							$elm$core$Dict$keys(optionsPerLevel))));
			}
		}());
};
var $author$project$Page$EditCharacter$view = F2(
	function (focusedDropdownId, data) {
		return _List_fromArray(
			[
				$author$project$Elements$viewNavButtons(
				_List_fromArray(
					[$author$project$Elements$viewGotoSheetButton, $author$project$Elements$viewGotoEquipmentButton, $author$project$Elements$viewSelectCharacterButton])),
				$author$project$Page$EditCharacter$viewSideNav(data),
				A2($author$project$Page$EditCharacter$viewMain, focusedDropdownId, data)
			]);
	});
var $author$project$Types$EquipWeapon = function (a) {
	return {$: 'EquipWeapon', a: a};
};
var $author$project$Page$Equipment$viewWeapon = function (_v0) {
	var base_weapon = _v0.base_weapon;
	var enchantment = _v0.enchantment;
	var category = _v0.category;
	var to_hit = _v0.to_hit;
	var damage = _v0.damage;
	var range = _v0.range;
	var notes = _v0.notes;
	var is_variant = _v0.is_variant;
	var weapon = _Utils_ap(
		base_weapon,
		(enchantment > 0) ? ('+' + $elm$core$String$fromInt(enchantment)) : '');
	var omitIfVariant = function (html) {
		return is_variant ? _List_Nil : html;
	};
	var indentIfVariant = function (str) {
		return is_variant ? ('↳ ' + str) : str;
	};
	return A2($rtfeldman$elm_css$Html$Styled$tr, _List_Nil, _List_Nil);
};
var $author$project$Page$Equipment$viewWeaponsTable = function (weapons) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$table,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('attacks')
			]),
		A2(
			$elm$core$List$cons,
			A2(
				$rtfeldman$elm_css$Html$Styled$tr,
				_List_Nil,
				A2(
					$elm$core$List$map,
					$author$project$Util$simple($rtfeldman$elm_css$Html$Styled$th),
					_List_fromArray(
						['', 'weapon', 'enchantment', 'category', 'to hit', 'damage', 'range', 'notes']))),
			A2($elm$core$List$map, $author$project$Page$Equipment$viewWeapon, weapons)));
};
var $author$project$Page$Equipment$view = function (_v0) {
	var weapons = _v0.weapons;
	var weapon_options = _v0.weapon_options;
	return _List_fromArray(
		[
			$author$project$Elements$viewNavButtons(
			_List_fromArray(
				[$author$project$Elements$viewGotoSheetButton, $author$project$Elements$viewEditCharacterButton, $author$project$Elements$viewSelectCharacterButton])),
			A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h1, 'Equipment'),
			A2($author$project$Util$simple, $rtfeldman$elm_css$Html$Styled$h2, 'Weapons'),
			$author$project$Page$Equipment$viewWeaponsTable(weapons),
			A2(
			$rtfeldman$elm_css$Html$Styled$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$select,
					_List_Nil,
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$option,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$disabled(true),
									$rtfeldman$elm_css$Html$Styled$Attributes$selected(true)
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('-- Add a weapon --')
								])),
						A2(
							$elm$core$List$map,
							function (s) {
								return A2(
									$rtfeldman$elm_css$Html$Styled$option,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Events$onClick(
											$author$project$Types$EquipWeapon(s))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(s)
										]));
							},
							weapon_options)))
				]))
		]);
};
var $rtfeldman$elm_css$Html$Styled$hr = $rtfeldman$elm_css$Html$Styled$node('hr');
var $rtfeldman$elm_css$Css$margin = $rtfeldman$elm_css$Css$prop1('margin');
var $author$project$Page$PrintableCharSheet$stRow = F3(
	function (label, modifier, bold) {
		var emphasis = bold ? _List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$fontWeight($rtfeldman$elm_css$Css$bold)
					]))
			]) : _List_Nil;
		return A2(
			$rtfeldman$elm_css$Html$Styled$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$td,
					emphasis,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(modifier)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$td,
					emphasis,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(label)
						]))
				]));
	});
var $author$project$Page$PrintableCharSheet$viewSkillTableRow = F2(
	function (table, skill) {
		var _v0 = A2($elm$core$Dict$get, skill, table);
		if (_v0.$ === 'Nothing') {
			return $rtfeldman$elm_css$Html$Styled$text('viewSkillTableRow ERROR');
		} else {
			var score = _v0.a.score;
			var proficient = _v0.a.proficient;
			return A3(
				$author$project$Page$PrintableCharSheet$stRow,
				skill,
				$author$project$Util$formatModifier(score),
				proficient);
		}
	});
var $author$project$Page$PrintableCharSheet$viewAbilityRow = F3(
	function (abilityTable, skillTable, _v0) {
		var ability = _v0.a;
		var skills = _v0.b;
		var _v1 = A2($elm$core$Dict$get, ability, abilityTable);
		if (_v1.$ === 'Nothing') {
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('viewAbilityRow ERROR')
				]);
		} else {
			var base = _v1.a.base;
			var totalBonus = _v1.a.totalBonus;
			var score = _v1.a.score;
			var mod = _v1.a.mod;
			var st = _v1.a.st;
			var stProf = _v1.a.stProf;
			return _List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$td,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('ability')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('ability-name')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(ability)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('ability-modifier')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(
											$author$project$Util$formatModifier(mod)),
											A2(
											$rtfeldman$elm_css$Html$Styled$hr,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$css(
													_List_fromArray(
														[
															$rtfeldman$elm_css$Css$margin($rtfeldman$elm_css$Css$zero)
														]))
												]),
											_List_Nil),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('ability-score')
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text(
													$elm$core$String$fromInt(score))
												]))
										]))
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$td,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('skill-td')
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$table,
							_List_Nil,
							A2(
								$elm$core$List$cons,
								A3(
									$author$project$Page$PrintableCharSheet$stRow,
									'saving throw',
									$author$project$Util$formatModifier(st),
									stProf),
								A2(
									$elm$core$List$map,
									$author$project$Page$PrintableCharSheet$viewSkillTableRow(skillTable),
									skills)))
						]))
				]);
		}
	});
var $author$project$Page$PrintableCharSheet$viewAbilities = F2(
	function (abilityTable, skillTable) {
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$table,
				_List_Nil,
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						$rtfeldman$elm_css$Html$Styled$tr(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('ability-row')
								])),
						A2($author$project$Page$PrintableCharSheet$viewAbilityRow, abilityTable, skillTable)),
					$author$project$Types$Ability$skillsPerAbility))
			]);
	});
var $author$project$Elements$viewGotoCardsButton = function (sheet) {
	return A3(
		$author$project$Elements$viewNavButton,
		A2(
			$author$project$Types$GotoCardsPage,
			{showSpells: $author$project$Types$AllSpells},
			sheet),
		'cards.png',
		'View/print cards');
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Page$PrintableCharSheet$viewHitDice = function (_v0) {
	var n = _v0.n;
	var d = _v0.d;
	return A2(
		$elm$core$List$repeat,
		n,
		A2(
			$rtfeldman$elm_css$Html$Styled$img,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$src(
					'/icons/d' + ($elm$core$String$fromInt(d) + '.svg'))
				]),
			_List_Nil));
};
var $author$project$Page$PrintableCharSheet$viewHitDiceSection = function (hitDice) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('badge-title')
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('hd')
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('hit-dice')
				]),
			A2(
				$elm$core$List$concatMap,
				$author$project$Page$PrintableCharSheet$viewHitDice,
				A2(
					$elm$core$List$sortBy,
					function ($) {
						return $.d;
					},
					hitDice)))
		]);
};
var $author$project$Page$PrintableCharSheet$raceAndClassesToString = F2(
	function (_class, race) {
		return $elm$core$String$concat(
			_List_fromArray(
				[race, ' — ', _class]));
	});
var $author$project$Page$PrintableCharSheet$simple = F2(
	function (f, x) {
		return A2(
			f,
			_List_Nil,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(x)
				]));
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $author$project$Page$PrintableCharSheet$nbsp = $elm$core$String$fromChar(
	$elm$core$Char$fromCode(160));
var $author$project$Page$PrintableCharSheet$plus = A2(
	$rtfeldman$elm_css$Html$Styled$div,
	_List_Nil,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$text($author$project$Page$PrintableCharSheet$nbsp + ('+' + $author$project$Page$PrintableCharSheet$nbsp))
		]));
var $author$project$Page$PrintableCharSheet$viewAcFormula = function (_v0) {
	var name = _v0.name;
	var ac = _v0.ac;
	var shield = _v0.shield;
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('ac-formula')
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('ac-formula-name')
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('▢ ' + name)
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('row')
					]),
				A2(
					$elm$core$List$cons,
					A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('labeled-flex')
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('filled-in')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_Nil,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(
												$elm$core$String$fromInt(ac))
											]))
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('base')
									]))
							])),
					function () {
						if (shield.$ === 'Nothing') {
							return _List_Nil;
						} else {
							var shieldAc = shield.a;
							return _List_fromArray(
								[
									$author$project$Page$PrintableCharSheet$plus,
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('labeled-flex')
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('filled-in')
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text(
													$elm$core$String$fromInt(shieldAc))
												])),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_Nil,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text('shield')
												]))
										]))
								]);
						}
					}()))
			]));
};
var $author$project$Page$PrintableCharSheet$viewArmorClassContent = function (acFormulas) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('column')
				]),
			A2($elm$core$List$map, $author$project$Page$PrintableCharSheet$viewAcFormula, acFormulas))
		]);
};
var $author$project$Page$PrintableCharSheet$viewAttackTableRow = function (_v0) {
	var name = _v0.name;
	var range = _v0.range;
	var to_hit_or_dc = _v0.to_hit_or_dc;
	var damage = _v0.damage;
	var notes = _v0.notes;
	return A2(
		$rtfeldman$elm_css$Html$Styled$tr,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$td,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$textTransform($rtfeldman$elm_css$Css$capitalize)
							]))
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(name)
					])),
				A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, to_hit_or_dc),
				A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, damage),
				A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, range),
				A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, notes)
			]));
};
var $author$project$Page$PrintableCharSheet$viewBadgeDiv = F3(
	function (badgeTitle, contentClass, content) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('badge')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('badge-title')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(badgeTitle)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class(contentClass)
						]),
					content)
				]));
	});
var $author$project$Page$PrintableCharSheet$viewBlank = A2(
	$rtfeldman$elm_css$Html$Styled$div,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$class('blank')
		]),
	_List_Nil);
var $author$project$Page$PrintableCharSheet$viewFilledIn = function (value) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('filled-in')
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text(
				$elm$core$String$fromInt(value))
			]));
};
var $author$project$Page$PrintableCharSheet$viewLabeledFlexBot = F2(
	function (label, blank) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('labeled-flex')
				]),
			_List_fromArray(
				[
					blank,
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(label)
						]))
				]));
	});
var $author$project$Page$PrintableCharSheet$viewLabeledFlexTop = F2(
	function (label, blank) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('labeled-flex')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(label)
						])),
					blank
				]));
	});
var $author$project$Page$PrintableCharSheet$viewHitpointsBadgeContent = function (maxHp) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('row')
				]),
			_List_fromArray(
				[
					A2($author$project$Page$PrintableCharSheet$viewLabeledFlexTop, 'current', $author$project$Page$PrintableCharSheet$viewBlank),
					$author$project$Page$PrintableCharSheet$plus,
					A2($author$project$Page$PrintableCharSheet$viewLabeledFlexTop, 'temp', $author$project$Page$PrintableCharSheet$viewBlank)
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$hr,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$marginTop(
							$rtfeldman$elm_css$Css$pt(2)),
							$rtfeldman$elm_css$Css$marginBottom(
							$rtfeldman$elm_css$Css$pt(4))
						]))
				]),
			_List_Nil),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('row')
				]),
			_List_fromArray(
				[
					A2(
					$author$project$Page$PrintableCharSheet$viewLabeledFlexBot,
					'max hp',
					$author$project$Page$PrintableCharSheet$viewFilledIn(maxHp)),
					$author$project$Page$PrintableCharSheet$plus,
					A2($author$project$Page$PrintableCharSheet$viewLabeledFlexBot, 'bonus max hp', $author$project$Page$PrintableCharSheet$viewBlank)
				]))
		]);
};
var $author$project$Page$PrintableCharSheet$viewStatTableContent = function (_v0) {
	var speed = _v0.speed;
	var initiative = _v0.initiative;
	var prof_bon = _v0.prof_bon;
	var pp = _v0.pp;
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$table,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$th, 'speed'),
							A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, speed)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$th, 'initiative'),
							A2(
							$author$project$Page$PrintableCharSheet$simple,
							$rtfeldman$elm_css$Html$Styled$td,
							$author$project$Util$formatModifier(initiative))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$th, 'proficiency bonus'),
							A2(
							$author$project$Page$PrintableCharSheet$simple,
							$rtfeldman$elm_css$Html$Styled$td,
							$author$project$Util$formatModifier(prof_bon))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$th, 'passive perception'),
							A2(
							$author$project$Page$PrintableCharSheet$simple,
							$rtfeldman$elm_css$Html$Styled$td,
							$elm$core$String$fromInt(pp))
						]))
				]))
		]);
};
var $author$project$Page$PrintableCharSheet$viewMainBody = function (sheet) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('charname')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h1,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(sheet.name)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('race-and-classes')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							A2($author$project$Page$PrintableCharSheet$raceAndClassesToString, sheet.summary._class, sheet.summary.race))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('charlevel')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							$elm$core$String$fromInt(sheet.summary.level))
						]))
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('badges')
				]),
			_List_fromArray(
				[
					A3(
					$author$project$Page$PrintableCharSheet$viewBadgeDiv,
					'hit points',
					'hp',
					$author$project$Page$PrintableCharSheet$viewHitpointsBadgeContent(sheet.summary.maxhp)),
					A3(
					$author$project$Page$PrintableCharSheet$viewBadgeDiv,
					'armor class',
					'ac',
					$author$project$Page$PrintableCharSheet$viewArmorClassContent(sheet.ac_formulas)),
					A3(
					$author$project$Page$PrintableCharSheet$viewBadgeDiv,
					'stats',
					'stat-table',
					$author$project$Page$PrintableCharSheet$viewStatTableContent(sheet.summary))
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('attacks attacks-positioning')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('badge-title')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('attacks')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$table,
					_List_Nil,
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$tr,
							_List_Nil,
							A2(
								$elm$core$List$map,
								$author$project$Page$PrintableCharSheet$simple($rtfeldman$elm_css$Html$Styled$th),
								_List_fromArray(
									['Attack', 'To Hit/DC', 'Damage', 'Range', 'Notes']))),
						A2(
							$elm$core$List$map,
							$author$project$Page$PrintableCharSheet$viewAttackTableRow,
							A2($elm$core$List$take, 5, sheet.attacks))))
				]))
		]);
};
var $author$project$Page$PrintableCharSheet$viewNotableTraitCategory = function (_v0) {
	var category = _v0.category;
	var traits = _v0.traits;
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$h3,
			_List_Nil,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(category)
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$ul,
			_List_Nil,
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeL,
					A2(
						$elm$core$Basics$composeL,
						A2(
							$elm$core$Basics$composeL,
							$rtfeldman$elm_css$Html$Styled$li(_List_Nil),
							$elm$core$List$singleton),
						$rtfeldman$elm_css$Html$Styled$text),
					function ($) {
						return $.name;
					}),
				traits))
		]);
};
var $author$project$Page$PrintableCharSheet$viewNotableTraits = function (categories) {
	return _List_fromArray(
		[
			A3(
			$author$project$Page$PrintableCharSheet$viewBadgeDiv,
			'notable traits',
			'notable-traits',
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeL,
					$rtfeldman$elm_css$Html$Styled$div(_List_Nil),
					$author$project$Page$PrintableCharSheet$viewNotableTraitCategory),
				A2(
					$elm$core$List$sortBy,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Basics$negate, $elm$core$List$length),
						function ($) {
							return $.traits;
						}),
					categories)))
		]);
};
var $author$project$Page$PrintableCharSheet$defaultWhenEmpty = F2(
	function (_default, l) {
		if (l.b) {
			return l;
		} else {
			return _List_fromArray(
				[_default]);
		}
	});
var $author$project$Page$PrintableCharSheet$viewOtherProficiencies = F4(
	function (weapons, armor, languages, tools) {
		return _List_fromArray(
			[
				A3(
				$author$project$Page$PrintableCharSheet$viewBadgeDiv,
				'other proficiencies',
				'other-proficiencies',
				A2(
					$elm$core$List$concatMap,
					function (_v0) {
						var category = _v0.a;
						var entries = _v0.b;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$h3,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(category)
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('details')
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(
										$elm$core$String$concat(
											A2($elm$core$List$intersperse, ', ', entries)))
									]))
							]);
					},
					_List_fromArray(
						[
							_Utils_Tuple2(
							'weapons',
							A2($author$project$Page$PrintableCharSheet$defaultWhenEmpty, '-', weapons)),
							_Utils_Tuple2(
							'armor',
							A2($author$project$Page$PrintableCharSheet$defaultWhenEmpty, '-', armor)),
							_Utils_Tuple2(
							'languages',
							A2($author$project$Page$PrintableCharSheet$defaultWhenEmpty, '-', languages)),
							_Utils_Tuple2(
							'tools',
							A2($author$project$Page$PrintableCharSheet$defaultWhenEmpty, '-', tools))
						])))
			]);
	});
var $author$project$Page$PrintableCharSheet$viewSlot = A2(
	$rtfeldman$elm_css$Html$Styled$input,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox')
		]),
	_List_Nil);
var $author$project$Page$PrintableCharSheet$viewPactMagic = function (pactMagic) {
	if (pactMagic.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var slot_count = pactMagic.a.slot_count;
		var slot_level = pactMagic.a.slot_level;
		return _List_fromArray(
			[
				A3(
				$author$project$Page$PrintableCharSheet$viewBadgeDiv,
				'pact magic',
				'badge-content spell-slots',
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$table,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$tr,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$author$project$Page$PrintableCharSheet$simple,
										$rtfeldman$elm_css$Html$Styled$th,
										$elm$core$String$fromInt(slot_level)),
										A2(
										$rtfeldman$elm_css$Html$Styled$td,
										_List_Nil,
										A2($elm$core$List$repeat, slot_count, $author$project$Page$PrintableCharSheet$viewSlot))
									]))
							]))
					]))
			]);
	}
};
var $author$project$Page$PrintableCharSheet$viewResourceRestoreInfoLine = F2(
	function (restType, restoreInfo) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, restType + ':'),
					A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, restoreInfo)
				]));
	});
var $author$project$Page$PrintableCharSheet$viewResourceRestoreInfo = F2(
	function (maybeShortRest, maybeLongRest) {
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$table,
				_List_Nil,
				$elm$core$List$concat(
					A2(
						$elm$core$List$map,
						$author$project$Util$maybeToList,
						_List_fromArray(
							[
								A2(
								$elm$core$Maybe$map,
								$author$project$Page$PrintableCharSheet$viewResourceRestoreInfoLine('short rest'),
								maybeShortRest),
								A2(
								$elm$core$Maybe$map,
								$author$project$Page$PrintableCharSheet$viewResourceRestoreInfoLine('long rest'),
								maybeLongRest)
							]))))
			]);
	});
var $author$project$Page$PrintableCharSheet$viewSmallBlank = A2(
	$rtfeldman$elm_css$Html$Styled$div,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$class('small-blank')
		]),
	_List_Nil);
var $author$project$Page$PrintableCharSheet$viewResourceSlots = function (num) {
	return (num <= 8) ? A2(
		$elm$core$List$repeat,
		num,
		A2(
			$rtfeldman$elm_css$Html$Styled$input,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox')
				]),
			_List_Nil)) : _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('row')
				]),
			_List_fromArray(
				[
					$author$project$Page$PrintableCharSheet$viewSmallBlank,
					$rtfeldman$elm_css$Html$Styled$text(
					$author$project$Page$PrintableCharSheet$nbsp + ('/' + ($author$project$Page$PrintableCharSheet$nbsp + $elm$core$String$fromInt(num))))
				]))
		]);
};
var $author$project$Page$PrintableCharSheet$viewResource = function (_v0) {
	var feature_name = _v0.feature_name;
	var unit_name = _v0.unit_name;
	var number = _v0.number;
	var short_rest = _v0.short_rest;
	var long_rest = _v0.long_rest;
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_Nil,
		_List_fromArray(
			[
				A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$h3, unit_name),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('resource-details')
					]),
				_Utils_ap(
					$author$project$Page$PrintableCharSheet$viewResourceSlots(number),
					A2($author$project$Page$PrintableCharSheet$viewResourceRestoreInfo, short_rest, long_rest)))
			]));
};
var $author$project$Page$PrintableCharSheet$viewResources = function (resources) {
	if (!resources.b) {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				A3(
				$author$project$Page$PrintableCharSheet$viewBadgeDiv,
				'resources',
				'resources spell-slots',
				A2($elm$core$List$map, $author$project$Page$PrintableCharSheet$viewResource, resources))
			]);
	}
};
var $author$project$Page$PrintableCharSheet$viewSpellSlotTableRow = F2(
	function (slotLevel, count) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Page$PrintableCharSheet$simple,
					$rtfeldman$elm_css$Html$Styled$th,
					$elm$core$String$fromInt(slotLevel)),
					A2(
					$rtfeldman$elm_css$Html$Styled$td,
					_List_Nil,
					A2($elm$core$List$repeat, count, $author$project$Page$PrintableCharSheet$viewSlot))
				]));
	});
var $author$project$Page$PrintableCharSheet$viewSpellSlots = function (slots) {
	if (!slots.b) {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('badge')
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('badge-title')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('spell slots')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('badge-content spell-slots')
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$table,
								_List_Nil,
								A3(
									$elm$core$List$map2,
									$author$project$Page$PrintableCharSheet$viewSpellSlotTableRow,
									A2($elm$core$List$range, 1, 9),
									slots))
							]))
					]))
			]);
	}
};
var $author$project$Util$classAbbrev = function (className) {
	switch (className) {
		case 'barbarian':
			return 'bb';
		case 'bard':
			return 'bd';
		case 'cleric':
			return 'cl';
		case 'druid':
			return 'dr';
		case 'fighter':
			return 'fi';
		case 'monk':
			return 'mo';
		case 'paladin':
			return 'pa';
		case 'ranger':
			return 'ra';
		case 'rogue':
			return 'ro';
		case 'sorcerer':
			return 'so';
		case 'warlock':
			return 'wl';
		case 'wizard':
			return 'wz';
		default:
			return '??';
	}
};
var $author$project$Page$PrintableCharSheet$viewMultiSectionSpellcastingTable = function (sections) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$table,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('spellcasting-table')
				]),
			A2(
				$elm$core$List$cons,
				A2(
					$rtfeldman$elm_css$Html$Styled$tr,
					_List_Nil,
					A2(
						$elm$core$List$cons,
						A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, ''),
						A2(
							$elm$core$List$map,
							A2(
								$elm$core$Basics$composeL,
								A2(
									$elm$core$Basics$composeL,
									$author$project$Page$PrintableCharSheet$simple($rtfeldman$elm_css$Html$Styled$th),
									$author$project$Util$classAbbrev),
								function ($) {
									return $.origin;
								}),
							sections))),
				A2(
					$elm$core$List$map,
					function (_v0) {
						var field = _v0.a;
						var fn = _v0.b;
						return A2(
							$rtfeldman$elm_css$Html$Styled$tr,
							_List_Nil,
							A2(
								$elm$core$List$cons,
								A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$th, field),
								A2(
									$elm$core$List$map,
									A2(
										$elm$core$Basics$composeL,
										$author$project$Page$PrintableCharSheet$simple($rtfeldman$elm_css$Html$Styled$td),
										fn),
									sections)));
					},
					_List_fromArray(
						[
							_Utils_Tuple2(
							'DC',
							A2(
								$elm$core$Basics$composeL,
								$elm$core$String$fromInt,
								function ($) {
									return $.spell_save_dc;
								})),
							_Utils_Tuple2(
							'mod',
							A2(
								$elm$core$Basics$composeL,
								$author$project$Util$formatModifier,
								function ($) {
									return $.spell_attack_mod;
								})),
							_Utils_Tuple2(
							'prep',
							A2(
								$elm$core$Basics$composeL,
								A2(
									$elm$core$Basics$composeL,
									$elm$core$Maybe$withDefault('-'),
									$elm$core$Maybe$map($elm$core$String$fromInt)),
								function ($) {
									return $.max_prepared_spells;
								})),
							_Utils_Tuple2(
							'abi',
							A2(
								$elm$core$Basics$composeL,
								$elm$core$String$toUpper,
								function ($) {
									return $.spellcasting_ability;
								}))
						]))))
		]);
};
var $author$project$Page$PrintableCharSheet$viewSingleSectionSpellcastingTable = function (section) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$table,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('spellcasting-table')
				]),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var field = _v0.a;
					var val = _v0.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$th, field),
								A2($author$project$Page$PrintableCharSheet$simple, $rtfeldman$elm_css$Html$Styled$td, val)
							]));
				},
				_List_fromArray(
					[
						_Utils_Tuple2(
						'save DC',
						$elm$core$String$fromInt(section.spell_save_dc)),
						_Utils_Tuple2(
						'attack mod',
						$author$project$Util$formatModifier(section.spell_attack_mod)),
						_Utils_Tuple2(
						'prepared',
						A2(
							$elm$core$Maybe$withDefault,
							'-',
							A2($elm$core$Maybe$map, $elm$core$String$fromInt, section.max_prepared_spells))),
						_Utils_Tuple2(
						'ability',
						$elm$core$String$toUpper(section.spellcasting_ability))
					])))
		]);
};
var $author$project$Page$PrintableCharSheet$viewSpellcastingTable = function (sections) {
	if (!sections.b) {
		return _List_Nil;
	} else {
		if (!sections.b.b) {
			var section = sections.a;
			return _List_fromArray(
				[
					A3(
					$author$project$Page$PrintableCharSheet$viewBadgeDiv,
					'spellcasting',
					'spellcasting',
					$author$project$Page$PrintableCharSheet$viewSingleSectionSpellcastingTable(section))
				]);
		} else {
			return _List_fromArray(
				[
					A3(
					$author$project$Page$PrintableCharSheet$viewBadgeDiv,
					'spellcasting',
					'spellcasting',
					$author$project$Page$PrintableCharSheet$viewMultiSectionSpellcastingTable(sections))
				]);
		}
	}
};
var $author$project$Page$PrintableCharSheet$view = function (sheet) {
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('dont-print')
				]),
			_List_fromArray(
				[
					$author$project$Elements$viewNavButtons(
					_List_fromArray(
						[
							$author$project$Elements$viewGotoCardsButton(sheet),
							$author$project$Elements$viewEditCharacterButton,
							$author$project$Elements$viewGotoEquipmentButton,
							$author$project$Elements$viewSelectCharacterButton
						]))
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('page')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('abilities')
						]),
					A2($author$project$Page$PrintableCharSheet$viewAbilities, sheet.ability_table, sheet.skill_table)),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('main-body')
						]),
					$author$project$Page$PrintableCharSheet$viewMainBody(sheet)),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('hit-dice-section')
						]),
					$author$project$Page$PrintableCharSheet$viewHitDiceSection(sheet.hit_dice))
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('page-break')
				]),
			_List_Nil),
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('page')
				]),
			A2(
				$elm$core$List$cons,
				A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('column')
						]),
					_Utils_ap(
						$author$project$Page$PrintableCharSheet$viewNotableTraits(sheet.notable_traits),
						A4($author$project$Page$PrintableCharSheet$viewOtherProficiencies, sheet.weapons, sheet.armor, sheet.languages, sheet.tools))),
				A2(
					$elm$core$List$cons,
					A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('column')
							]),
						_Utils_ap(
							$author$project$Page$PrintableCharSheet$viewSpellcastingTable(sheet.spellcasting_sections),
							_Utils_ap(
								$author$project$Page$PrintableCharSheet$viewSpellSlots(sheet.spell_slots),
								$author$project$Page$PrintableCharSheet$viewPactMagic(sheet.pact_magic)))),
					A2(
						$elm$core$List$cons,
						A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('column')
								]),
							$author$project$Page$PrintableCharSheet$viewResources(sheet.resources)),
						_List_Nil))))
		]);
};
var $author$project$Main$view = function (model) {
	return {
		body: A2(
			$elm$core$List$cons,
			A3(
				$elm$html$Html$node,
				'link',
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'href', 'https://fonts.googleapis.com/css2?family=Dosis:wght@400;700&display=swap'),
						A2($elm$html$Html$Attributes$attribute, 'rel', 'stylesheet')
					]),
				_List_Nil),
			A2(
				$elm$core$List$cons,
				A3(
					$elm$html$Html$node,
					'link',
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'href', 'css/printable-char-sheet.css'),
							A2($elm$html$Html$Attributes$attribute, 'rel', 'stylesheet')
						]),
					_List_Nil),
				A2(
					$elm$core$List$cons,
					$rtfeldman$elm_css$Html$Styled$toUnstyled($author$project$Main$globalCss),
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Html$Styled$toUnstyled,
						function () {
							var _v0 = model.page;
							switch (_v0.$) {
								case 'Loading':
									return _List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Loading...')
										]);
								case 'Error':
									var msg = _v0.a;
									return _List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(msg)
										]);
								case 'CharacterSelectionPage':
									var data = _v0.a;
									return A2($author$project$Main$characterSelectionPage, data, model);
								case 'CharacterSheetPage':
									var data = _v0.a;
									return A3($author$project$Page$CharacterSheet$view, model.preparedSpells, model.showOnlyPreparedSpells, data);
								case 'PrintableCharSheetPage':
									var data = _v0.a;
									return $author$project$Page$PrintableCharSheet$view(data);
								case 'EditCharacterPage':
									var data = _v0.a;
									return A2($author$project$Page$EditCharacter$view, model.focusedDropdownId, data);
								case 'CardsPage':
									var options = _v0.a;
									var data = _v0.b;
									return A3($author$project$Page$CardsPage$view, options, data, model.preparedSpells);
								default:
									var data = _v0.a;
									return $author$project$Page$Equipment$view(data);
							}
						}())))),
		title: 'Character Sheet'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Types$UrlChanged, onUrlRequest: $author$project$Types$LinkClicked, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$updateOrTick, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));