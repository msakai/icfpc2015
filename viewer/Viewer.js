"use strict";
// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof F) {
            f = E(B(f));
        }
        if(f instanceof PAP) {
            // f is a partial application
            if(args.length == f.arity) {
                // Saturated application
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                // Application is still unsaturated
                return new PAP(f.f, f.args.concat(args));
            } else {
                // Application is oversaturated; 
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else if(f instanceof Function) {
            if(args.length == f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,str.charCodeAt(i),new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1]));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,e];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, es[i], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, nl[i], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,elem];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,elem.childNodes[i]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,elem.childNodes[i]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, elem.childNodes[i], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(children[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1]));
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, ks[i], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,xhr.responseText],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

/* Utility functions for working with JSStrings. */

var _jss_singleton = String.fromCharCode;

function _jss_cons(c,s) {return String.fromCharCode(c)+s;}
function _jss_snoc(s,c) {return s+String.fromCharCode(c);}
function _jss_append(a,b) {return a+b;}
function _jss_len(s) {return s.length;}
function _jss_index(s,i) {return s.charCodeAt(i);}
function _jss_drop(s,i) {return s.substr(i);}
function _jss_substr(s,a,b) {return s.substr(a,b);}
function _jss_take(n,s) {return s.substr(0,n);}
// TODO: incorrect for some unusual characters.
function _jss_rev(s) {return s.split("").reverse().join("");}

function _jss_map(f,s) {
    f = E(f);
    var s2 = '';
    for(var i in s) {
        s2 += String.fromCharCode(E(f(s.charCodeAt(i))));
    }
    return s2;
}

function _jss_foldl(f,x,s) {
    f = E(f);
    for(var i in s) {
        x = A(f,[x,s.charCodeAt(i)]);
    }
    return x;
}

function _jss_re_match(s,re) {return s.search(re)>=0;}
function _jss_re_compile(re,fs) {return new RegExp(re,fs);}
function _jss_re_replace(s,re,rep) {return s.replace(re,rep);}

function _jss_re_find(re,s) {
    var a = s.match(re);
    return a ? mklst(a) : [0];
}

function mklst(arr) {
    var l = [0], len = arr.length-1;
    for(var i = 0; i <= len; ++i) {
        l = [1,arr[len-i],l];
    }
    return l;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as[0] === 1; as = as[2]) {
        arr.push(as[1]);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __jsTrue = function() {return true;}
var __jsFalse = function() {return false;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem],new T(function(){return __arr2lst(elem+1,arr);})]
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs[0] === 1; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0=function(_1){var _2=new T(function(){_1=E(_1);return fromJSStr(_1);});return new F(function(){return err(B(unAppCStr("Haste.JSON.!: unable to look up key ",_2)));});},_3=function(_4,_5){_4=E(_4);_5=E(_5);var _6=strEq(_4,_5);_6=E(_6);return (_6==0)?true:false;},_7=function(_8,_9){_8=E(_8);_9=E(_9);var _a=strEq(_8,_9);_a=E(_a);return (_a==0)?false:true;},_b=function(_c,_d){return new F(function(){return _7(_c,_d);});},_e=[0,_b,_3],_f=function(_g){_g=E(_g);return E(_g[1]);},_h=function(_i,_j,_k){while(1){_k=E(_k);if(!_k[0]){return [0];}else{var _l=_k[1];_l=E(_l);if(!B(A(_f,[_i,_j,_l[1]]))){var _m=_k[2];_k=_m;continue;}else{return [1,_l[2]];}}}},_n=function(_o,_p){_o=E(_o);if(_o[0]==4){var _q=B(_h(_e,_p,_o[1]));if(!_q[0]){return new F(function(){return _0(_p);});}else{return E(_q[1]);}}else{return new F(function(){return _0(_p);});}},_r=[0],_s=function(_t,_){_t=E(_t);if(!_t[0]){return _r;}else{var _u=_t[1],_v=B(_s(_t[2],_)),_w=new T(function(){_u=E(_u);var _x=Number(_u);return jsTrunc(_x);});return [1,_w,_v];}},_y=function(_z,_){var _A=__arr2lst(0,_z);return new F(function(){return _s(_A,_);});},_B=function(_C,_){_C=E(_C);return new F(function(){return _y(_C,_);});},_D=function(_E,_){return new T(function(){_E=E(_E);var _F=Number(_E);return jsTrunc(_F);});},_G=[0,_D,_B],_H=function(_I,_J){_I=E(_I);if(!_I[0]){return E(_J);}else{var _K=_I[2],_L=new T(function(){return B(_H(_K,_J));});return [1,_I[1],_L];}},_M=function(_N,_O){var _P=jsShowI(_N);return new F(function(){return _H(fromJSStr(_P),_O);});},_Q=41,_R=40,_S=function(_T,_U,_V){if(_U>=0){return new F(function(){return _M(_U,_V);});}else{if(_T<=6){return new F(function(){return _M(_U,_V);});}else{var _W=new T(function(){var _X=jsShowI(_U);return B(_H(fromJSStr(_X),[1,_Q,_V]));});return [1,_R,_W];}}},_Y=41,_Z=[1,_Y,_r],_10=new T(function(){return B(_S(0,2,_Z));}),_11=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_10));}),_12=function(_13){var _14=new T(function(){return B(_S(0,_13,_11));});return new F(function(){return err(B(unAppCStr("toEnum{MouseButton}: tag (",_14)));});},_15=function(_16,_){return new T(function(){_16=E(_16);var _17=Number(_16),_18=jsTrunc(_17);if(_18<0){return B(_12(_18));}else{if(_18>2){return B(_12(_18));}else{return _18;}}});},_19=function(_1a,_){_1a=E(_1a);if(!_1a[0]){return _r;}else{var _1b=B(_19(_1a[2],_));return [1,_1a[1],_1b];}},_1c=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_1d=new T(function(){return B(unCStr("base"));}),_1e=new T(function(){return B(unCStr("IOException"));}),_1f=new T(function(){var _1g=hs_wordToWord64(4053623282),_1h=hs_wordToWord64(3693590983);return [0,_1g,_1h,[0,_1g,_1h,_1d,_1c,_1e],_r];}),_1i=function(_1j){return E(_1f);},_1k=function(_1l){_1l=E(_1l);return E(_1l[1]);},_1m=function(_1n,_1o,_1p){var _1q=B(A(_1n,[_])),_1r=B(A(_1o,[_])),_1s=hs_eqWord64(_1q[1],_1r[1]);_1s=E(_1s);if(!_1s){return [0];}else{var _1t=hs_eqWord64(_1q[2],_1r[2]);_1t=E(_1t);return (_1t==0)?[0]:[1,_1p];}},_1u=function(_1v){_1v=E(_1v);return new F(function(){return _1m(B(_1k(_1v[1])),_1i,_1v[2]);});},_1w=new T(function(){return B(unCStr(": "));}),_1x=41,_1y=new T(function(){return B(unCStr(" ("));}),_1z=new T(function(){return B(unCStr("already exists"));}),_1A=new T(function(){return B(unCStr("does not exist"));}),_1B=new T(function(){return B(unCStr("protocol error"));}),_1C=new T(function(){return B(unCStr("failed"));}),_1D=new T(function(){return B(unCStr("invalid argument"));}),_1E=new T(function(){return B(unCStr("inappropriate type"));}),_1F=new T(function(){return B(unCStr("hardware fault"));}),_1G=new T(function(){return B(unCStr("unsupported operation"));}),_1H=new T(function(){return B(unCStr("timeout"));}),_1I=new T(function(){return B(unCStr("resource vanished"));}),_1J=new T(function(){return B(unCStr("interrupted"));}),_1K=new T(function(){return B(unCStr("resource busy"));}),_1L=new T(function(){return B(unCStr("resource exhausted"));}),_1M=new T(function(){return B(unCStr("end of file"));}),_1N=new T(function(){return B(unCStr("illegal operation"));}),_1O=new T(function(){return B(unCStr("permission denied"));}),_1P=new T(function(){return B(unCStr("user error"));}),_1Q=new T(function(){return B(unCStr("unsatisified constraints"));}),_1R=new T(function(){return B(unCStr("system error"));}),_1S=function(_1T,_1U){_1T=E(_1T);switch(_1T){case 0:return new F(function(){return _H(_1z,_1U);});break;case 1:return new F(function(){return _H(_1A,_1U);});break;case 2:return new F(function(){return _H(_1K,_1U);});break;case 3:return new F(function(){return _H(_1L,_1U);});break;case 4:return new F(function(){return _H(_1M,_1U);});break;case 5:return new F(function(){return _H(_1N,_1U);});break;case 6:return new F(function(){return _H(_1O,_1U);});break;case 7:return new F(function(){return _H(_1P,_1U);});break;case 8:return new F(function(){return _H(_1Q,_1U);});break;case 9:return new F(function(){return _H(_1R,_1U);});break;case 10:return new F(function(){return _H(_1B,_1U);});break;case 11:return new F(function(){return _H(_1C,_1U);});break;case 12:return new F(function(){return _H(_1D,_1U);});break;case 13:return new F(function(){return _H(_1E,_1U);});break;case 14:return new F(function(){return _H(_1F,_1U);});break;case 15:return new F(function(){return _H(_1G,_1U);});break;case 16:return new F(function(){return _H(_1H,_1U);});break;case 17:return new F(function(){return _H(_1I,_1U);});break;default:return new F(function(){return _H(_1J,_1U);});}},_1V=125,_1W=new T(function(){return B(unCStr("{handle: "));}),_1X=function(_1Y,_1Z,_20,_21,_22,_23){var _24=new T(function(){var _25=new T(function(){var _26=new T(function(){_21=E(_21);if(!_21[0]){return E(_23);}else{var _27=new T(function(){return B(_H(_21,[1,_1x,_23]));},1);return B(_H(_1y,_27));}},1);return B(_1S(_1Z,_26));},1);_20=E(_20);if(!_20[0]){return E(_25);}else{var _28=new T(function(){return B(_H(_1w,_25));},1);return B(_H(_20,_28));}},1);_22=E(_22);if(!_22[0]){_1Y=E(_1Y);if(!_1Y[0]){return E(_24);}else{var _29=_1Y[1];_29=E(_29);if(!_29[0]){var _2a=_29[1],_2b=new T(function(){var _2c=new T(function(){return B(_H(_1w,_24));});return B(_H(_2a,[1,_1V,_2c]));},1);return new F(function(){return _H(_1W,_2b);});}else{var _2d=_29[1],_2e=new T(function(){var _2f=new T(function(){return B(_H(_1w,_24));});return B(_H(_2d,[1,_1V,_2f]));},1);return new F(function(){return _H(_1W,_2e);});}}}else{var _2g=new T(function(){return B(_H(_1w,_24));},1);return new F(function(){return _H(_22[1],_2g);});}},_2h=function(_2i){_2i=E(_2i);return new F(function(){return _1X(_2i[1],_2i[2],_2i[3],_2i[4],_2i[6],_r);});},_2j=function(_2k,_2l){_2k=E(_2k);return new F(function(){return _1X(_2k[1],_2k[2],_2k[3],_2k[4],_2k[6],_2l);});},_2m=44,_2n=93,_2o=91,_2p=function(_2q,_2r,_2s){_2r=E(_2r);if(!_2r[0]){return new F(function(){return unAppCStr("[]",_2s);});}else{var _2t=_2r[1],_2u=_2r[2],_2v=new T(function(){var _2w=new T(function(){var _2x=[1,_2n,_2s],_2y=function(_2z){_2z=E(_2z);if(!_2z[0]){return E(_2x);}else{var _2A=_2z[1],_2B=_2z[2],_2C=new T(function(){var _2D=new T(function(){return B(_2y(_2B));});return B(A(_2q,[_2A,_2D]));});return [1,_2m,_2C];}};return B(_2y(_2u));});return B(A(_2q,[_2t,_2w]));});return [1,_2o,_2v];}},_2E=function(_2F,_2G){return new F(function(){return _2p(_2j,_2F,_2G);});},_2H=function(_2I,_2J,_2K){_2J=E(_2J);return new F(function(){return _1X(_2J[1],_2J[2],_2J[3],_2J[4],_2J[6],_2K);});},_2L=[0,_2H,_2h,_2E],_2M=new T(function(){return [0,_1i,_2L,_2N,_1u];}),_2N=function(_2O){return [0,_2M,_2O];},_2P=[0],_2Q=7,_2R=function(_2S){return [0,_2P,_2Q,_r,_2S,_2P,_2P];},_2T=function(_2U,_){var _2V=new T(function(){var _2W=new T(function(){return B(_2R(_2U));});return B(_2N(_2W));});return new F(function(){return die(_2V);});},_2X=function(_2Y,_){return new F(function(){return _2T(_2Y,_);});},_2Z=new T(function(){return B(unCStr("Pattern match failure in do expression at /var/folders/6b/kfb6m36d3nx19tdfydttmskw0000gn/T/ghc20502_0/ghc20502_28.hscpp:288:5-9"));}),_30=function(_){return new F(function(){return _2X(_2Z,_);});},_31=function(_32){_32=E(_32);return E(_32[1]);},_33=function(_34,_35,_36,_){var _37=__arr2lst(0,_36),_38=B(_19(_37,_));_38=E(_38);if(!_38[0]){return new F(function(){return _30(_);});}else{var _39=_38[2];_39=E(_39);if(!_39[0]){return new F(function(){return _30(_);});}else{var _3a=_39[2];_3a=E(_3a);if(!_3a[0]){var _3b=B(A(_31,[_34,_38[1],_])),_3c=B(A(_31,[_35,_39[1],_]));return [0,_3b,_3c];}else{return new F(function(){return _30(_);});}}}},_3d=new T(function(){return jsGetMouseCoords;}),_3e=0,_3f=[0,_3e,_3e,_3e],_3g="deltaX",_3h="deltaY",_3i="deltaZ",_3j="button",_3k=function(_3l,_3m,_){_3l=E(_3l);if(_3l==7){_3d=E(_3d);var _3n=_3d(_3m),_3o=B(_33(_G,_G,_3n,_)),_3p=_3o;_3g=E(_3g);var _3q=_3m[_3g],_3r=_3q;_3h=E(_3h);var _3s=_3m[_3h],_3t=_3s;_3i=E(_3i);var _3u=_3m[_3i],_3v=_3u;return new T(function(){_3p=E(_3p);var _3w=new T(function(){return Number(_3v);}),_3x=new T(function(){return Number(_3t);}),_3y=new T(function(){return Number(_3r);});return [0,E(_3p),E(_2P),[0,_3y,_3x,_3w]];});}else{_3d=E(_3d);var _3z=_3d(_3m),_3A=B(_33(_G,_G,_3z,_)),_3B=_3A;_3j=E(_3j);var _3C=_3m[_3j],_3D=__jsNull(),_3E=__eq(_3C,_3D);_3E=E(_3E);if(!_3E){var _3F=B(_15(_3C,_)),_3G=_3F;return new T(function(){_3B=E(_3B);return [0,E(_3B),[1,_3G],E(_3f)];});}else{return new T(function(){_3B=E(_3B);return [0,E(_3B),E(_2P),E(_3f)];});}}},_3H=function(_3I,_3J,_){_3J=E(_3J);return new F(function(){return _3k(_3I,_3J,_);});},_3K="wheel",_3L="mouseout",_3M="mouseover",_3N="mousemove",_3O="mouseup",_3P="mousedown",_3Q="dblclick",_3R="click",_3S=function(_3T){_3T=E(_3T);switch(_3T){case 0:return E(_3R);case 1:return E(_3Q);case 2:return E(_3P);case 3:return E(_3O);case 4:return E(_3N);case 5:return E(_3M);case 6:return E(_3L);default:return E(_3K);}},_3U=[0,_3S,_3H],_3V=function(_3W,_){return [1,_3W];},_3X=function(_3Y){return E(_3Y);},_3Z=[0,_3X,_3V],_40=function(_41,_){return new F(function(){return _2X(_41,_);});},_42=function(_43,_){return new F(function(){return _40(_43,_);});},_44=function(_45,_46,_){var _47=B(A(_45,[_]));return new F(function(){return A(_46,[_47,_]);});},_48=function(_49,_){return _49;},_4a=function(_4b,_4c,_){var _4d=B(A(_4b,[_]));return new F(function(){return A(_4c,[_]);});},_4e=[0,_44,_4a,_48,_42],_4f=function(_4g,_4h){_4g=E(_4g);_4h=E(_4h);return _4g-_4h|0;},_4i=function(_4j,_4k){if(_4j<=_4k){var _4l=function(_4m){var _4n=new T(function(){if(_4m!=_4k){return B(_4l(_4m+1|0));}else{return [0];}});return [1,_4m,_4n];};return new F(function(){return _4l(_4j);});}else{return [0];}},_4o=function(_4p){_4p=E(_4p);return new F(function(){return _4i(_4p,2147483647);});},_4q=function(_4r,_4s,_4t){if(_4t<=_4s){var _4u=new T(function(){var _4v=_4s-_4r|0,_4w=_4t-_4v|0,_4x=function(_4y){if(_4y>=_4w){var _4z=new T(function(){return B(_4x(_4y+_4v|0));});return [1,_4y,_4z];}else{return [1,_4y,_r];}};return B(_4x(_4s));});return [1,_4r,_4u];}else{return (_4t<=_4r)?[1,_4r,_r]:[0];}},_4A=function(_4B,_4C,_4D){if(_4D>=_4C){var _4E=new T(function(){var _4F=_4C-_4B|0,_4G=_4D-_4F|0,_4H=function(_4I){if(_4I<=_4G){var _4J=new T(function(){return B(_4H(_4I+_4F|0));});return [1,_4I,_4J];}else{return [1,_4I,_r];}};return B(_4H(_4C));});return [1,_4B,_4E];}else{return (_4D>=_4B)?[1,_4B,_r]:[0];}},_4K=function(_4L,_4M){if(_4M<_4L){return new F(function(){return _4q(_4L,_4M,-2147483648);});}else{return new F(function(){return _4A(_4L,_4M,2147483647);});}},_4N=function(_4O,_4P){_4O=E(_4O);_4P=E(_4P);return new F(function(){return _4K(_4O,_4P);});},_4Q=function(_4R,_4S,_4T){if(_4S<_4R){return new F(function(){return _4q(_4R,_4S,_4T);});}else{return new F(function(){return _4A(_4R,_4S,_4T);});}},_4U=function(_4V,_4W,_4X){_4V=E(_4V);_4W=E(_4W);_4X=E(_4X);return new F(function(){return _4Q(_4V,_4W,_4X);});},_4Y=function(_4Z,_50){_4Z=E(_4Z);_50=E(_50);return new F(function(){return _4i(_4Z,_50);});},_51=function(_52){return E(_52);},_53=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_54=new T(function(){return B(err(_53));}),_55=function(_56){_56=E(_56);return (_56==(-2147483648))?E(_54):_56-1|0;},_57=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_58=new T(function(){return B(err(_57));}),_59=function(_5a){_5a=E(_5a);return (_5a==2147483647)?E(_58):_5a+1|0;},_5b=[0,_59,_55,_51,_51,_4o,_4N,_4Y,_4U],_5c=function(_5d,_5e){if(_5d<=0){if(_5d>=0){return new F(function(){return quot(_5d,_5e);});}else{if(_5e<=0){return new F(function(){return quot(_5d,_5e);});}else{return quot(_5d+1|0,_5e)-1|0;}}}else{if(_5e>=0){if(_5d>=0){return new F(function(){return quot(_5d,_5e);});}else{if(_5e<=0){return new F(function(){return quot(_5d,_5e);});}else{return quot(_5d+1|0,_5e)-1|0;}}}else{return quot(_5d-1|0,_5e)-1|0;}}},_5f=new T(function(){return B(unCStr("ArithException"));}),_5g=new T(function(){return B(unCStr("GHC.Exception"));}),_5h=new T(function(){return B(unCStr("base"));}),_5i=new T(function(){var _5j=hs_wordToWord64(4194982440),_5k=hs_wordToWord64(3110813675);return [0,_5j,_5k,[0,_5j,_5k,_5h,_5g,_5f],_r];}),_5l=function(_5m){return E(_5i);},_5n=function(_5o){_5o=E(_5o);return new F(function(){return _1m(B(_1k(_5o[1])),_5l,_5o[2]);});},_5p=new T(function(){return B(unCStr("arithmetic underflow"));}),_5q=new T(function(){return B(unCStr("arithmetic overflow"));}),_5r=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_5s=new T(function(){return B(unCStr("denormal"));}),_5t=new T(function(){return B(unCStr("divide by zero"));}),_5u=new T(function(){return B(unCStr("loss of precision"));}),_5v=function(_5w){_5w=E(_5w);switch(_5w){case 0:return E(_5q);case 1:return E(_5p);case 2:return E(_5u);case 3:return E(_5t);case 4:return E(_5s);default:return E(_5r);}},_5x=function(_5y){return new F(function(){return _H(_5p,_5y);});},_5z=function(_5y){return new F(function(){return _H(_5q,_5y);});},_5A=function(_5y){return new F(function(){return _H(_5r,_5y);});},_5B=function(_5y){return new F(function(){return _H(_5s,_5y);});},_5C=function(_5y){return new F(function(){return _H(_5t,_5y);});},_5D=function(_5y){return new F(function(){return _H(_5u,_5y);});},_5E=function(_5F){_5F=E(_5F);switch(_5F){case 0:return E(_5z);case 1:return E(_5x);case 2:return E(_5D);case 3:return E(_5C);case 4:return E(_5B);default:return E(_5A);}},_5G=function(_5H,_5I){return new F(function(){return _2p(_5E,_5H,_5I);});},_5J=function(_5K,_5L){_5L=E(_5L);switch(_5L){case 0:return E(_5z);case 1:return E(_5x);case 2:return E(_5D);case 3:return E(_5C);case 4:return E(_5B);default:return E(_5A);}},_5M=[0,_5J,_5v,_5G],_5N=new T(function(){return [0,_5l,_5M,_5O,_5n];}),_5O=function(_5y){return [0,_5N,_5y];},_5P=3,_5Q=new T(function(){return B(_5O(_5P));}),_5R=new T(function(){return die(_5Q);}),_5S=0,_5T=new T(function(){return B(_5O(_5S));}),_5U=new T(function(){return die(_5T);}),_5V=function(_5W,_5X){_5X=E(_5X);switch(_5X){case -1:_5W=E(_5W);if(_5W==(-2147483648)){return E(_5U);}else{return new F(function(){return _5c(_5W,-1);});}break;case 0:return E(_5R);default:return new F(function(){return _5c(_5W,_5X);});}},_5Y=function(_5Z,_60){_5Z=E(_5Z);_60=E(_60);return new F(function(){return _5V(_5Z,_60);});},_61=0,_62=[0,_5U,_61],_63=function(_64,_65){_64=E(_64);_65=E(_65);switch(_65){case -1:_64=E(_64);if(_64==(-2147483648)){return E(_62);}else{if(_64<=0){if(_64>=0){var _66=quotRemI(_64,-1);return [0,_66[1],_66[2]];}else{var _67=quotRemI(_64,-1);return [0,_67[1],_67[2]];}}else{var _68=quotRemI(_64-1|0,-1);return [0,_68[1]-1|0,(_68[2]+(-1)|0)+1|0];}}break;case 0:return E(_5R);default:if(_64<=0){if(_64>=0){var _69=quotRemI(_64,_65);return [0,_69[1],_69[2]];}else{if(_65<=0){var _6a=quotRemI(_64,_65);return [0,_6a[1],_6a[2]];}else{var _6b=quotRemI(_64+1|0,_65);return [0,_6b[1]-1|0,(_6b[2]+_65|0)-1|0];}}}else{if(_65>=0){if(_64>=0){var _6c=quotRemI(_64,_65);return [0,_6c[1],_6c[2]];}else{if(_65<=0){var _6d=quotRemI(_64,_65);return [0,_6d[1],_6d[2]];}else{var _6e=quotRemI(_64+1|0,_65);return [0,_6e[1]-1|0,(_6e[2]+_65|0)-1|0];}}}else{var _6f=quotRemI(_64-1|0,_65);return [0,_6f[1]-1|0,(_6f[2]+_65|0)+1|0];}}}},_6g=function(_6h,_6i){var _6j=_6h%_6i;if(_6h<=0){if(_6h>=0){return E(_6j);}else{if(_6i<=0){return E(_6j);}else{_6j=E(_6j);return (_6j==0)?0:_6j+_6i|0;}}}else{if(_6i>=0){if(_6h>=0){return E(_6j);}else{if(_6i<=0){return E(_6j);}else{_6j=E(_6j);return (_6j==0)?0:_6j+_6i|0;}}}else{_6j=E(_6j);return (_6j==0)?0:_6j+_6i|0;}}},_6k=function(_6l,_6m){_6m=E(_6m);switch(_6m){case -1:return E(_61);case 0:return E(_5R);default:_6l=E(_6l);return new F(function(){return _6g(_6l,_6m);});}},_6n=function(_6o,_6p){_6o=E(_6o);_6p=E(_6p);switch(_6p){case -1:_6o=E(_6o);if(_6o==(-2147483648)){return E(_5U);}else{return new F(function(){return quot(_6o,-1);});}break;case 0:return E(_5R);default:return new F(function(){return quot(_6o,_6p);});}},_6q=function(_6r,_6s){_6r=E(_6r);_6s=E(_6s);switch(_6s){case -1:_6r=E(_6r);if(_6r==(-2147483648)){return E(_62);}else{var _6t=quotRemI(_6r,-1);return [0,_6t[1],_6t[2]];}break;case 0:return E(_5R);default:var _6u=quotRemI(_6r,_6s);return [0,_6u[1],_6u[2]];}},_6v=function(_6w,_6x){_6x=E(_6x);switch(_6x){case -1:return E(_61);case 0:return E(_5R);default:_6w=E(_6w);return _6w%_6x;}},_6y=function(_6z){return [0,_6z];},_6A=function(_6B){_6B=E(_6B);return new F(function(){return _6y(_6B);});},_6C=[0,1],_6D=function(_6E){_6E=E(_6E);return [0,E(B(_6y(_6E))),E(_6C)];},_6F=function(_6G,_6H){_6G=E(_6G);_6H=E(_6H);return imul(_6G,_6H)|0;},_6I=function(_6J,_6K){_6J=E(_6J);_6K=E(_6K);return _6J+_6K|0;},_6L=function(_6M){_6M=E(_6M);return (_6M<0)? -_6M:E(_6M);},_6N=function(_6O){_6O=E(_6O);if(!_6O[0]){return E(_6O[1]);}else{return new F(function(){return I_toInt(_6O[1]);});}},_6P=function(_6Q){return new F(function(){return _6N(_6Q);});},_6R=function(_6S){_6S=E(_6S);return  -_6S;},_6T=-1,_6U=0,_6V=1,_6W=function(_6X){_6X=E(_6X);if(_6X>=0){_6X=E(_6X);return (_6X==0)?E(_6U):E(_6V);}else{return E(_6T);}},_6Y=[0,_6I,_6F,_4f,_6R,_6L,_6W,_6P],_6Z=function(_70,_71){_70=E(_70);_71=E(_71);return _70==_71;},_72=function(_73,_74){_73=E(_73);_74=E(_74);return _73!=_74;},_75=[0,_6Z,_72],_76=function(_77,_78){_77=E(_77);_78=E(_78);return (_77>_78)?E(_77):E(_78);},_79=function(_7a,_7b){_7a=E(_7a);_7b=E(_7b);return (_7a>_7b)?E(_7b):E(_7a);},_7c=function(_7d,_7e){return (_7d>=_7e)?(_7d!=_7e)?2:1:0;},_7f=function(_7g,_7h){_7g=E(_7g);_7h=E(_7h);return new F(function(){return _7c(_7g,_7h);});},_7i=function(_7j,_7k){_7j=E(_7j);_7k=E(_7k);return _7j>=_7k;},_7l=function(_7m,_7n){_7m=E(_7m);_7n=E(_7n);return _7m>_7n;},_7o=function(_7p,_7q){_7p=E(_7p);_7q=E(_7q);return _7p<=_7q;},_7r=function(_7s,_7t){_7s=E(_7s);_7t=E(_7t);return _7s<_7t;},_7u=[0,_75,_7f,_7r,_7i,_7l,_7o,_76,_79],_7v=[0,_6Y,_7u,_6D],_7w=[0,_7v,_5b,_6n,_6v,_5Y,_6k,_6q,_63,_6A],_7x=[0,1],_7y=function(_7z){_7z=E(_7z);return E(_7z[1]);},_7A=[0,0],_7B=0,_7C=[0,2],_7D=function(_7E){_7E=E(_7E);return E(_7E[1]);},_7F=function(_7G,_7H){while(1){_7G=E(_7G);if(!_7G[0]){var _7I=_7G[1];_7H=E(_7H);if(!_7H[0]){var _7J=_7H[1];if(!(imul(_7I,_7J)|0)){return [0,imul(_7I,_7J)|0];}else{_7G=[1,I_fromInt(_7I)];_7H=[1,I_fromInt(_7J)];continue;}}else{_7G=[1,I_fromInt(_7I)];continue;}}else{_7H=E(_7H);if(!_7H[0]){var _7K=[1,I_fromInt(_7H[1])];_7H=_7K;continue;}else{return [1,I_mul(_7G[1],_7H[1])];}}}},_7L=function(_7M,_7N,_7O){while(1){if(!(_7N%2)){var _7P=B(_7F(_7M,_7M)),_7Q=quot(_7N,2);_7M=_7P;_7N=_7Q;continue;}else{_7N=E(_7N);if(_7N==1){return new F(function(){return _7F(_7M,_7O);});}else{var _7P=B(_7F(_7M,_7M)),_7Q=quot(_7N-1|0,2),_7R=B(_7F(_7M,_7O));_7M=_7P;_7N=_7Q;_7O=_7R;continue;}}}},_7S=function(_7T,_7U){while(1){if(!(_7U%2)){var _7V=B(_7F(_7T,_7T)),_7W=quot(_7U,2);_7T=_7V;_7U=_7W;continue;}else{_7U=E(_7U);if(_7U==1){return E(_7T);}else{return new F(function(){return _7L(B(_7F(_7T,_7T)),quot(_7U-1|0,2),_7T);});}}}},_7X=function(_7Y){_7Y=E(_7Y);return E(_7Y[2]);},_7Z=function(_80){_80=E(_80);return E(_80[1]);},_81=function(_82){_82=E(_82);return E(_82[2]);},_83=[0,0],_84=[0,2],_85=function(_86){_86=E(_86);return E(_86[7]);},_87=function(_88,_89,_8a,_8b,_8c){_89=E(_89);var _8d=_89[1];_8d=E(_8d);var _8e=new T(function(){return B(A(_85,[_88,_83]));}),_8f=new T(function(){var _8g=new T(function(){return B(A(_85,[_88,_84]));});return B(A(_8b,[_8c,_8g]));});return new F(function(){return A(_8d[1],[_8f,_8e]);});},_8h=function(_8i){_8i=E(_8i);return E(_8i[3]);},_8j=new T(function(){return B(unCStr("Negative exponent"));}),_8k=new T(function(){return B(err(_8j));}),_8l=function(_8m,_8n,_8o,_8p){var _8q=B(_7y(_8n)),_8r=_8q[1],_8s=_8q[2];_8s=E(_8s);var _8t=_8s[1];_8t=E(_8t);var _8u=new T(function(){return B(A(_85,[_8r,_83]));});if(!B(A(_8s[3],[_8p,_8u]))){var _8v=new T(function(){return B(A(_85,[_8r,_83]));});if(!B(A(_8t[1],[_8p,_8v]))){var _8w=B(_7y(_8n)),_8x=_8w[1],_8y=_8w[2];_8y=E(_8y);var _8z=_8y[1];_8z=E(_8z);var _8A=_8z[1],_8B=new T(function(){return B(_7y(_8n));}),_8C=new T(function(){return B(_7D(_8B));}),_8D=new T(function(){return B(A(_85,[_8C,_6C]));}),_8E=new T(function(){return B(A(_85,[_8C,_6C]));}),_8F=new T(function(){return B(A(_85,[_8C,_84]));}),_8G=new T(function(){return B(A(_85,[_8C,_84]));}),_8H=new T(function(){return B(_81(_8B));},1),_8I=new T(function(){return B(_7Z(_8H));}),_8J=new T(function(){return B(_f(_8I));}),_8K=new T(function(){return B(_8h(_8C));}),_8L=new T(function(){return B(_7X(_8m));}),_8M=new T(function(){return B(_7X(_8m));}),_8N=new T(function(){return B(_7X(_8m));}),_8O=new T(function(){return B(_7X(_8m));}),_8P=function(_8Q,_8R,_8S){while(1){var _8T=(function(_8U,_8V,_8W){_8n=E(_8n);var _8X=_8n[1],_8Y=_8n[3];_8X=E(_8X);if(!B(_87(_8X[1],_8X[2],_8X[3],_8n[4],_8V))){if(!B(A(_8J,[_8V,_8D]))){var _8Z=new T(function(){return B(A(_8N,[_8U,_8W]));}),_90=new T(function(){var _91=new T(function(){return B(A(_8K,[_8V,_8E]));});return B(A(_8Y,[_91,_8F]));}),_92=new T(function(){return B(A(_8O,[_8U,_8U]));});_8Q=_92;_8R=_90;_8S=_8Z;return null;}else{return new F(function(){return A(_8M,[_8U,_8W]);});}}else{var _93=new T(function(){return B(A(_8Y,[_8V,_8G]));}),_94=new T(function(){return B(A(_8L,[_8U,_8U]));});_8Q=_94;_8R=_93;var _95=_8W;_8S=_95;return null;}})(_8Q,_8R,_8S);if(_8T!=null){return _8T;}}},_96=new T(function(){return B(A(_85,[_8x,_6C]));}),_97=new T(function(){return B(A(_85,[_8x,_84]));}),_98=new T(function(){return B(A(_85,[_8x,_6C]));}),_99=new T(function(){return B(A(_85,[_8x,_84]));}),_9a=new T(function(){return B(_8h(_8x));}),_9b=new T(function(){return B(_7X(_8m));}),_9c=new T(function(){return B(_7X(_8m));}),_9d=_8o,_9e=_8p;while(1){var _9f=(function(_9g,_9h){_8n=E(_8n);var _9i=_8n[1],_9j=_8n[3];_9i=E(_9i);if(!B(_87(_9i[1],_9i[2],_9i[3],_8n[4],_9h))){if(!B(A(_8A,[_9h,_98]))){var _9k=new T(function(){var _9l=new T(function(){return B(A(_9a,[_9h,_96]));});return B(A(_9j,[_9l,_97]));}),_9m=new T(function(){return B(A(_9c,[_9g,_9g]));});return new F(function(){return _8P(_9m,_9k,_9g);});}else{return E(_9g);}}else{var _9n=new T(function(){return B(A(_9j,[_9h,_99]));}),_9o=new T(function(){return B(A(_9b,[_9g,_9g]));});_9d=_9o;_9e=_9n;return null;}})(_9d,_9e);if(_9f!=null){return _9f;}}}else{return new F(function(){return A(_85,[_8m,_6C]);});}}else{return E(_8k);}},_9p=new T(function(){return B(err(_8j));}),_9q=function(_9r){var _9s=I_decodeDouble(_9r);return [0,[1,_9s[2]],_9s[1]];},_9t=function(_9u,_9v){_9u=E(_9u);return (_9u[0]==0)?_9u[1]*Math.pow(2,_9v):I_toNumber(_9u[1])*Math.pow(2,_9v);},_9w=function(_9x,_9y){_9x=E(_9x);if(!_9x[0]){var _9z=_9x[1];_9y=E(_9y);return (_9y[0]==0)?_9z==_9y[1]:(I_compareInt(_9y[1],_9z)==0)?true:false;}else{var _9A=_9x[1];_9y=E(_9y);return (_9y[0]==0)?(I_compareInt(_9A,_9y[1])==0)?true:false:(I_compare(_9A,_9y[1])==0)?true:false;}},_9B=function(_9C,_9D){while(1){_9C=E(_9C);if(!_9C[0]){var _9E=_9C[1];_9E=E(_9E);if(_9E==(-2147483648)){_9C=[1,I_fromInt(-2147483648)];continue;}else{_9D=E(_9D);if(!_9D[0]){var _9F=_9D[1];return [0,[0,quot(_9E,_9F)],[0,_9E%_9F]];}else{_9C=[1,I_fromInt(_9E)];continue;}}}else{_9D=E(_9D);if(!_9D[0]){var _9G=[1,I_fromInt(_9D[1])];_9D=_9G;continue;}else{var _9H=I_quotRem(_9C[1],_9D[1]);return [0,[1,_9H[1]],[1,_9H[2]]];}}}},_9I=function(_9J,_9K){var _9L=B(_9q(_9K)),_9M=_9L[1],_9N=_9L[2],_9O=new T(function(){return B(_7y(_9J));},1),_9P=new T(function(){return B(_7D(_9O));});if(_9N<0){var _9Q= -_9N;if(_9Q>=0){_9Q=E(_9Q);if(!_9Q){var _9R=E(_6C);}else{var _9R=B(_7S(_7C,_9Q));}if(!B(_9w(_9R,_7A))){var _9S=B(_9B(_9M,_9R)),_9T=_9S[1],_9U=_9S[2],_9V=new T(function(){return B(_9t(_9U,_9N));}),_9W=new T(function(){return B(A(_85,[_9P,_9T]));});return [0,_9W,_9V];}else{return E(_5R);}}else{return E(_9p);}}else{var _9X=new T(function(){var _9Y=new T(function(){var _9Z=new T(function(){return B(A(_85,[_9P,_7C]));});return B(_8l(_9P,_7w,_9Z,_9N));}),_a0=new T(function(){return B(A(_85,[_9P,_9M]));});return B(A(_7X,[_9P,_a0,_9Y]));});return [0,_9X,_7B];}},_a1=function(_a2,_a3){var _a4=B(_9I(_a2,_a3)),_a5=_a4[1],_a6=_a4[2];_a6=E(_a6);var _a7=_a6,_a8=new T(function(){var _a9=B(_7y(_a2))[1];_a9=E(_a9);var _aa=_a9[7];if(_a7>=0){var _ab=new T(function(){return B(A(_aa,[_7x]));});return B(A(_a9[1],[_a5,_ab]));}else{var _ac=new T(function(){return B(A(_aa,[_7x]));});return B(A(_a9[3],[_a5,_ac]));}},1);if(_a7<0){var _ad= -_a7-0.5;if(_ad>=0){_ad=E(_ad);if(!_ad){_a2=E(_a2);var _ae=_a2[1];_ae=E(_ae);return (!B(_87(_ae[1],_ae[2],_ae[3],_a2[4],_a5)))?E(_a8):E(_a5);}else{return E(_a8);}}else{return E(_a5);}}else{var _af=_a7-0.5;if(_af>=0){_af=E(_af);if(!_af){_a2=E(_a2);var _ag=_a2[1];_ag=E(_ag);return (!B(_87(_ag[1],_ag[2],_ag[3],_a2[4],_a5)))?E(_a8):E(_a5);}else{return E(_a8);}}else{return E(_a5);}}},_ah=new T(function(){return B(unCStr("Control.Exception.Base"));}),_ai=new T(function(){return B(unCStr("base"));}),_aj=new T(function(){return B(unCStr("PatternMatchFail"));}),_ak=new T(function(){var _al=hs_wordToWord64(18445595),_am=hs_wordToWord64(52003073);return [0,_al,_am,[0,_al,_am,_ai,_ah,_aj],_r];}),_an=function(_ao){return E(_ak);},_ap=function(_aq){_aq=E(_aq);return new F(function(){return _1m(B(_1k(_aq[1])),_an,_aq[2]);});},_ar=function(_as){_as=E(_as);return E(_as[1]);},_at=function(_au,_av){_au=E(_au);return new F(function(){return _H(_au[1],_av);});},_aw=function(_ax,_ay){return new F(function(){return _2p(_at,_ax,_ay);});},_az=function(_aA,_aB,_aC){_aB=E(_aB);return new F(function(){return _H(_aB[1],_aC);});},_aD=[0,_az,_ar,_aw],_aE=new T(function(){return [0,_an,_aD,_aF,_ap];}),_aF=function(_aG){return [0,_aE,_aG];},_aH=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_aI=function(_aJ,_aK){var _aL=new T(function(){return B(A(_aK,[_aJ]));});return new F(function(){return die(_aL);});},_aM=function(_aN,_aO){_aO=E(_aO);if(!_aO[0]){return [0,_r,_r];}else{var _aP=_aO[1],_aQ=_aO[2];if(!B(A(_aN,[_aP]))){return [0,_r,_aO];}else{var _aR=new T(function(){var _aS=B(_aM(_aN,_aQ));return [0,_aS[1],_aS[2]];}),_aT=new T(function(){_aR=E(_aR);return E(_aR[2]);}),_aU=new T(function(){_aR=E(_aR);return E(_aR[1]);});return [0,[1,_aP,_aU],_aT];}}},_aV=32,_aW=10,_aX=[1,_aW,_r],_aY=function(_aZ){_aZ=E(_aZ);return (_aZ==124)?false:true;},_b0=function(_b1,_b2){var _b3=B(_aM(_aY,B(unCStr(_b1)))),_b4=_b3[1],_b5=_b3[2],_b6=function(_b7,_b8){var _b9=new T(function(){var _ba=new T(function(){var _bb=new T(function(){return B(_H(_b8,_aX));},1);return B(_H(_b2,_bb));});return B(unAppCStr(": ",_ba));},1);return new F(function(){return _H(_b7,_b9);});};_b5=E(_b5);if(!_b5[0]){return new F(function(){return _b6(_b4,_r);});}else{var _bc=_b5[1];_bc=E(_bc);if(_bc==124){return new F(function(){return _b6(_b4,[1,_aV,_b5[2]]);});}else{return new F(function(){return _b6(_b4,_r);});}}},_bd=function(_be){var _bf=new T(function(){return B(_b0(_be,_aH));});return new F(function(){return _aI([0,_bf],_aF);});},_bg=new T(function(){return B(_bd("Viewer.hs:44:15-32|Haste.JSON.Num x"));}),_bh=120,_bi=[1,_bh,_r],_bj=new T(function(){return toJSStr(_bi);}),_bk=new T(function(){return B(_bd("Viewer.hs:45:15-32|Haste.JSON.Num y"));}),_bl=121,_bm=[1,_bl,_r],_bn=new T(function(){return toJSStr(_bm);}),_bo=function(_bp){var _bq=new T(function(){var _br=B(_n(_bp,_bn));if(!_br[0]){return B(_a1(_7w,_br[1]));}else{return E(_bk);}}),_bs=new T(function(){var _bt=B(_n(_bp,_bj));if(!_bt[0]){return B(_a1(_7w,_bt[1]));}else{return E(_bg);}});return [0,_bs,_bq];},_bu=function(_bv){var _bw=B(_bo(_bv));return [0,_bw[1],_bw[2]];},_bx=[1],_by=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_bz=function(_bA){return new F(function(){return err(_by);});},_bB=new T(function(){return B(_bz(_));}),_bC=function(_bD,_bE,_bF,_bG){_bF=E(_bF);if(!_bF[0]){var _bH=_bF[1];_bG=E(_bG);if(!_bG[0]){var _bI=_bG[1],_bJ=_bG[2],_bK=_bG[3],_bL=_bG[4],_bM=_bG[5];if(_bI<=(imul(3,_bH)|0)){_bD=E(_bD);return [0,(1+_bH|0)+_bI|0,E(_bD),_bE,E(_bF),E(_bG)];}else{_bL=E(_bL);if(!_bL[0]){var _bN=_bL[1],_bO=_bL[2],_bP=_bL[3],_bQ=_bL[4],_bR=_bL[5];_bM=E(_bM);if(!_bM[0]){var _bS=_bM[1];if(_bN>=(imul(2,_bS)|0)){var _bT=function(_bU){_bD=E(_bD);_bR=E(_bR);return (_bR[0]==0)?[0,(1+_bH|0)+_bI|0,E(_bO),_bP,[0,(1+_bH|0)+_bU|0,E(_bD),_bE,E(_bF),E(_bQ)],[0,(1+_bS|0)+_bR[1]|0,E(_bJ),_bK,E(_bR),E(_bM)]]:[0,(1+_bH|0)+_bI|0,E(_bO),_bP,[0,(1+_bH|0)+_bU|0,E(_bD),_bE,E(_bF),E(_bQ)],[0,1+_bS|0,E(_bJ),_bK,E(_bx),E(_bM)]];};_bQ=E(_bQ);if(!_bQ[0]){return new F(function(){return _bT(_bQ[1]);});}else{return new F(function(){return _bT(0);});}}else{_bD=E(_bD);return [0,(1+_bH|0)+_bI|0,E(_bJ),_bK,[0,(1+_bH|0)+_bN|0,E(_bD),_bE,E(_bF),E(_bL)],E(_bM)];}}else{return E(_bB);}}else{return E(_bB);}}}else{_bD=E(_bD);return [0,1+_bH|0,E(_bD),_bE,E(_bF),E(_bx)];}}else{_bG=E(_bG);if(!_bG[0]){var _bV=_bG[1],_bW=_bG[2],_bX=_bG[3],_bY=_bG[4],_bZ=_bG[5];_bY=E(_bY);if(!_bY[0]){var _c0=_bY[1],_c1=_bY[2],_c2=_bY[3],_c3=_bY[4],_c4=_bY[5];_bZ=E(_bZ);if(!_bZ[0]){var _c5=_bZ[1];if(_c0>=(imul(2,_c5)|0)){var _c6=function(_c7){_bD=E(_bD);_c4=E(_c4);return (_c4[0]==0)?[0,1+_bV|0,E(_c1),_c2,[0,1+_c7|0,E(_bD),_bE,E(_bx),E(_c3)],[0,(1+_c5|0)+_c4[1]|0,E(_bW),_bX,E(_c4),E(_bZ)]]:[0,1+_bV|0,E(_c1),_c2,[0,1+_c7|0,E(_bD),_bE,E(_bx),E(_c3)],[0,1+_c5|0,E(_bW),_bX,E(_bx),E(_bZ)]];};_c3=E(_c3);if(!_c3[0]){return new F(function(){return _c6(_c3[1]);});}else{return new F(function(){return _c6(0);});}}else{_bD=E(_bD);return [0,1+_bV|0,E(_bW),_bX,[0,1+_c0|0,E(_bD),_bE,E(_bx),E(_bY)],E(_bZ)];}}else{_bD=E(_bD);return [0,3,E(_c1),_c2,[0,1,E(_bD),_bE,E(_bx),E(_bx)],[0,1,E(_bW),_bX,E(_bx),E(_bx)]];}}else{_bZ=E(_bZ);if(!_bZ[0]){_bD=E(_bD);return [0,3,E(_bW),_bX,[0,1,E(_bD),_bE,E(_bx),E(_bx)],E(_bZ)];}else{_bD=E(_bD);return [0,2,E(_bD),_bE,E(_bx),E(_bG)];}}}else{_bD=E(_bD);return [0,1,E(_bD),_bE,E(_bx),E(_bx)];}}},_c8=function(_c9,_ca){_c9=E(_c9);return [0,1,E(_c9),_ca,E(_bx),E(_bx)];},_cb=function(_cc,_cd,_ce){_ce=E(_ce);if(!_ce[0]){return new F(function(){return _bC(_ce[2],_ce[3],_ce[4],B(_cb(_cc,_cd,_ce[5])));});}else{return new F(function(){return _c8(_cc,_cd);});}},_cf=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_cg=function(_ch){return new F(function(){return err(_cf);});},_ci=new T(function(){return B(_cg(_));}),_cj=function(_ck,_cl,_cm,_cn){_cn=E(_cn);if(!_cn[0]){var _co=_cn[1];_cm=E(_cm);if(!_cm[0]){var _cp=_cm[1],_cq=_cm[2],_cr=_cm[3],_cs=_cm[4],_ct=_cm[5];if(_cp<=(imul(3,_co)|0)){_ck=E(_ck);return [0,(1+_cp|0)+_co|0,E(_ck),_cl,E(_cm),E(_cn)];}else{_cs=E(_cs);if(!_cs[0]){var _cu=_cs[1];_ct=E(_ct);if(!_ct[0]){var _cv=_ct[1],_cw=_ct[2],_cx=_ct[3],_cy=_ct[4],_cz=_ct[5];if(_cv>=(imul(2,_cu)|0)){var _cA=function(_cB){_cz=E(_cz);if(!_cz[0]){_ck=E(_ck);return [0,(1+_cp|0)+_co|0,E(_cw),_cx,[0,(1+_cu|0)+_cB|0,E(_cq),_cr,E(_cs),E(_cy)],[0,(1+_co|0)+_cz[1]|0,E(_ck),_cl,E(_cz),E(_cn)]];}else{_ck=E(_ck);return [0,(1+_cp|0)+_co|0,E(_cw),_cx,[0,(1+_cu|0)+_cB|0,E(_cq),_cr,E(_cs),E(_cy)],[0,1+_co|0,E(_ck),_cl,E(_bx),E(_cn)]];}};_cy=E(_cy);if(!_cy[0]){return new F(function(){return _cA(_cy[1]);});}else{return new F(function(){return _cA(0);});}}else{_ck=E(_ck);return [0,(1+_cp|0)+_co|0,E(_cq),_cr,E(_cs),[0,(1+_co|0)+_cv|0,E(_ck),_cl,E(_ct),E(_cn)]];}}else{return E(_ci);}}else{return E(_ci);}}}else{_ck=E(_ck);return [0,1+_co|0,E(_ck),_cl,E(_bx),E(_cn)];}}else{_cm=E(_cm);if(!_cm[0]){var _cC=_cm[1],_cD=_cm[2],_cE=_cm[3],_cF=_cm[4],_cG=_cm[5];_cF=E(_cF);if(!_cF[0]){var _cH=_cF[1];_cG=E(_cG);if(!_cG[0]){var _cI=_cG[1],_cJ=_cG[2],_cK=_cG[3],_cL=_cG[4],_cM=_cG[5];if(_cI>=(imul(2,_cH)|0)){var _cN=function(_cO){_cM=E(_cM);if(!_cM[0]){_ck=E(_ck);return [0,1+_cC|0,E(_cJ),_cK,[0,(1+_cH|0)+_cO|0,E(_cD),_cE,E(_cF),E(_cL)],[0,1+_cM[1]|0,E(_ck),_cl,E(_cM),E(_bx)]];}else{_ck=E(_ck);return [0,1+_cC|0,E(_cJ),_cK,[0,(1+_cH|0)+_cO|0,E(_cD),_cE,E(_cF),E(_cL)],[0,1,E(_ck),_cl,E(_bx),E(_bx)]];}};_cL=E(_cL);if(!_cL[0]){return new F(function(){return _cN(_cL[1]);});}else{return new F(function(){return _cN(0);});}}else{_ck=E(_ck);return [0,1+_cC|0,E(_cD),_cE,E(_cF),[0,1+_cI|0,E(_ck),_cl,E(_cG),E(_bx)]];}}else{_ck=E(_ck);return [0,3,E(_cD),_cE,E(_cF),[0,1,E(_ck),_cl,E(_bx),E(_bx)]];}}else{_cG=E(_cG);if(!_cG[0]){_ck=E(_ck);return [0,3,E(_cG[2]),_cG[3],[0,1,E(_cD),_cE,E(_bx),E(_bx)],[0,1,E(_ck),_cl,E(_bx),E(_bx)]];}else{_ck=E(_ck);return [0,2,E(_ck),_cl,E(_cm),E(_bx)];}}}else{_ck=E(_ck);return [0,1,E(_ck),_cl,E(_bx),E(_bx)];}}},_cP=function(_cQ,_cR,_cS){_cS=E(_cS);if(!_cS[0]){return new F(function(){return _cj(_cS[2],_cS[3],B(_cP(_cQ,_cR,_cS[4])),_cS[5]);});}else{return new F(function(){return _c8(_cQ,_cR);});}},_cT=function(_cU,_cV,_cW,_cX,_cY,_cZ,_d0){return new F(function(){return _cj(_cX,_cY,B(_cP(_cU,_cV,_cZ)),_d0);});},_d1=function(_d2,_d3,_d4,_d5,_d6,_d7,_d8,_d9){_d4=E(_d4);if(!_d4[0]){var _da=_d4[1],_db=_d4[2],_dc=_d4[3],_dd=_d4[4],_de=_d4[5];if((imul(3,_da)|0)>=_d5){if((imul(3,_d5)|0)>=_da){_d2=E(_d2);return [0,(_da+_d5|0)+1|0,E(_d2),_d3,E(_d4),[0,_d5,E(_d6),_d7,E(_d8),E(_d9)]];}else{return new F(function(){return _bC(_db,_dc,_dd,B(_d1(_d2,_d3,_de,_d5,_d6,_d7,_d8,_d9)));});}}else{return new F(function(){return _cj(_d6,_d7,B(_df(_d2,_d3,_da,_db,_dc,_dd,_de,_d8)),_d9);});}}else{return new F(function(){return _cT(_d2,_d3,_d5,_d6,_d7,_d8,_d9);});}},_df=function(_dg,_dh,_di,_dj,_dk,_dl,_dm,_dn){_dn=E(_dn);if(!_dn[0]){var _do=_dn[1],_dp=_dn[2],_dq=_dn[3],_dr=_dn[4],_ds=_dn[5];if((imul(3,_di)|0)>=_do){if((imul(3,_do)|0)>=_di){_dg=E(_dg);return [0,(_di+_do|0)+1|0,E(_dg),_dh,[0,_di,E(_dj),_dk,E(_dl),E(_dm)],E(_dn)];}else{return new F(function(){return _bC(_dj,_dk,_dl,B(_d1(_dg,_dh,_dm,_do,_dp,_dq,_dr,_ds)));});}}else{return new F(function(){return _cj(_dp,_dq,B(_df(_dg,_dh,_di,_dj,_dk,_dl,_dm,_dr)),_ds);});}}else{return new F(function(){return _cb(_dg,_dh,[0,_di,E(_dj),_dk,E(_dl),E(_dm)]);});}},_dt=function(_du,_dv,_dw,_dx){_dw=E(_dw);if(!_dw[0]){var _dy=_dw[1],_dz=_dw[2],_dA=_dw[3],_dB=_dw[4],_dC=_dw[5];_dx=E(_dx);if(!_dx[0]){var _dD=_dx[1],_dE=_dx[2],_dF=_dx[3],_dG=_dx[4],_dH=_dx[5];if((imul(3,_dy)|0)>=_dD){if((imul(3,_dD)|0)>=_dy){_du=E(_du);return [0,(_dy+_dD|0)+1|0,E(_du),_dv,E(_dw),E(_dx)];}else{return new F(function(){return _bC(_dz,_dA,_dB,B(_d1(_du,_dv,_dC,_dD,_dE,_dF,_dG,_dH)));});}}else{return new F(function(){return _cj(_dE,_dF,B(_df(_du,_dv,_dy,_dz,_dA,_dB,_dC,_dG)),_dH);});}}else{return new F(function(){return _cb(_du,_dv,_dw);});}}else{return new F(function(){return _cP(_du,_dv,_dx);});}},_dI=function(_dJ,_dK,_dL,_dM,_dN){_dJ=E(_dJ);if(_dJ==1){_dN=E(_dN);if(!_dN[0]){return [0,[0,1,[0,_dK,_dL],_dM,E(_bx),E(_bx)],_r,_r];}else{var _dO=_dN[1];_dO=E(_dO);var _dP=_dO[1];_dP=E(_dP);var _dQ=_dP[1],_dR=_dP[2];_dQ=E(_dQ);if(_dK>=_dQ){if(_dK!=_dQ){return [0,[0,1,[0,_dK,_dL],_dM,E(_bx),E(_bx)],_r,_dN];}else{_dR=E(_dR);return (_dL<_dR)?[0,[0,1,[0,_dK,_dL],_dM,E(_bx),E(_bx)],_dN,_r]:[0,[0,1,[0,_dK,_dL],_dM,E(_bx),E(_bx)],_r,_dN];}}else{return [0,[0,1,[0,_dK,_dL],_dM,E(_bx),E(_bx)],_dN,_r];}}}else{var _dS=B(_dI(_dJ>>1,_dK,_dL,_dM,_dN)),_dT=_dS[1],_dU=_dS[2],_dV=_dS[3];_dU=E(_dU);if(!_dU[0]){return [0,_dT,_r,_dV];}else{var _dW=_dU[1],_dX=_dU[2];_dW=E(_dW);var _dY=_dW[1],_dZ=_dW[2];_dX=E(_dX);if(!_dX[0]){var _e0=new T(function(){return B(_cb(_dY,_dZ,_dT));});return [0,_e0,_r,_dV];}else{var _e1=_dX[1],_e2=_dX[2];_e1=E(_e1);var _e3=_e1[1],_e4=_e1[2];_dY=E(_dY);var _e5=_dY[1],_e6=_dY[2];_e3=E(_e3);var _e7=_e3[1],_e8=_e3[2];_e5=E(_e5);_e7=E(_e7);if(_e5>=_e7){if(_e5!=_e7){return [0,_dT,_r,_dU];}else{_e6=E(_e6);_e8=E(_e8);if(_e6<_e8){var _e9=B(_dI(_dJ>>1,_e7,_e8,_e4,_e2)),_ea=_e9[1],_eb=new T(function(){return B(_dt(_dY,_dZ,_dT,_ea));});return [0,_eb,_e9[2],_e9[3]];}else{return [0,_dT,_r,_dU];}}}else{var _ec=B(_ed(_dJ>>1,_e7,_e8,_e4,_e2)),_ee=_ec[1],_ef=new T(function(){return B(_dt(_dY,_dZ,_dT,_ee));});return [0,_ef,_ec[2],_ec[3]];}}}}},_ed=function(_eg,_eh,_ei,_ej,_ek){_eg=E(_eg);if(_eg==1){_ek=E(_ek);if(!_ek[0]){return [0,[0,1,[0,_eh,_ei],_ej,E(_bx),E(_bx)],_r,_r];}else{var _el=_ek[1];_el=E(_el);var _em=_el[1];_em=E(_em);var _en=_em[1],_eo=_em[2];_en=E(_en);if(_eh>=_en){if(_eh!=_en){return [0,[0,1,[0,_eh,_ei],_ej,E(_bx),E(_bx)],_r,_ek];}else{_ei=E(_ei);_eo=E(_eo);return (_ei<_eo)?[0,[0,1,[0,_eh,_ei],_ej,E(_bx),E(_bx)],_ek,_r]:[0,[0,1,[0,_eh,_ei],_ej,E(_bx),E(_bx)],_r,_ek];}}else{return [0,[0,1,[0,_eh,_ei],_ej,E(_bx),E(_bx)],_ek,_r];}}}else{var _ep=B(_ed(_eg>>1,_eh,_ei,_ej,_ek)),_eq=_ep[1],_er=_ep[2],_es=_ep[3];_er=E(_er);if(!_er[0]){return [0,_eq,_r,_es];}else{var _et=_er[1],_eu=_er[2];_et=E(_et);var _ev=_et[1],_ew=_et[2];_eu=E(_eu);if(!_eu[0]){var _ex=new T(function(){return B(_cb(_ev,_ew,_eq));});return [0,_ex,_r,_es];}else{var _ey=_eu[1],_ez=_eu[2];_ey=E(_ey);var _eA=_ey[1],_eB=_ey[2];_ev=E(_ev);var _eC=_ev[1],_eD=_ev[2];_eA=E(_eA);var _eE=_eA[1],_eF=_eA[2];_eC=E(_eC);_eE=E(_eE);if(_eC>=_eE){if(_eC!=_eE){return [0,_eq,_r,_er];}else{_eD=E(_eD);_eF=E(_eF);if(_eD<_eF){var _eG=B(_dI(_eg>>1,_eE,_eF,_eB,_ez)),_eH=_eG[1],_eI=new T(function(){return B(_dt(_ev,_ew,_eq,_eH));});return [0,_eI,_eG[2],_eG[3]];}else{return [0,_eq,_r,_er];}}}else{var _eJ=B(_ed(_eg>>1,_eE,_eF,_eB,_ez)),_eK=_eJ[1],_eL=new T(function(){return B(_dt(_ev,_ew,_eq,_eK));});return [0,_eL,_eJ[2],_eJ[3]];}}}}},_eM=function(_eN,_eO,_eP,_eQ){_eQ=E(_eQ);if(!_eQ[0]){var _eR=_eQ[2],_eS=_eQ[3],_eT=_eQ[4],_eU=_eQ[5];_eR=E(_eR);var _eV=_eR[1],_eW=_eR[2];_eV=E(_eV);if(_eN>=_eV){if(_eN!=_eV){return new F(function(){return _bC(_eR,_eS,_eT,B(_eM(_eN,_eO,_eP,_eU)));});}else{_eW=E(_eW);if(_eO>=_eW){if(_eO!=_eW){return new F(function(){return _bC(_eR,_eS,_eT,B(_eM(_eN,_eO,_eP,_eU)));});}else{return [0,_eQ[1],[0,_eN,_eO],_eP,E(_eT),E(_eU)];}}else{return new F(function(){return _cj(_eR,_eS,B(_eM(_eN,_eO,_eP,_eT)),_eU);});}}}else{return new F(function(){return _cj(_eR,_eS,B(_eM(_eN,_eO,_eP,_eT)),_eU);});}}else{return [0,1,[0,_eN,_eO],_eP,E(_bx),E(_bx)];}},_eX=function(_eY,_eZ,_f0,_f1){_f1=E(_f1);if(!_f1[0]){var _f2=_f1[2],_f3=_f1[3],_f4=_f1[4],_f5=_f1[5];_f2=E(_f2);var _f6=_f2[1],_f7=_f2[2];_f6=E(_f6);if(_eY>=_f6){if(_eY!=_f6){return new F(function(){return _bC(_f2,_f3,_f4,B(_eX(_eY,_eZ,_f0,_f5)));});}else{_eZ=E(_eZ);_f7=E(_f7);if(_eZ>=_f7){if(_eZ!=_f7){return new F(function(){return _bC(_f2,_f3,_f4,B(_eM(_eY,_eZ,_f0,_f5)));});}else{return [0,_f1[1],[0,_eY,_eZ],_f0,E(_f4),E(_f5)];}}else{return new F(function(){return _cj(_f2,_f3,B(_eM(_eY,_eZ,_f0,_f4)),_f5);});}}}else{return new F(function(){return _cj(_f2,_f3,B(_eX(_eY,_eZ,_f0,_f4)),_f5);});}}else{return [0,1,[0,_eY,_eZ],_f0,E(_bx),E(_bx)];}},_f8=function(_f9,_fa,_fb,_fc){_fc=E(_fc);if(!_fc[0]){var _fd=_fc[2],_fe=_fc[3],_ff=_fc[4],_fg=_fc[5];_fd=E(_fd);var _fh=_fd[1],_fi=_fd[2];_f9=E(_f9);_fh=E(_fh);if(_f9>=_fh){if(_f9!=_fh){return new F(function(){return _bC(_fd,_fe,_ff,B(_eX(_f9,_fa,_fb,_fg)));});}else{_fa=E(_fa);_fi=E(_fi);if(_fa>=_fi){if(_fa!=_fi){return new F(function(){return _bC(_fd,_fe,_ff,B(_eM(_f9,_fa,_fb,_fg)));});}else{return [0,_fc[1],[0,_f9,_fa],_fb,E(_ff),E(_fg)];}}else{return new F(function(){return _cj(_fd,_fe,B(_eM(_f9,_fa,_fb,_ff)),_fg);});}}}else{return new F(function(){return _cj(_fd,_fe,B(_eX(_f9,_fa,_fb,_ff)),_fg);});}}else{return [0,1,[0,_f9,_fa],_fb,E(_bx),E(_bx)];}},_fj=function(_fk,_fl){while(1){_fl=E(_fl);if(!_fl[0]){return E(_fk);}else{var _fm=_fl[1];_fm=E(_fm);var _fn=_fm[1];_fn=E(_fn);var _fo=B(_f8(_fn[1],_fn[2],_fm[2],_fk)),_fp=_fl[2];_fk=_fo;_fl=_fp;continue;}}},_fq=function(_fr,_fs,_ft,_fu,_fv){return new F(function(){return _fj(B(_f8(_fs,_ft,_fu,_fr)),_fv);});},_fw=function(_fx,_fy,_fz){_fy=E(_fy);var _fA=_fy[1];_fA=E(_fA);return new F(function(){return _fj(B(_f8(_fA[1],_fA[2],_fy[2],_fx)),_fz);});},_fB=function(_fC,_fD,_fE){_fE=E(_fE);if(!_fE[0]){return E(_fD);}else{var _fF=_fE[1],_fG=_fE[2];_fF=E(_fF);var _fH=_fF[1],_fI=_fF[2];_fG=E(_fG);if(!_fG[0]){return new F(function(){return _cb(_fH,_fI,_fD);});}else{var _fJ=_fG[1],_fK=_fG[2];_fJ=E(_fJ);var _fL=_fJ[1],_fM=_fJ[2];_fH=E(_fH);var _fN=_fH[1],_fO=_fH[2];_fL=E(_fL);var _fP=_fL[1],_fQ=_fL[2];_fN=E(_fN);_fP=E(_fP);var _fR=_fP,_fS=function(_fT){var _fU=B(_ed(_fC,_fR,_fQ,_fM,_fK)),_fV=_fU[1],_fW=_fU[3];_fW=E(_fW);if(!_fW[0]){return new F(function(){return _fB(_fC<<1,B(_dt(_fH,_fI,_fD,_fV)),_fU[2]);});}else{return new F(function(){return _fw(B(_dt(_fH,_fI,_fD,_fV)),_fW[1],_fW[2]);});}};if(_fN>=_fR){if(_fN!=_fR){return new F(function(){return _fq(_fD,_fN,_fO,_fI,_fG);});}else{_fO=E(_fO);_fQ=E(_fQ);if(_fO<_fQ){return new F(function(){return _fS(_);});}else{return new F(function(){return _fq(_fD,_fN,_fO,_fI,_fG);});}}}else{return new F(function(){return _fS(_);});}}}},_fX=function(_fY,_fZ,_g0,_g1,_g2,_g3){_g3=E(_g3);if(!_g3[0]){return new F(function(){return _cb([0,_g0,_g1],_g2,_fZ);});}else{var _g4=_g3[1],_g5=_g3[2];_g4=E(_g4);var _g6=_g4[1],_g7=_g4[2];_g6=E(_g6);var _g8=_g6[1],_g9=_g6[2];_g8=E(_g8);var _ga=_g8,_gb=function(_gc){var _gd=B(_ed(_fY,_ga,_g9,_g7,_g5)),_ge=_gd[1],_gf=_gd[3];_gf=E(_gf);if(!_gf[0]){return new F(function(){return _fB(_fY<<1,B(_dt([0,_g0,_g1],_g2,_fZ,_ge)),_gd[2]);});}else{return new F(function(){return _fw(B(_dt([0,_g0,_g1],_g2,_fZ,_ge)),_gf[1],_gf[2]);});}};if(_g0>=_ga){if(_g0!=_ga){return new F(function(){return _fq(_fZ,_g0,_g1,_g2,_g3);});}else{_g9=E(_g9);if(_g1<_g9){return new F(function(){return _gb(_);});}else{return new F(function(){return _fq(_fZ,_g0,_g1,_g2,_g3);});}}}else{return new F(function(){return _gb(_);});}}},_gg=function(_gh,_gi,_gj,_gk,_gl,_gm){_gm=E(_gm);if(!_gm[0]){return new F(function(){return _cb([0,_gj,_gk],_gl,_gi);});}else{var _gn=_gm[1],_go=_gm[2];_gn=E(_gn);var _gp=_gn[1],_gq=_gn[2];_gp=E(_gp);var _gr=_gp[1],_gs=_gp[2];_gr=E(_gr);var _gt=_gr,_gu=function(_gv){var _gw=B(_ed(_gh,_gt,_gs,_gq,_go)),_gx=_gw[1],_gy=_gw[3];_gy=E(_gy);if(!_gy[0]){return new F(function(){return _fB(_gh<<1,B(_dt([0,_gj,_gk],_gl,_gi,_gx)),_gw[2]);});}else{return new F(function(){return _fw(B(_dt([0,_gj,_gk],_gl,_gi,_gx)),_gy[1],_gy[2]);});}};if(_gj>=_gt){if(_gj!=_gt){return new F(function(){return _fq(_gi,_gj,_gk,_gl,_gm);});}else{_gk=E(_gk);_gs=E(_gs);if(_gk<_gs){return new F(function(){return _gu(_);});}else{return new F(function(){return _fq(_gi,_gj,_gk,_gl,_gm);});}}}else{return new F(function(){return _gu(_);});}}},_gz=function(_gA){_gA=E(_gA);if(!_gA[0]){return [1];}else{var _gB=_gA[1],_gC=_gA[2];_gB=E(_gB);var _gD=_gB[1],_gE=_gB[2];_gC=E(_gC);if(!_gC[0]){_gD=E(_gD);return [0,1,E(_gD),_gE,E(_bx),E(_bx)];}else{var _gF=_gC[1],_gG=_gC[2];_gF=E(_gF);var _gH=_gF[1],_gI=_gF[2];_gD=E(_gD);var _gJ=_gD[1],_gK=_gD[2];_gH=E(_gH);var _gL=_gH[1],_gM=_gH[2];_gJ=E(_gJ);_gL=E(_gL);if(_gJ>=_gL){if(_gJ!=_gL){return new F(function(){return _fq([0,1,E(_gD),_gE,E(_bx),E(_bx)],_gL,_gM,_gI,_gG);});}else{_gK=E(_gK);_gM=E(_gM);if(_gK<_gM){return new F(function(){return _fX(1,[0,1,E(_gD),_gE,E(_bx),E(_bx)],_gL,_gM,_gI,_gG);});}else{return new F(function(){return _fq([0,1,E(_gD),_gE,E(_bx),E(_bx)],_gL,_gM,_gI,_gG);});}}}else{return new F(function(){return _gg(1,[0,1,E(_gD),_gE,E(_bx),E(_bx)],_gL,_gM,_gI,_gG);});}}}},_gN=function(_gO){_gO=E(_gO);if(!_gO[0]){return [0];}else{var _gP=_gO[1],_gQ=_gO[2];_gP=E(_gP);var _gR=new T(function(){return B(_gN(_gQ));});return [1,_gP[1],_gR];}},_gS=function(_gT,_gU){_gT=E(_gT);var _gV=new T(function(){return B(_gN(_gU));});return [1,_gT[1],_gV];},_gW=function(_gX){_gX=E(_gX);if(!_gX[0]){return [0];}else{var _gY=_gX[1],_gZ=_gX[2];_gY=E(_gY);var _h0=new T(function(){return B(_gW(_gZ));});return [1,_gY[2],_h0];}},_h1=function(_h2,_h3){_h2=E(_h2);var _h4=new T(function(){return B(_gW(_h3));});return [1,_h2[2],_h4];},_h5=function(_h6){_h6=E(_h6);if(!_h6[0]){return [0];}else{var _h7=_h6[1],_h8=_h6[2];_h7=E(_h7);var _h9=new T(function(){return B(_h5(_h8));});return [1,_h7[1],_h9];}},_ha=function(_hb,_hc){_hb=E(_hb);var _hd=new T(function(){return B(_h5(_hc));});return [1,_hb[1],_hd];},_he=function(_hf){_hf=E(_hf);if(!_hf[0]){return [0];}else{var _hg=_hf[1],_hh=_hf[2];_hg=E(_hg);var _hi=new T(function(){return B(_he(_hh));});return [1,_hg[2],_hi];}},_hj=function(_hk,_hl){_hk=E(_hk);var _hm=new T(function(){return B(_he(_hl));});return [1,_hk[2],_hm];},_hn=function(_ho,_hp,_hq){while(1){_hq=E(_hq);if(!_hq[0]){var _hr=_hq[2],_hs=_hq[4],_ht=_hq[5];_hr=E(_hr);var _hu=_hr[1],_hv=_hr[2];_hu=E(_hu);if(_ho>=_hu){if(_ho!=_hu){_hq=_ht;continue;}else{_hv=E(_hv);if(_hp>=_hv){if(_hp!=_hv){_hq=_ht;continue;}else{return [1,_hq[3]];}}else{_hq=_hs;continue;}}}else{_hq=_hs;continue;}}else{return [0];}}},_hw=function(_hx,_hy,_hz){_hz=E(_hz);if(!_hz[0]){var _hA=_hz[2],_hB=_hz[4],_hC=_hz[5];_hA=E(_hA);var _hD=_hA[1],_hE=_hA[2];_hx=E(_hx);_hD=E(_hD);if(_hx>=_hD){if(_hx!=_hD){return new F(function(){return _hn(_hx,_hy,_hC);});}else{_hE=E(_hE);if(_hy>=_hE){if(_hy!=_hE){return new F(function(){return _hn(_hx,_hy,_hC);});}else{return [1,_hz[3]];}}else{return new F(function(){return _hn(_hx,_hy,_hB);});}}}else{return new F(function(){return _hn(_hx,_hy,_hB);});}}else{return [0];}},_hF=0,_hG=function(_hH,_hI,_hJ,_hK,_){var _hL=jsMoveTo(_hK,_hH+_hJ,_hI),_hM=jsArc(_hK,_hH,_hI,_hJ,0,6.283185307179586);return _hF;},_hN=function(_hO,_hP,_){var _hQ=jsBeginPath(_hP),_hR=B(A(_hO,[_hP,_])),_hS=jsFill(_hP);return _hF;},_hT=new T(function(){return toJSStr(_r);}),_hU="rgb(",_hV=44,_hW=[1,_hV,_r],_hX=new T(function(){return toJSStr(_hW);}),_hY="rgba(",_hZ=41,_i0=[1,_hZ,_r],_i1=new T(function(){return toJSStr(_i0);}),_i2=[1,_i1,_r],_i3=function(_i4){_i4=E(_i4);if(!_i4[0]){var _i5=_i4[1],_i6=_i4[2],_i7=_i4[3],_i8=new T(function(){return String(_i7);}),_i9=new T(function(){return String(_i6);}),_ia=new T(function(){return String(_i5);});_hT=E(_hT);var _ib=jsCat([1,_hU,[1,_ia,[1,_hX,[1,_i9,[1,_hX,[1,_i8,_i2]]]]]],_hT);return E(_ib);}else{var _ic=_i4[4],_id=_i4[1],_ie=_i4[2],_if=_i4[3],_ig=new T(function(){return String(_ic);}),_ih=new T(function(){return String(_if);}),_ii=new T(function(){return String(_ie);}),_ij=new T(function(){return String(_id);});_hT=E(_hT);var _ik=jsCat([1,_hY,[1,_ij,[1,_hX,[1,_ii,[1,_hX,[1,_ih,[1,_hX,[1,_ig,_i2]]]]]]]],_hT);return E(_ik);}},_il=function(_im,_){return _hF;},_in="strokeStyle",_io="fillStyle",_ip=[0,187,187,187],_iq=new T(function(){return 16*Math.sqrt(3)/2;}),_ir=[0,0,0,0],_is=new T(function(){return B(_i3(_ir));}),_it=new T(function(){return 16*Math.sqrt(3)/2;}),_iu=[0,255,0,0],_iv=new T(function(){return B(_i3(_iu));}),_iw=new T(function(){return 16*Math.sqrt(3)/2;}),_ix=function(_iy,_){return _hF;},_iz=function(_iA){_iA=E(_iA);if(!_iA[0]){return E(_ix);}else{var _iB=_iA[1],_iC=_iA[2];_iB=E(_iB);var _iD=_iB[1],_iE=_iB[2],_iF=function(_iG,_){_iG=E(_iG);var _iH=_iG;_iD=E(_iD);_iE=E(_iE);var _iI=jsMoveTo(_iH,_iD,_iE),_iJ=_iC,_=_;while(1){_iJ=E(_iJ);if(!_iJ[0]){return _hF;}else{var _iK=_iJ[1];_iK=E(_iK);var _iL=_iK[1],_iM=_iK[2];_iL=E(_iL);_iM=E(_iM);var _iN=jsLineTo(_iH,_iL,_iM),_iO=_iJ[2];_iJ=_iO;continue;}}};return E(_iF);}},_iP=function(_iQ,_iR,_iS,_iT,_iU,_iV,_){var _iW=jsResetCanvas(_iR);_in=E(_in);_is=E(_is);var _iX=jsSet(_iQ,_in,_is),_iY=_iT-1|0;if(0<=_iY){var _iZ=new T(function(){_iS=E(_iS);return B(_4i(0,_iS-1|0));}),_j0=function(_j1){var _j2=new T(function(){var _j3=2+24*_j1,_j4=_j3+24,_j5=_j3+8,_j6=new T(function(){if(!(_j1%2)){return true;}else{return false;}}),_j7=_j3+32,_j8=_j3+16,_j9=_j3,_ja=function(_jb){_jb=E(_jb);if(!_jb[0]){return E(_il);}else{var _jc=_jb[1],_jd=_jb[2],_je=new T(function(){_j6=E(_j6);if(!_j6){_jc=E(_jc);return 2+16*Math.sqrt(3)*(_jc+0.5);}else{_jc=E(_jc);return 2+16*Math.sqrt(3)*_jc;}}),_jf=function(_jg,_){_je=E(_je);_iw=E(_iw);_jg=E(_jg);return new F(function(){return _hG(_je+_iw,_j8,5.333333333333333,_jg,_);});},_jh=new T(function(){_iV=E(_iV);if(!_iV[0]){return E(_il);}else{var _ji=_iV[1];_ji=E(_ji);var _jj=_ji[1],_jk=_ji[2];_jc=E(_jc);_jj=E(_jj);if(_jc!=_jj){return E(_il);}else{_jk=E(_jk);if(_j1!=_jk){return E(_il);}else{var _jl=function(_jm,_){_jm=E(_jm);_io=E(_io);_iv=E(_iv);var _jn=jsSet(_jm,_io,_iv);return new F(function(){return _hN(_jf,_jm,_);});};return E(_jl);}}}}),_jo=new T(function(){var _jp=new T(function(){_je=E(_je);_it=E(_it);return _je+_it;}),_jq=new T(function(){_je=E(_je);return _je+16*Math.sqrt(3);}),_jr=new T(function(){_je=E(_je);return _je+16*Math.sqrt(3);}),_js=new T(function(){_je=E(_je);_iq=E(_iq);return _je+_iq;});return B(_iz([1,[0,_je,_j5],[1,[0,_js,_j9],[1,[0,_jr,_j5],[1,[0,_jq,_j4],[1,[0,_jp,_j7],[1,[0,_je,_j4],_r]]]]]]));}),_jt=new T(function(){var _ju=B(_hw(_jc,_j1,_iU));if(!_ju[0]){return B(_i3(_ip));}else{return B(_i3(_ju[1]));}}),_jv=new T(function(){return B(_ja(_jd));}),_jw=function(_jx,_){_jx=E(_jx);_io=E(_io);_jt=E(_jt);var _jy=jsSet(_jx,_io,_jt),_jz=jsBeginPath(_jx),_jA=B(A(_jo,[_jx,_])),_jB=jsFill(_jx),_jC=jsBeginPath(_jx),_jD=B(A(_jo,[_jx,_])),_jE=jsStroke(_jx),_jF=B(A(_jh,[_jx,_]));return new F(function(){return A(_jv,[_jx,_]);});};return E(_jw);}};return B(_ja(_iZ));}),_jG=new T(function(){if(_j1!=_iY){return B(_j0(_j1+1|0));}else{return E(_il);}}),_jH=function(_jI,_){var _jJ=B(A(_j2,[_jI,_]));return new F(function(){return A(_jG,[_jI,_]);});};return E(_jH);};return new F(function(){return A(_j0,[0,_iQ,_]);});}else{return _hF;}},_jK=function(_jL){_jL=E(_jL);var _jM=jsShow(_jL);return new F(function(){return fromJSStr(_jM);});},_jN=function(_jO){var _jP=new T(function(){return B(_jK(_jO));});return function(_jQ){return new F(function(){return _H(_jP,_jQ);});};},_jR=45,_jS=function(_jT,_jU,_jV){var _jW=function(_jX){_jU=E(_jU);var _jY=new T(function(){return B(A(_jT,[ -_jV]));});if(_jU<=6){var _jZ=function(_k0){var _k1=new T(function(){return B(A(_jY,[_k0]));});return [1,_jR,_k1];};return E(_jZ);}else{var _k2=function(_k3){var _k4=new T(function(){return B(A(_jY,[[1,_Q,_k3]]));});return [1,_R,[1,_jR,_k4]];};return E(_k2);}};if(_jV>=0){var _k5=isDoubleNegativeZero(_jV);_k5=E(_k5);if(!_k5){return new F(function(){return A(_jT,[_jV]);});}else{return new F(function(){return _jW(_);});}}else{return new F(function(){return _jW(_);});}},_k6=new T(function(){return B(unCStr("height"));}),_k7=new T(function(){return B(unCStr("px"));}),_k8=new T(function(){return B(unCStr("width"));}),_k9=0,_ka=function(_kb,_kc,_kd,_){_k8=E(_k8);var _ke=jsSetAttr(_kb,toJSStr(_k8),toJSStr(B(_H(B(A(_jS,[_jN,_k9,16*Math.sqrt(3)*(_kc+0.5)+5,_r])),_k7))));_k6=E(_k6);var _kf=jsSetAttr(_kb,toJSStr(_k6),toJSStr(B(_H(B(A(_jS,[_jN,_k9,24*_kd+16+5,_r])),_k7))));return _hF;},_kg=function(_kh,_ki){while(1){_ki=E(_ki);if(!_ki[0]){return E(_kh);}else{var _kj=_ki[1],_kk=_ki[2];_kj=E(_kj);if(_kh>_kj){_kh=_kj;_ki=_kk;continue;}else{_ki=_kk;continue;}}}},_kl=function(_km,_kn){while(1){_kn=E(_kn);if(!_kn[0]){return E(_km);}else{var _ko=_kn[1],_kp=_kn[2];_ko=E(_ko);if(_km>_ko){_km=_ko;_kn=_kp;continue;}else{_kn=_kp;continue;}}}},_kq=function(_kr,_ks){while(1){_ks=E(_ks);if(!_ks[0]){return E(_kr);}else{var _kt=_ks[1],_ku=_ks[2];_kt=E(_kt);if(_kr>_kt){_ks=_ku;continue;}else{_kr=_kt;_ks=_ku;continue;}}}},_kv=function(_kw,_kx){while(1){_kx=E(_kx);if(!_kx[0]){return E(_kw);}else{var _ky=_kx[1],_kz=_kx[2];_ky=E(_ky);if(_kw>_ky){_kx=_kz;continue;}else{_kw=_ky;_kx=_kz;continue;}}}},_kA=function(_kB){_kB=E(_kB);return E(_kB[1]);},_kC=function(_kD){_kD=E(_kD);return E(_kD[2]);},_kE=function(_kF){_kF=E(_kF);return E(_kF[1]);},_kG=function(_){return new F(function(){return nMV(_2P);});},_kH=function(_kI){var _kJ=B(A(_kI,[_]));return E(_kJ);},_kK=new T(function(){return B(_kH(_kG));}),_kL=new T(function(){return __jsNull();}),_kM=new T(function(){return E(_kL);}),_kN=new T(function(){return E(_kM);}),_kO=(function(e,name,f){e.addEventListener(name,f,false);return [f];}),_kP=function(_kQ,_kR,_kS,_kT,_kU,_kV,_kW,_kX){_kQ=E(_kQ);var _kY=_kQ[1],_kZ=_kQ[3],_l0=new T(function(){return B(A(_kA,[_kT,_kV]));}),_l1=new T(function(){return B(A(_kE,[_kU,_kW]));}),_l2=function(_l3){return new F(function(){return A(_kZ,[[0,_l1,_l0,_l3]]);});},_l4=function(_l5){var _l6=new T(function(){var _l7=new T(function(){_l0=E(_l0);var _l8=_l0;_l1=E(_l1);var _l9=_l1;_l5=E(_l5);var _la=function(_lb,_){var _lc=B(A(_l5,[_lb,_]));return _kN;};_la=E(_la);var _ld=__createJSFunc(2,_la),_le=_ld,_lf=function(_){_kO=E(_kO);return new F(function(){return _kO(_l8,_l9,_le);});};return E(_lf);});return B(A(_kR,[_l7]));});return new F(function(){return A(_kY,[_l6,_l2]);});},_lg=new T(function(){var _lh=new T(function(){return B(_kC(_kU));}),_li=function(_lj){var _lk=new T(function(){var _ll=function(_){_kK=E(_kK);var _=wMV(_kK,[1,_lj]);return new F(function(){return A(_lh,[_kW,_lj,_]);});};return B(A(_kR,[_ll]));});return new F(function(){return A(_kY,[_lk,_kX]);});};return B(A(_kS,[_li]));});return new F(function(){return A(_kY,[_lg,_l4]);});},_lm=0,_ln=new T(function(){return B(unCStr("Pattern match failure in do expression at Viewer.hs:55:7-17"));}),_lo=new T(function(){return B(unCStr("Pattern match failure in do expression at Viewer.hs:74:9-19"));}),_lp=function(_lq,_){_lq=E(_lq);if(!_lq[0]){return _r;}else{var _lr=_lq[1];_lr=E(_lr);var _ls=jsFind(_lr),_lt=B(_lp(_lq[2],_));return [1,_ls,_lt];}},_lu=function(_lv,_lw){while(1){_lw=E(_lw);if(!_lw[0]){return false;}else{if(!B(A(_lv,[_lw[1]]))){var _lx=_lw[2];_lw=_lx;continue;}else{return true;}}}},_ly=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_lz=function(_lA){var _lB=new T(function(){return B(_b0(_lA,_ly));});return new F(function(){return _aI([0,_lB],_aF);});},_lC=function(_lD){return new F(function(){return _lz("Viewer.hs:(30,48)-(77,13)|lambda");});},_lE=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_lF=new T(function(){return B(err(_lE));}),_lG=function(_lH){_lH=E(_lH);return (_lH[0]==0)?E(_lF):E(_lH[1]);},_lI=[0,187,187,17],_lJ=function(_lK){_lK=E(_lK);if(!_lK[0]){return [0];}else{var _lL=_lK[1],_lM=_lK[2];_lL=E(_lL);var _lN=new T(function(){return B(_lJ(_lM));});return [1,[0,_lL,_lI],_lN];}},_lO=function(_lP){_lP=E(_lP);if(!_lP[0]){return [0];}else{var _lQ=_lP[1],_lR=_lP[2],_lS=new T(function(){return B(_lO(_lR));}),_lT=new T(function(){return B(_bu(_lQ));});return [1,[0,_lT,_lI],_lS];}},_lU=function(_lV){_lV=E(_lV);return (_lV[0]==0)?true:false;},_lW=new T(function(){return B(_bd("Viewer.hs:41:11-35|Data.Either.Right json"));}),_lX=new T(function(){return B(_bd("Viewer.hs:52:119-149|Haste.JSON.Arr members"));}),_lY="members",_lZ="pivot",_m0=new T(function(){return B(_bd("Viewer.hs:52:69-95|Haste.JSON.Arr units\'"));}),_m1="units",_m2=new T(function(){return B(_bd("Viewer.hs:50:71-98|Haste.JSON.Arr filled"));}),_m3="filled",_m4=new T(function(){return B(_bd("Viewer.hs:47:11-39|Haste.JSON.Num height\'"));}),_m5="height",_m6=new T(function(){return B(_bd("Viewer.hs:46:11-38|Haste.JSON.Num width\'"));}),_m7="width",_m8=new T(function(){return B(unCStr("Pattern match failure in do expression at Viewer.hs:36:7-17"));}),_m9=new T(function(){return B(unCStr("canvas"));}),_ma="value",_mb=function(_mc,_md){_md=E(_md);if(!_md[0]){return [0];}else{var _me=_md[1],_mf=_md[2],_mg=new T(function(){return B(_mb(_mc,_mf));}),_mh=new T(function(){return B(A(_mc,[_me]));});return [1,_mh,_mg];}},_mi=new T(function(){return B(unCStr(": empty list"));}),_mj=new T(function(){return B(unCStr("Prelude."));}),_mk=function(_ml){var _mm=new T(function(){return B(_H(_ml,_mi));},1);return new F(function(){return err(B(_H(_mj,_mm)));});},_mn=new T(function(){return B(unCStr("maximum"));}),_mo=new T(function(){return B(_mk(_mn));}),_mp=new T(function(){return B(unCStr("minimum"));}),_mq=new T(function(){return B(_mk(_mp));}),_mr=new T(function(){return B(unCStr("board"));}),_ms=new T(function(){return B(unCStr("input"));}),_mt=new T(function(){return B(unCStr("load"));}),_mu=new T(function(){return B(unCStr("units"));}),_mv=[1,_mu,_r],_mw=[1,_mt,_mv],_mx=[1,_ms,_mw],_my=[1,_mr,_mx],_mz=function(_mA){_mA=E(_mA);return new F(function(){return toJSStr(_mA);});},_mB=new T(function(){return B(_mb(_mz,_my));}),_mC=5,_mD=function(_mE,_mF){while(1){var _mG=(function(_mH,_mI){_mH=E(_mH);if(!_mH[0]){return [0];}else{var _mJ=_mH[2];_mI=E(_mI);if(!_mI[0]){return [0];}else{var _mK=_mI[1],_mL=_mI[2];_mK=E(_mK);if(!_mK[0]){var _mM=new T(function(){return B(_mD(_mJ,_mL));});return [1,_mH[1],_mM];}else{_mE=_mJ;_mF=_mL;return null;}}}})(_mE,_mF);if(_mG!=null){return _mG;}}},_mN=new T(function(){return B(unAppCStr("[]",_r));}),_mO=[1,_2n,_r],_mP=function(_mQ){_mQ=E(_mQ);if(!_mQ[0]){return E(_mO);}else{var _mR=_mQ[1],_mS=_mQ[2],_mT=new T(function(){_mR=E(_mR);var _mU=new T(function(){return B(_mP(_mS));},1);return B(_H(fromJSStr(_mR),_mU));});return [1,_2m,_mT];}},_mV=function(_mW,_mX){var _mY=new T(function(){var _mZ=B(_mD(_mX,_mW));if(!_mZ[0]){return E(_mN);}else{var _n0=_mZ[1],_n1=_mZ[2],_n2=new T(function(){_n0=E(_n0);var _n3=new T(function(){return B(_mP(_n1));},1);return B(_H(fromJSStr(_n0),_n3));});return [1,_2o,_n2];}});return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",_mY)));});},_n4=function(_){var _n5=B(_lp(_mB,_));if(!B(_lu(_lU,_n5))){var _n6=B(_mb(_lG,_n5));if(!_n6[0]){return new F(function(){return _lC(_);});}else{var _n7=_n6[1],_n8=_n6[2];_n8=E(_n8);if(!_n8[0]){return new F(function(){return _lC(_);});}else{var _n9=_n8[1],_na=_n8[2];_na=E(_na);if(!_na[0]){return new F(function(){return _lC(_);});}else{var _nb=_na[2];_nb=E(_nb);if(!_nb[0]){return new F(function(){return _lC(_);});}else{var _nc=_nb[1],_nd=_nb[2];_nd=E(_nd);if(!_nd[0]){_n7=E(_n7);var _ne=_n7,_nf=B(_ka(_ne,5,10,_)),_ng=jsHasCtx2D(_ne);_ng=E(_ng);if(!_ng){return new F(function(){return _2X(_m8,_);});}else{var _nh=jsGetCtx2D(_ne),_ni=B(_iP(_nh,_ne,_mC,10,_bx,_2P,_)),_nj=function(_nk,_){_n9=E(_n9);_ma=E(_ma);var _nl=jsGet(_n9,_ma),_nm=jsParseJSON(_nl);if(!_nm[0]){return E(_lW);}else{var _nn=_nm[1],_no=B(_n(_nn,_m7));if(!_no[0]){var _np=jsRound(_no[1]),_nq=_np,_nr=B(_n(_nn,_m5));if(!_nr[0]){var _ns=jsRound(_nr[1]),_nt=_ns,_nu=B(_ka(_ne,_nq,_nt,_)),_nv=jsHasCtx2D(_ne),_nw=function(_,_nx){_nx=E(_nx);if(!_nx[0]){return new F(function(){return _2X(_ln,_);});}else{var _ny=_nx[1];_ny=E(_ny);var _nz=new T(function(){var _nA=B(_n(_nn,_m3));if(_nA[0]==3){return B(_gz(B(_lO(_nA[1]))));}else{return E(_m2);}}),_nB=B(_iP(_ny[1],_ny[2],_nq,_nt,_nz,_2P,_));_nc=E(_nc);var _nC=_nc,_nD=jsClearChildren(_nC),_nE=B(_n(_nn,_m1));if(_nE[0]==3){var _nF=_nE[1];_nF=E(_nF);if(!_nF[0]){return _hF;}else{var _nG=_nF[1];_m9=E(_m9);var _nH=jsCreateElem(toJSStr(_m9)),_nI=jsAppendChild(_nH,_nC),_nJ=new T(function(){var _nK=new T(function(){return B(_n(_nG,_lZ));}),_nL=B(_bo(_nK));return [0,_nL[1],_nL[2]];}),_nM=new T(function(){var _nN=B(_n(_nG,_lY));if(_nN[0]==3){return B(_mb(_bu,_nN[1]));}else{return E(_lX);}}),_nO=new T(function(){var _nP=B(_h1(_nJ,_nM));if(!_nP[0]){return E(_mq);}else{var _nQ=_nP[1];_nQ=E(_nQ);return B(_kl(_nQ,_nP[2]));}}),_nR=new T(function(){var _nS=B(_gS(_nJ,_nM));if(!_nS[0]){return E(_mq);}else{var _nT=_nS[1];_nT=E(_nT);return B(_kg(_nT,_nS[2]));}}),_nU=new T(function(){var _nV=function(_nW){_nW=E(_nW);var _nX=_nW[1],_nY=_nW[2],_nZ=new T(function(){return B(_4f(_nY,_nO));}),_o0=new T(function(){return B(_4f(_nX,_nR));});return [0,_o0,_nZ];};return B(_mb(_nV,_nM));}),_o1=new T(function(){_nJ=E(_nJ);var _o2=_nJ[1],_o3=_nJ[2],_o4=new T(function(){return B(_4f(_o3,_nO));}),_o5=new T(function(){return B(_4f(_o2,_nR));});return [0,_o5,_o4];}),_o6=B(_ha(_o1,_nU));if(!_o6[0]){return E(_mo);}else{var _o7=_o6[1];_o7=E(_o7);var _o8=B(_hj(_o1,_nU));if(!_o8[0]){return E(_mo);}else{var _o9=_o8[1];_o9=E(_o9);var _oa=B(_kv(_o9,_o8[2]))+1|0,_ob=B(_kq(_o7,_o6[2]))+1|0,_oc=B(_ka(_nH,_ob,_oa,_)),_od=jsHasCtx2D(_nH);_od=E(_od);if(!_od){return new F(function(){return _2X(_lo,_);});}else{var _oe=jsGetCtx2D(_nH),_of=new T(function(){return B(_gz(B(_lJ(_nU))));}),_og=B(_iP(_oe,_nH,_ob,_oa,_of,[1,_o1],_)),_oh=_nF[2],_=_;while(1){var _oi=(function(_oj,_){_oj=E(_oj);if(!_oj[0]){return _hF;}else{var _ok=_oj[1],_ol=jsCreateElem(toJSStr(_m9)),_om=jsAppendChild(_ol,_nC),_on=new T(function(){var _oo=new T(function(){return B(_n(_ok,_lZ));}),_op=B(_bo(_oo));return [0,_op[1],_op[2]];}),_oq=new T(function(){var _or=B(_n(_ok,_lY));if(_or[0]==3){return B(_mb(_bu,_or[1]));}else{return E(_lX);}}),_os=new T(function(){var _ot=B(_h1(_on,_oq));if(!_ot[0]){return E(_mq);}else{var _ou=_ot[1];_ou=E(_ou);return B(_kl(_ou,_ot[2]));}}),_ov=new T(function(){var _ow=B(_gS(_on,_oq));if(!_ow[0]){return E(_mq);}else{var _ox=_ow[1];_ox=E(_ox);return B(_kg(_ox,_ow[2]));}}),_oy=new T(function(){var _oz=function(_oA){_oA=E(_oA);var _oB=_oA[1],_oC=_oA[2],_oD=new T(function(){return B(_4f(_oC,_os));}),_oE=new T(function(){return B(_4f(_oB,_ov));});return [0,_oE,_oD];};return B(_mb(_oz,_oq));}),_oF=new T(function(){_on=E(_on);var _oG=_on[1],_oH=_on[2],_oI=new T(function(){return B(_4f(_oH,_os));}),_oJ=new T(function(){return B(_4f(_oG,_ov));});return [0,_oJ,_oI];}),_oK=B(_ha(_oF,_oy));if(!_oK[0]){return E(_mo);}else{var _oL=_oK[1];_oL=E(_oL);var _oM=B(_hj(_oF,_oy));if(!_oM[0]){return E(_mo);}else{var _oN=_oM[1];_oN=E(_oN);var _oO=B(_kv(_oN,_oM[2]))+1|0,_oP=B(_kq(_oL,_oK[2]))+1|0,_oQ=B(_ka(_ol,_oP,_oO,_)),_oR=jsHasCtx2D(_ol);_oR=E(_oR);if(!_oR){return new F(function(){return _2X(_lo,_);});}else{var _oS=jsGetCtx2D(_ol),_oT=new T(function(){return B(_gz(B(_lJ(_oy))));}),_oU=B(_iP(_oS,_ol,_oP,_oO,_oT,[1,_oF],_)),_oV=_oj[2];_oh=_oV;return null;}}}}})(_oh,_);if(_oi!=null){return _oi;}}}}}}}else{return E(_m0);}}};_nv=E(_nv);if(!_nv){return new F(function(){return _nw(_,_2P);});}else{var _oW=jsGetCtx2D(_ne);return new F(function(){return _nw(_,[1,[0,_oW,_ne]]);});}}else{return E(_m4);}}else{return E(_m6);}}},_oX=B(A(_kP,[_4e,_3X,_48,_3Z,_3U,_na[1],_lm,_nj,_]));return _hF;}}else{return new F(function(){return _lC(_);});}}}}}}else{return new F(function(){return _mV(_n5,_mB);});}},_oY=function(_){return new F(function(){return _n4(_);});};
var hasteMain = function() {B(A(_oY, [0]));};window.onload = hasteMain;