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

var _0=function(_1){var _2=new T(function(){_1=E(_1);return fromJSStr(_1);});return new F(function(){return err(B(unAppCStr("Haste.JSON.!: unable to look up key ",_2)));});},_3=function(_4,_5){_4=E(_4);_5=E(_5);var _6=strEq(_4,_5);_6=E(_6);return (_6==0)?true:false;},_7=function(_8,_9){_8=E(_8);_9=E(_9);var _a=strEq(_8,_9);_a=E(_a);return (_a==0)?false:true;},_b=function(_c,_d){return new F(function(){return _7(_c,_d);});},_e=[0,_b,_3],_f=function(_g){_g=E(_g);return E(_g[1]);},_h=function(_i,_j,_k){while(1){_k=E(_k);if(!_k[0]){return [0];}else{var _l=_k[1];_l=E(_l);if(!B(A(_f,[_i,_j,_l[1]]))){var _m=_k[2];_k=_m;continue;}else{return [1,_l[2]];}}}},_n=function(_o,_p){_o=E(_o);if(_o[0]==4){var _q=B(_h(_e,_p,_o[1]));if(!_q[0]){return new F(function(){return _0(_p);});}else{return E(_q[1]);}}else{return new F(function(){return _0(_p);});}},_r=[0],_s=function(_t,_){_t=E(_t);if(!_t[0]){return _r;}else{var _u=_t[1],_v=B(_s(_t[2],_)),_w=new T(function(){_u=E(_u);var _x=Number(_u);return jsTrunc(_x);});return [1,_w,_v];}},_y=function(_z,_){var _A=__arr2lst(0,_z);return new F(function(){return _s(_A,_);});},_B=function(_C,_){_C=E(_C);return new F(function(){return _y(_C,_);});},_D=function(_E,_){return new T(function(){_E=E(_E);var _F=Number(_E);return jsTrunc(_F);});},_G=[0,_D,_B],_H=function(_I,_J){_I=E(_I);if(!_I[0]){return E(_J);}else{var _K=_I[2],_L=new T(function(){return B(_H(_K,_J));});return [1,_I[1],_L];}},_M=function(_N,_O){var _P=jsShowI(_N);return new F(function(){return _H(fromJSStr(_P),_O);});},_Q=41,_R=40,_S=function(_T,_U,_V){if(_U>=0){return new F(function(){return _M(_U,_V);});}else{if(_T<=6){return new F(function(){return _M(_U,_V);});}else{var _W=new T(function(){var _X=jsShowI(_U);return B(_H(fromJSStr(_X),[1,_Q,_V]));});return [1,_R,_W];}}},_Y=41,_Z=[1,_Y,_r],_10=new T(function(){return B(_S(0,2,_Z));}),_11=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_10));}),_12=function(_13){var _14=new T(function(){return B(_S(0,_13,_11));});return new F(function(){return err(B(unAppCStr("toEnum{MouseButton}: tag (",_14)));});},_15=function(_16,_){return new T(function(){_16=E(_16);var _17=Number(_16),_18=jsTrunc(_17);if(_18<0){return B(_12(_18));}else{if(_18>2){return B(_12(_18));}else{return _18;}}});},_19=function(_1a,_){_1a=E(_1a);if(!_1a[0]){return _r;}else{var _1b=B(_19(_1a[2],_));return [1,_1a[1],_1b];}},_1c=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_1d=new T(function(){return B(unCStr("base"));}),_1e=new T(function(){return B(unCStr("IOException"));}),_1f=new T(function(){var _1g=hs_wordToWord64(4053623282),_1h=hs_wordToWord64(3693590983);return [0,_1g,_1h,[0,_1g,_1h,_1d,_1c,_1e],_r];}),_1i=function(_1j){return E(_1f);},_1k=function(_1l){_1l=E(_1l);return E(_1l[1]);},_1m=function(_1n,_1o,_1p){var _1q=B(A(_1n,[_])),_1r=B(A(_1o,[_])),_1s=hs_eqWord64(_1q[1],_1r[1]);_1s=E(_1s);if(!_1s){return [0];}else{var _1t=hs_eqWord64(_1q[2],_1r[2]);_1t=E(_1t);return (_1t==0)?[0]:[1,_1p];}},_1u=function(_1v){_1v=E(_1v);return new F(function(){return _1m(B(_1k(_1v[1])),_1i,_1v[2]);});},_1w=new T(function(){return B(unCStr(": "));}),_1x=41,_1y=new T(function(){return B(unCStr(" ("));}),_1z=new T(function(){return B(unCStr("already exists"));}),_1A=new T(function(){return B(unCStr("does not exist"));}),_1B=new T(function(){return B(unCStr("protocol error"));}),_1C=new T(function(){return B(unCStr("failed"));}),_1D=new T(function(){return B(unCStr("invalid argument"));}),_1E=new T(function(){return B(unCStr("inappropriate type"));}),_1F=new T(function(){return B(unCStr("hardware fault"));}),_1G=new T(function(){return B(unCStr("unsupported operation"));}),_1H=new T(function(){return B(unCStr("timeout"));}),_1I=new T(function(){return B(unCStr("resource vanished"));}),_1J=new T(function(){return B(unCStr("interrupted"));}),_1K=new T(function(){return B(unCStr("resource busy"));}),_1L=new T(function(){return B(unCStr("resource exhausted"));}),_1M=new T(function(){return B(unCStr("end of file"));}),_1N=new T(function(){return B(unCStr("illegal operation"));}),_1O=new T(function(){return B(unCStr("permission denied"));}),_1P=new T(function(){return B(unCStr("user error"));}),_1Q=new T(function(){return B(unCStr("unsatisified constraints"));}),_1R=new T(function(){return B(unCStr("system error"));}),_1S=function(_1T,_1U){_1T=E(_1T);switch(_1T){case 0:return new F(function(){return _H(_1z,_1U);});break;case 1:return new F(function(){return _H(_1A,_1U);});break;case 2:return new F(function(){return _H(_1K,_1U);});break;case 3:return new F(function(){return _H(_1L,_1U);});break;case 4:return new F(function(){return _H(_1M,_1U);});break;case 5:return new F(function(){return _H(_1N,_1U);});break;case 6:return new F(function(){return _H(_1O,_1U);});break;case 7:return new F(function(){return _H(_1P,_1U);});break;case 8:return new F(function(){return _H(_1Q,_1U);});break;case 9:return new F(function(){return _H(_1R,_1U);});break;case 10:return new F(function(){return _H(_1B,_1U);});break;case 11:return new F(function(){return _H(_1C,_1U);});break;case 12:return new F(function(){return _H(_1D,_1U);});break;case 13:return new F(function(){return _H(_1E,_1U);});break;case 14:return new F(function(){return _H(_1F,_1U);});break;case 15:return new F(function(){return _H(_1G,_1U);});break;case 16:return new F(function(){return _H(_1H,_1U);});break;case 17:return new F(function(){return _H(_1I,_1U);});break;default:return new F(function(){return _H(_1J,_1U);});}},_1V=125,_1W=new T(function(){return B(unCStr("{handle: "));}),_1X=function(_1Y,_1Z,_20,_21,_22,_23){var _24=new T(function(){var _25=new T(function(){var _26=new T(function(){_21=E(_21);if(!_21[0]){return E(_23);}else{var _27=new T(function(){return B(_H(_21,[1,_1x,_23]));},1);return B(_H(_1y,_27));}},1);return B(_1S(_1Z,_26));},1);_20=E(_20);if(!_20[0]){return E(_25);}else{var _28=new T(function(){return B(_H(_1w,_25));},1);return B(_H(_20,_28));}},1);_22=E(_22);if(!_22[0]){_1Y=E(_1Y);if(!_1Y[0]){return E(_24);}else{var _29=_1Y[1];_29=E(_29);if(!_29[0]){var _2a=_29[1],_2b=new T(function(){var _2c=new T(function(){return B(_H(_1w,_24));});return B(_H(_2a,[1,_1V,_2c]));},1);return new F(function(){return _H(_1W,_2b);});}else{var _2d=_29[1],_2e=new T(function(){var _2f=new T(function(){return B(_H(_1w,_24));});return B(_H(_2d,[1,_1V,_2f]));},1);return new F(function(){return _H(_1W,_2e);});}}}else{var _2g=new T(function(){return B(_H(_1w,_24));},1);return new F(function(){return _H(_22[1],_2g);});}},_2h=function(_2i){_2i=E(_2i);return new F(function(){return _1X(_2i[1],_2i[2],_2i[3],_2i[4],_2i[6],_r);});},_2j=function(_2k,_2l){_2k=E(_2k);return new F(function(){return _1X(_2k[1],_2k[2],_2k[3],_2k[4],_2k[6],_2l);});},_2m=44,_2n=93,_2o=91,_2p=function(_2q,_2r,_2s){_2r=E(_2r);if(!_2r[0]){return new F(function(){return unAppCStr("[]",_2s);});}else{var _2t=_2r[1],_2u=_2r[2],_2v=new T(function(){var _2w=new T(function(){var _2x=[1,_2n,_2s],_2y=function(_2z){_2z=E(_2z);if(!_2z[0]){return E(_2x);}else{var _2A=_2z[1],_2B=_2z[2],_2C=new T(function(){var _2D=new T(function(){return B(_2y(_2B));});return B(A(_2q,[_2A,_2D]));});return [1,_2m,_2C];}};return B(_2y(_2u));});return B(A(_2q,[_2t,_2w]));});return [1,_2o,_2v];}},_2E=function(_2F,_2G){return new F(function(){return _2p(_2j,_2F,_2G);});},_2H=function(_2I,_2J,_2K){_2J=E(_2J);return new F(function(){return _1X(_2J[1],_2J[2],_2J[3],_2J[4],_2J[6],_2K);});},_2L=[0,_2H,_2h,_2E],_2M=new T(function(){return [0,_1i,_2L,_2N,_1u];}),_2N=function(_2O){return [0,_2M,_2O];},_2P=[0],_2Q=7,_2R=function(_2S){return [0,_2P,_2Q,_r,_2S,_2P,_2P];},_2T=function(_2U,_){var _2V=new T(function(){var _2W=new T(function(){return B(_2R(_2U));});return B(_2N(_2W));});return new F(function(){return die(_2V);});},_2X=function(_2Y,_){return new F(function(){return _2T(_2Y,_);});},_2Z=new T(function(){return B(unCStr("Pattern match failure in do expression at /var/folders/6b/kfb6m36d3nx19tdfydttmskw0000gn/T/ghc20502_0/ghc20502_28.hscpp:288:5-9"));}),_30=function(_){return new F(function(){return _2X(_2Z,_);});},_31=function(_32){_32=E(_32);return E(_32[1]);},_33=function(_34,_35,_36,_){var _37=__arr2lst(0,_36),_38=B(_19(_37,_));_38=E(_38);if(!_38[0]){return new F(function(){return _30(_);});}else{var _39=_38[2];_39=E(_39);if(!_39[0]){return new F(function(){return _30(_);});}else{var _3a=_39[2];_3a=E(_3a);if(!_3a[0]){var _3b=B(A(_31,[_34,_38[1],_])),_3c=B(A(_31,[_35,_39[1],_]));return [0,_3b,_3c];}else{return new F(function(){return _30(_);});}}}},_3d=new T(function(){return jsGetMouseCoords;}),_3e=0,_3f=[0,_3e,_3e,_3e],_3g="deltaX",_3h="deltaY",_3i="deltaZ",_3j="button",_3k=function(_3l,_3m,_){_3l=E(_3l);if(_3l==7){_3d=E(_3d);var _3n=_3d(_3m),_3o=B(_33(_G,_G,_3n,_)),_3p=_3o;_3g=E(_3g);var _3q=_3m[_3g],_3r=_3q;_3h=E(_3h);var _3s=_3m[_3h],_3t=_3s;_3i=E(_3i);var _3u=_3m[_3i],_3v=_3u;return new T(function(){_3p=E(_3p);var _3w=new T(function(){return Number(_3v);}),_3x=new T(function(){return Number(_3t);}),_3y=new T(function(){return Number(_3r);});return [0,E(_3p),E(_2P),[0,_3y,_3x,_3w]];});}else{_3d=E(_3d);var _3z=_3d(_3m),_3A=B(_33(_G,_G,_3z,_)),_3B=_3A;_3j=E(_3j);var _3C=_3m[_3j],_3D=__jsNull(),_3E=__eq(_3C,_3D);_3E=E(_3E);if(!_3E){var _3F=B(_15(_3C,_)),_3G=_3F;return new T(function(){_3B=E(_3B);return [0,E(_3B),[1,_3G],E(_3f)];});}else{return new T(function(){_3B=E(_3B);return [0,E(_3B),E(_2P),E(_3f)];});}}},_3H=function(_3I,_3J,_){_3J=E(_3J);return new F(function(){return _3k(_3I,_3J,_);});},_3K="wheel",_3L="mouseout",_3M="mouseover",_3N="mousemove",_3O="mouseup",_3P="mousedown",_3Q="dblclick",_3R="click",_3S=function(_3T){_3T=E(_3T);switch(_3T){case 0:return E(_3R);case 1:return E(_3Q);case 2:return E(_3P);case 3:return E(_3O);case 4:return E(_3N);case 5:return E(_3M);case 6:return E(_3L);default:return E(_3K);}},_3U=[0,_3S,_3H],_3V=function(_3W,_){return [1,_3W];},_3X=function(_3Y){return E(_3Y);},_3Z=[0,_3X,_3V],_40=function(_41,_){return new F(function(){return _2X(_41,_);});},_42=function(_43,_){return new F(function(){return _40(_43,_);});},_44=function(_45,_46,_){var _47=B(A(_45,[_]));return new F(function(){return A(_46,[_47,_]);});},_48=function(_49,_){return _49;},_4a=function(_4b,_4c,_){var _4d=B(A(_4b,[_]));return new F(function(){return A(_4c,[_]);});},_4e=[0,_44,_4a,_48,_42],_4f=function(_4g){_4g=E(_4g);var _4h=jsShow(_4g);return new F(function(){return fromJSStr(_4h);});},_4i=function(_4j){var _4k=new T(function(){return B(_4f(_4j));});return function(_4l){return new F(function(){return _H(_4k,_4l);});};},_4m=[1],_4n=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_4o=function(_4p){return new F(function(){return err(_4n);});},_4q=new T(function(){return B(_4o(_));}),_4r=function(_4s,_4t,_4u){_4t=E(_4t);if(!_4t[0]){var _4v=_4t[1];_4u=E(_4u);if(!_4u[0]){var _4w=_4u[1],_4x=_4u[2],_4y=_4u[3],_4z=_4u[4];if(_4w<=(imul(3,_4v)|0)){_4s=E(_4s);return [0,(1+_4v|0)+_4w|0,E(_4s),E(_4t),E(_4u)];}else{_4y=E(_4y);if(!_4y[0]){var _4A=_4y[1],_4B=_4y[2],_4C=_4y[3],_4D=_4y[4];_4z=E(_4z);if(!_4z[0]){var _4E=_4z[1];if(_4A>=(imul(2,_4E)|0)){var _4F=function(_4G){_4s=E(_4s);_4D=E(_4D);return (_4D[0]==0)?[0,(1+_4v|0)+_4w|0,E(_4B),[0,(1+_4v|0)+_4G|0,E(_4s),E(_4t),E(_4C)],[0,(1+_4E|0)+_4D[1]|0,E(_4x),E(_4D),E(_4z)]]:[0,(1+_4v|0)+_4w|0,E(_4B),[0,(1+_4v|0)+_4G|0,E(_4s),E(_4t),E(_4C)],[0,1+_4E|0,E(_4x),E(_4m),E(_4z)]];};_4C=E(_4C);if(!_4C[0]){return new F(function(){return _4F(_4C[1]);});}else{return new F(function(){return _4F(0);});}}else{_4s=E(_4s);return [0,(1+_4v|0)+_4w|0,E(_4x),[0,(1+_4v|0)+_4A|0,E(_4s),E(_4t),E(_4y)],E(_4z)];}}else{return E(_4q);}}else{return E(_4q);}}}else{_4s=E(_4s);return [0,1+_4v|0,E(_4s),E(_4t),E(_4m)];}}else{_4u=E(_4u);if(!_4u[0]){var _4H=_4u[1],_4I=_4u[2],_4J=_4u[3],_4K=_4u[4];_4J=E(_4J);if(!_4J[0]){var _4L=_4J[1],_4M=_4J[2],_4N=_4J[3],_4O=_4J[4];_4K=E(_4K);if(!_4K[0]){var _4P=_4K[1];if(_4L>=(imul(2,_4P)|0)){var _4Q=function(_4R){_4s=E(_4s);_4O=E(_4O);return (_4O[0]==0)?[0,1+_4H|0,E(_4M),[0,1+_4R|0,E(_4s),E(_4m),E(_4N)],[0,(1+_4P|0)+_4O[1]|0,E(_4I),E(_4O),E(_4K)]]:[0,1+_4H|0,E(_4M),[0,1+_4R|0,E(_4s),E(_4m),E(_4N)],[0,1+_4P|0,E(_4I),E(_4m),E(_4K)]];};_4N=E(_4N);if(!_4N[0]){return new F(function(){return _4Q(_4N[1]);});}else{return new F(function(){return _4Q(0);});}}else{_4s=E(_4s);return [0,1+_4H|0,E(_4I),[0,1+_4L|0,E(_4s),E(_4m),E(_4J)],E(_4K)];}}else{_4s=E(_4s);return [0,3,E(_4M),[0,1,E(_4s),E(_4m),E(_4m)],[0,1,E(_4I),E(_4m),E(_4m)]];}}else{_4K=E(_4K);if(!_4K[0]){_4s=E(_4s);return [0,3,E(_4I),[0,1,E(_4s),E(_4m),E(_4m)],E(_4K)];}else{_4s=E(_4s);return [0,2,E(_4s),E(_4m),E(_4u)];}}}else{_4s=E(_4s);return [0,1,E(_4s),E(_4m),E(_4m)];}}},_4S=function(_4T){_4T=E(_4T);return [0,1,E(_4T),E(_4m),E(_4m)];},_4U=function(_4V,_4W){_4W=E(_4W);if(!_4W[0]){return new F(function(){return _4r(_4W[2],_4W[3],B(_4U(_4V,_4W[4])));});}else{return new F(function(){return _4S(_4V);});}},_4X=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_4Y=function(_4Z){return new F(function(){return err(_4X);});},_50=new T(function(){return B(_4Y(_));}),_51=function(_52,_53,_54){_54=E(_54);if(!_54[0]){var _55=_54[1];_53=E(_53);if(!_53[0]){var _56=_53[1],_57=_53[2],_58=_53[3],_59=_53[4];if(_56<=(imul(3,_55)|0)){_52=E(_52);return [0,(1+_56|0)+_55|0,E(_52),E(_53),E(_54)];}else{_58=E(_58);if(!_58[0]){var _5a=_58[1];_59=E(_59);if(!_59[0]){var _5b=_59[1],_5c=_59[2],_5d=_59[3],_5e=_59[4];if(_5b>=(imul(2,_5a)|0)){var _5f=function(_5g){_5e=E(_5e);if(!_5e[0]){_52=E(_52);return [0,(1+_56|0)+_55|0,E(_5c),[0,(1+_5a|0)+_5g|0,E(_57),E(_58),E(_5d)],[0,(1+_55|0)+_5e[1]|0,E(_52),E(_5e),E(_54)]];}else{_52=E(_52);return [0,(1+_56|0)+_55|0,E(_5c),[0,(1+_5a|0)+_5g|0,E(_57),E(_58),E(_5d)],[0,1+_55|0,E(_52),E(_4m),E(_54)]];}};_5d=E(_5d);if(!_5d[0]){return new F(function(){return _5f(_5d[1]);});}else{return new F(function(){return _5f(0);});}}else{_52=E(_52);return [0,(1+_56|0)+_55|0,E(_57),E(_58),[0,(1+_55|0)+_5b|0,E(_52),E(_59),E(_54)]];}}else{return E(_50);}}else{return E(_50);}}}else{_52=E(_52);return [0,1+_55|0,E(_52),E(_4m),E(_54)];}}else{_53=E(_53);if(!_53[0]){var _5h=_53[1],_5i=_53[2],_5j=_53[3],_5k=_53[4];_5j=E(_5j);if(!_5j[0]){var _5l=_5j[1];_5k=E(_5k);if(!_5k[0]){var _5m=_5k[1],_5n=_5k[2],_5o=_5k[3],_5p=_5k[4];if(_5m>=(imul(2,_5l)|0)){var _5q=function(_5r){_5p=E(_5p);if(!_5p[0]){_52=E(_52);return [0,1+_5h|0,E(_5n),[0,(1+_5l|0)+_5r|0,E(_5i),E(_5j),E(_5o)],[0,1+_5p[1]|0,E(_52),E(_5p),E(_4m)]];}else{_52=E(_52);return [0,1+_5h|0,E(_5n),[0,(1+_5l|0)+_5r|0,E(_5i),E(_5j),E(_5o)],[0,1,E(_52),E(_4m),E(_4m)]];}};_5o=E(_5o);if(!_5o[0]){return new F(function(){return _5q(_5o[1]);});}else{return new F(function(){return _5q(0);});}}else{_52=E(_52);return [0,1+_5h|0,E(_5i),E(_5j),[0,1+_5m|0,E(_52),E(_5k),E(_4m)]];}}else{_52=E(_52);return [0,3,E(_5i),E(_5j),[0,1,E(_52),E(_4m),E(_4m)]];}}else{_5k=E(_5k);if(!_5k[0]){_52=E(_52);return [0,3,E(_5k[2]),[0,1,E(_5i),E(_4m),E(_4m)],[0,1,E(_52),E(_4m),E(_4m)]];}else{_52=E(_52);return [0,2,E(_52),E(_53),E(_4m)];}}}else{_52=E(_52);return [0,1,E(_52),E(_4m),E(_4m)];}}},_5s=function(_5t,_5u){_5u=E(_5u);if(!_5u[0]){return new F(function(){return _51(_5u[2],B(_5s(_5t,_5u[3])),_5u[4]);});}else{return new F(function(){return _4S(_5t);});}},_5v=function(_5w,_5x,_5y,_5z,_5A){return new F(function(){return _4r(_5y,_5z,B(_4U(_5w,_5A)));});},_5B=function(_5C,_5D,_5E,_5F,_5G){return new F(function(){return _51(_5E,B(_5s(_5C,_5F)),_5G);});},_5H=function(_5I,_5J,_5K,_5L,_5M,_5N){_5J=E(_5J);if(!_5J[0]){var _5O=_5J[1],_5P=_5J[2],_5Q=_5J[3],_5R=_5J[4];if((imul(3,_5O)|0)>=_5K){if((imul(3,_5K)|0)>=_5O){_5I=E(_5I);return [0,(_5O+_5K|0)+1|0,E(_5I),E(_5J),[0,_5K,E(_5L),E(_5M),E(_5N)]];}else{return new F(function(){return _4r(_5P,_5Q,B(_5H(_5I,_5R,_5K,_5L,_5M,_5N)));});}}else{return new F(function(){return _51(_5L,B(_5S(_5I,_5O,_5P,_5Q,_5R,_5M)),_5N);});}}else{return new F(function(){return _5B(_5I,_5K,_5L,_5M,_5N);});}},_5S=function(_5T,_5U,_5V,_5W,_5X,_5Y){_5Y=E(_5Y);if(!_5Y[0]){var _5Z=_5Y[1],_60=_5Y[2],_61=_5Y[3],_62=_5Y[4];if((imul(3,_5U)|0)>=_5Z){if((imul(3,_5Z)|0)>=_5U){_5T=E(_5T);return [0,(_5U+_5Z|0)+1|0,E(_5T),[0,_5U,E(_5V),E(_5W),E(_5X)],E(_5Y)];}else{return new F(function(){return _4r(_5V,_5W,B(_5H(_5T,_5X,_5Z,_60,_61,_62)));});}}else{return new F(function(){return _51(_60,B(_5S(_5T,_5U,_5V,_5W,_5X,_61)),_62);});}}else{return new F(function(){return _5v(_5T,_5U,_5V,_5W,_5X);});}},_63=function(_64,_65,_66){_65=E(_65);if(!_65[0]){var _67=_65[1],_68=_65[2],_69=_65[3],_6a=_65[4];_66=E(_66);if(!_66[0]){var _6b=_66[1],_6c=_66[2],_6d=_66[3],_6e=_66[4];if((imul(3,_67)|0)>=_6b){if((imul(3,_6b)|0)>=_67){_64=E(_64);return [0,(_67+_6b|0)+1|0,E(_64),E(_65),E(_66)];}else{return new F(function(){return _4r(_68,_69,B(_5H(_64,_6a,_6b,_6c,_6d,_6e)));});}}else{return new F(function(){return _51(_6c,B(_5S(_64,_67,_68,_69,_6a,_6d)),_6e);});}}else{return new F(function(){return _5v(_64,_67,_68,_69,_6a);});}}else{return new F(function(){return _5s(_64,_66);});}},_6f=function(_6g,_6h,_6i,_6j){_6g=E(_6g);if(_6g==1){_6j=E(_6j);if(!_6j[0]){return [0,[0,1,[0,_6h,_6i],E(_4m),E(_4m)],_r,_r];}else{var _6k=_6j[1];_6k=E(_6k);var _6l=_6k[1],_6m=_6k[2];_6l=E(_6l);if(_6h>=_6l){if(_6h!=_6l){return [0,[0,1,[0,_6h,_6i],E(_4m),E(_4m)],_r,_6j];}else{_6m=E(_6m);return (_6i<_6m)?[0,[0,1,[0,_6h,_6i],E(_4m),E(_4m)],_6j,_r]:[0,[0,1,[0,_6h,_6i],E(_4m),E(_4m)],_r,_6j];}}else{return [0,[0,1,[0,_6h,_6i],E(_4m),E(_4m)],_6j,_r];}}}else{var _6n=B(_6f(_6g>>1,_6h,_6i,_6j)),_6o=_6n[1],_6p=_6n[2],_6q=_6n[3];_6p=E(_6p);if(!_6p[0]){return [0,_6o,_r,_6q];}else{var _6r=_6p[1],_6s=_6p[2];_6s=E(_6s);if(!_6s[0]){var _6t=new T(function(){return B(_4U(_6r,_6o));});return [0,_6t,_r,_6q];}else{var _6u=_6s[1],_6v=_6s[2];_6r=E(_6r);var _6w=_6r[1],_6x=_6r[2];_6u=E(_6u);var _6y=_6u[1],_6z=_6u[2];_6w=E(_6w);_6y=E(_6y);if(_6w>=_6y){if(_6w!=_6y){return [0,_6o,_r,_6p];}else{_6x=E(_6x);_6z=E(_6z);if(_6x<_6z){var _6A=B(_6f(_6g>>1,_6y,_6z,_6v)),_6B=_6A[1],_6C=new T(function(){return B(_63(_6r,_6o,_6B));});return [0,_6C,_6A[2],_6A[3]];}else{return [0,_6o,_r,_6p];}}}else{var _6D=B(_6E(_6g>>1,_6y,_6z,_6v)),_6F=_6D[1],_6G=new T(function(){return B(_63(_6r,_6o,_6F));});return [0,_6G,_6D[2],_6D[3]];}}}}},_6E=function(_6H,_6I,_6J,_6K){_6H=E(_6H);if(_6H==1){_6K=E(_6K);if(!_6K[0]){return [0,[0,1,[0,_6I,_6J],E(_4m),E(_4m)],_r,_r];}else{var _6L=_6K[1];_6L=E(_6L);var _6M=_6L[1],_6N=_6L[2];_6M=E(_6M);if(_6I>=_6M){if(_6I!=_6M){return [0,[0,1,[0,_6I,_6J],E(_4m),E(_4m)],_r,_6K];}else{_6J=E(_6J);_6N=E(_6N);return (_6J<_6N)?[0,[0,1,[0,_6I,_6J],E(_4m),E(_4m)],_6K,_r]:[0,[0,1,[0,_6I,_6J],E(_4m),E(_4m)],_r,_6K];}}else{return [0,[0,1,[0,_6I,_6J],E(_4m),E(_4m)],_6K,_r];}}}else{var _6O=B(_6E(_6H>>1,_6I,_6J,_6K)),_6P=_6O[1],_6Q=_6O[2],_6R=_6O[3];_6Q=E(_6Q);if(!_6Q[0]){return [0,_6P,_r,_6R];}else{var _6S=_6Q[1],_6T=_6Q[2];_6T=E(_6T);if(!_6T[0]){var _6U=new T(function(){return B(_4U(_6S,_6P));});return [0,_6U,_r,_6R];}else{var _6V=_6T[1],_6W=_6T[2];_6S=E(_6S);var _6X=_6S[1],_6Y=_6S[2];_6V=E(_6V);var _6Z=_6V[1],_70=_6V[2];_6X=E(_6X);_6Z=E(_6Z);if(_6X>=_6Z){if(_6X!=_6Z){return [0,_6P,_r,_6Q];}else{_6Y=E(_6Y);_70=E(_70);if(_6Y<_70){var _71=B(_6f(_6H>>1,_6Z,_70,_6W)),_72=_71[1],_73=new T(function(){return B(_63(_6S,_6P,_72));});return [0,_73,_71[2],_71[3]];}else{return [0,_6P,_r,_6Q];}}}else{var _74=B(_6E(_6H>>1,_6Z,_70,_6W)),_75=_74[1],_76=new T(function(){return B(_63(_6S,_6P,_75));});return [0,_76,_74[2],_74[3]];}}}}},_77=function(_78){_78=E(_78);return E(_78[3]);},_79=function(_7a,_7b,_7c,_7d,_7e,_7f){switch(B(A(_7a,[_7c,_7e]))){case 0:return true;case 1:return new F(function(){return A(_77,[_7b,_7d,_7f]);});break;default:return false;}},_7g=function(_7h,_7i,_7j,_7k,_7l){_7i=E(_7i);_7k=E(_7k);_7l=E(_7l);return new F(function(){return _79(_7i[2],_7j,_7k[1],_7k[2],_7l[1],_7l[2]);});},_7m=function(_7n){_7n=E(_7n);return E(_7n[6]);},_7o=function(_7p,_7q,_7r,_7s,_7t,_7u){switch(B(A(_7p,[_7r,_7t]))){case 0:return true;case 1:return new F(function(){return A(_7m,[_7q,_7s,_7u]);});break;default:return false;}},_7v=function(_7w,_7x,_7y,_7z,_7A){_7x=E(_7x);_7z=E(_7z);_7A=E(_7A);return new F(function(){return _7o(_7x[2],_7y,_7z[1],_7z[2],_7A[1],_7A[2]);});},_7B=function(_7C){_7C=E(_7C);return E(_7C[5]);},_7D=function(_7E,_7F,_7G,_7H,_7I,_7J){switch(B(A(_7E,[_7G,_7I]))){case 0:return false;case 1:return new F(function(){return A(_7B,[_7F,_7H,_7J]);});break;default:return true;}},_7K=function(_7L,_7M,_7N,_7O,_7P){_7M=E(_7M);_7O=E(_7O);_7P=E(_7P);return new F(function(){return _7D(_7M[2],_7N,_7O[1],_7O[2],_7P[1],_7P[2]);});},_7Q=function(_7R){_7R=E(_7R);return E(_7R[4]);},_7S=function(_7T,_7U,_7V,_7W,_7X,_7Y){switch(B(A(_7T,[_7V,_7X]))){case 0:return false;case 1:return new F(function(){return A(_7Q,[_7U,_7W,_7Y]);});break;default:return true;}},_7Z=function(_80,_81,_82,_83,_84){_81=E(_81);_83=E(_83);_84=E(_84);return new F(function(){return _7S(_81[2],_82,_83[1],_83[2],_84[1],_84[2]);});},_85=function(_86){_86=E(_86);return E(_86[2]);},_87=function(_88,_89,_8a,_8b,_8c,_8d){switch(B(A(_88,[_8a,_8c]))){case 0:return 0;case 1:return new F(function(){return A(_85,[_89,_8b,_8d]);});break;default:return 2;}},_8e=function(_8f,_8g,_8h,_8i,_8j){_8g=E(_8g);_8i=E(_8i);_8j=E(_8j);return new F(function(){return _87(_8g[2],_8h,_8i[1],_8i[2],_8j[1],_8j[2]);});},_8k=function(_8l,_8m,_8n,_8o,_8p){_8m=E(_8m);_8o=E(_8o);var _8q=_8o[1],_8r=_8o[2];_8p=E(_8p);var _8s=_8p[1],_8t=_8p[2];switch(B(A(_8m[2],[_8q,_8s]))){case 0:return [0,_8s,_8t];case 1:return (!B(A(_7m,[_8n,_8r,_8t])))?[0,_8q,_8r]:[0,_8s,_8t];default:return [0,_8q,_8r];}},_8u=function(_8v,_8w,_8x,_8y,_8z){_8w=E(_8w);_8y=E(_8y);var _8A=_8y[1],_8B=_8y[2];_8z=E(_8z);var _8C=_8z[1],_8D=_8z[2];switch(B(A(_8w[2],[_8A,_8C]))){case 0:return [0,_8A,_8B];case 1:return (!B(A(_7m,[_8x,_8B,_8D])))?[0,_8C,_8D]:[0,_8A,_8B];default:return [0,_8C,_8D];}},_8E=function(_8F,_8G,_8H){var _8I=function(_8J,_8K){return new F(function(){return _8u(_8F,_8G,_8H,_8J,_8K);});},_8L=function(_8J,_8K){return new F(function(){return _8k(_8F,_8G,_8H,_8J,_8K);});},_8M=function(_8J,_8K){return new F(function(){return _7v(_8F,_8G,_8H,_8J,_8K);});},_8N=function(_8J,_8K){return new F(function(){return _7K(_8F,_8G,_8H,_8J,_8K);});},_8O=function(_8J,_8K){return new F(function(){return _7Z(_8F,_8G,_8H,_8J,_8K);});},_8P=function(_8J,_8K){return new F(function(){return _7g(_8F,_8G,_8H,_8J,_8K);});},_8Q=function(_8J,_8K){return new F(function(){return _8e(_8F,_8G,_8H,_8J,_8K);});};return [0,_8F,_8Q,_8P,_8O,_8N,_8M,_8L,_8I];},_8R=function(_8S,_8T){_8S=E(_8S);_8T=E(_8T);return _8S==_8T;},_8U=function(_8V,_8W){_8V=E(_8V);_8W=E(_8W);return _8V!=_8W;},_8X=[0,_8R,_8U],_8Y=function(_8Z,_90){_8Z=E(_8Z);_90=E(_90);return (_8Z>_90)?E(_8Z):E(_90);},_91=function(_92,_93){_92=E(_92);_93=E(_93);return (_92>_93)?E(_93):E(_92);},_94=function(_95,_96){return (_95>=_96)?(_95!=_96)?2:1:0;},_97=function(_98,_99){_98=E(_98);_99=E(_99);return new F(function(){return _94(_98,_99);});},_9a=function(_9b,_9c){_9b=E(_9b);_9c=E(_9c);return _9b>=_9c;},_9d=function(_9e,_9f){_9e=E(_9e);_9f=E(_9f);return _9e>_9f;},_9g=function(_9h,_9i){_9h=E(_9h);_9i=E(_9i);return _9h<=_9i;},_9j=function(_9k,_9l){_9k=E(_9k);_9l=E(_9l);return _9k<_9l;},_9m=[0,_8X,_97,_9j,_9a,_9d,_9g,_8Y,_91],_9n=function(_9o,_9p,_9q,_9r,_9s,_9t){return (!B(A(_9o,[_9q,_9s])))?true:(!B(A(_f,[_9p,_9r,_9t])))?true:false;},_9u=function(_9v,_9w,_9x,_9y){_9v=E(_9v);_9x=E(_9x);_9y=E(_9y);return new F(function(){return _9n(_9v[1],_9w,_9x[1],_9x[2],_9y[1],_9y[2]);});},_9z=function(_9A,_9B,_9C,_9D,_9E,_9F){if(!B(A(_9A,[_9C,_9E]))){return false;}else{return new F(function(){return A(_f,[_9B,_9D,_9F]);});}},_9G=function(_9H,_9I,_9J,_9K){_9H=E(_9H);_9J=E(_9J);_9K=E(_9K);return new F(function(){return _9z(_9H[1],_9I,_9J[1],_9J[2],_9K[1],_9K[2]);});},_9L=function(_9M,_9N){var _9O=function(_8J,_8K){return new F(function(){return _9u(_9M,_9N,_8J,_8K);});},_9P=function(_8J,_8K){return new F(function(){return _9G(_9M,_9N,_8J,_8K);});};return [0,_9P,_9O];},_9Q=new T(function(){return B(_9L(_8X,_8X));}),_9R=new T(function(){return B(_8E(_9Q,_9m,_9m));}),_9S=function(_9T,_9U,_9V){_9U=E(_9U);_9V=E(_9V);if(!_9V[0]){var _9W=_9V[2],_9X=_9V[3],_9Y=_9V[4];switch(B(A(_85,[_9T,_9U,_9W]))){case 0:return new F(function(){return _51(_9W,B(_9S(_9T,_9U,_9X)),_9Y);});break;case 1:return [0,_9V[1],E(_9U),E(_9X),E(_9Y)];default:return new F(function(){return _4r(_9W,_9X,B(_9S(_9T,_9U,_9Y)));});}}else{return [0,1,E(_9U),E(_4m),E(_4m)];}},_9Z=function(_a0,_a1){while(1){_a1=E(_a1);if(!_a1[0]){return E(_a0);}else{var _a2=B(_9S(_9R,_a1[1],_a0)),_a3=_a1[2];_a0=_a2;_a1=_a3;continue;}}},_a4=function(_a5,_a6,_a7){return new F(function(){return _9Z(B(_9S(_9R,_a6,_a5)),_a7);});},_a8=function(_a9,_aa,_ab){_ab=E(_ab);if(!_ab[0]){return E(_aa);}else{var _ac=_ab[1],_ad=_ab[2];_ad=E(_ad);if(!_ad[0]){return new F(function(){return _4U(_ac,_aa);});}else{var _ae=_ad[1],_af=_ad[2];_ac=E(_ac);var _ag=_ac[1],_ah=_ac[2];_ae=E(_ae);var _ai=_ae[1],_aj=_ae[2];_ag=E(_ag);_ai=E(_ai);var _ak=_ai,_al=function(_am){var _an=B(_6E(_a9,_ak,_aj,_af)),_ao=_an[1],_ap=_an[3];_ap=E(_ap);if(!_ap[0]){return new F(function(){return _a8(_a9<<1,B(_63(_ac,_aa,_ao)),_an[2]);});}else{return new F(function(){return _a4(B(_63(_ac,_aa,_ao)),_ap[1],_ap[2]);});}};if(_ag>=_ak){if(_ag!=_ak){return new F(function(){return _a4(_aa,_ac,_ad);});}else{_ah=E(_ah);_aj=E(_aj);if(_ah<_aj){return new F(function(){return _al(_);});}else{return new F(function(){return _a4(_aa,_ac,_ad);});}}}else{return new F(function(){return _al(_);});}}}},_aq=function(_ar,_as,_at,_au,_av){_av=E(_av);if(!_av[0]){return new F(function(){return _4U([0,_at,_au],_as);});}else{var _aw=_av[1],_ax=_av[2];_aw=E(_aw);var _ay=_aw[1],_az=_aw[2];_ay=E(_ay);var _aA=_ay,_aB=function(_aC){var _aD=B(_6E(_ar,_aA,_az,_ax)),_aE=_aD[1],_aF=_aD[3];_aF=E(_aF);if(!_aF[0]){return new F(function(){return _a8(_ar<<1,B(_63([0,_at,_au],_as,_aE)),_aD[2]);});}else{return new F(function(){return _a4(B(_63([0,_at,_au],_as,_aE)),_aF[1],_aF[2]);});}};if(_at>=_aA){if(_at!=_aA){return new F(function(){return _a4(_as,[0,_at,_au],_av);});}else{_az=E(_az);if(_au<_az){return new F(function(){return _aB(_);});}else{return new F(function(){return _a4(_as,[0,_at,_au],_av);});}}}else{return new F(function(){return _aB(_);});}}},_aG=function(_aH,_aI,_aJ,_aK,_aL){_aL=E(_aL);if(!_aL[0]){return new F(function(){return _4U([0,_aJ,_aK],_aI);});}else{var _aM=_aL[1],_aN=_aL[2];_aM=E(_aM);var _aO=_aM[1],_aP=_aM[2];_aO=E(_aO);var _aQ=_aO,_aR=function(_aS){var _aT=B(_6E(_aH,_aQ,_aP,_aN)),_aU=_aT[1],_aV=_aT[3];_aV=E(_aV);if(!_aV[0]){return new F(function(){return _a8(_aH<<1,B(_63([0,_aJ,_aK],_aI,_aU)),_aT[2]);});}else{return new F(function(){return _a4(B(_63([0,_aJ,_aK],_aI,_aU)),_aV[1],_aV[2]);});}};if(_aJ>=_aQ){if(_aJ!=_aQ){return new F(function(){return _a4(_aI,[0,_aJ,_aK],_aL);});}else{_aK=E(_aK);_aP=E(_aP);if(_aK<_aP){return new F(function(){return _aR(_);});}else{return new F(function(){return _a4(_aI,[0,_aJ,_aK],_aL);});}}}else{return new F(function(){return _aR(_);});}}},_aW=function(_aX){_aX=E(_aX);if(!_aX[0]){return [1];}else{var _aY=_aX[1],_aZ=_aX[2];_aZ=E(_aZ);if(!_aZ[0]){_aY=E(_aY);return [0,1,E(_aY),E(_4m),E(_4m)];}else{var _b0=_aZ[1],_b1=_aZ[2];_aY=E(_aY);var _b2=_aY[1],_b3=_aY[2];_b0=E(_b0);var _b4=_b0[1],_b5=_b0[2];_b2=E(_b2);_b4=E(_b4);if(_b2>=_b4){if(_b2!=_b4){return new F(function(){return _a4([0,1,E(_aY),E(_4m),E(_4m)],_b0,_b1);});}else{_b3=E(_b3);_b5=E(_b5);if(_b3<_b5){return new F(function(){return _aq(1,[0,1,E(_aY),E(_4m),E(_4m)],_b4,_b5,_b1);});}else{return new F(function(){return _a4([0,1,E(_aY),E(_4m),E(_4m)],_b0,_b1);});}}}else{return new F(function(){return _aG(1,[0,1,E(_aY),E(_4m),E(_4m)],_b4,_b5,_b1);});}}}},_b6=45,_b7=function(_b8,_b9,_ba){var _bb=function(_bc){_b9=E(_b9);var _bd=new T(function(){return B(A(_b8,[ -_ba]));});if(_b9<=6){var _be=function(_bf){var _bg=new T(function(){return B(A(_bd,[_bf]));});return [1,_b6,_bg];};return E(_be);}else{var _bh=function(_bi){var _bj=new T(function(){return B(A(_bd,[[1,_Q,_bi]]));});return [1,_R,[1,_b6,_bj]];};return E(_bh);}};if(_ba>=0){var _bk=isDoubleNegativeZero(_ba);_bk=E(_bk);if(!_bk){return new F(function(){return A(_b8,[_ba]);});}else{return new F(function(){return _bb(_);});}}else{return new F(function(){return _bb(_);});}},_bl=function(_bm,_bn,_bo){while(1){_bo=E(_bo);if(!_bo[0]){var _bp=_bo[2],_bq=_bo[3],_br=_bo[4];_bp=E(_bp);var _bs=_bp[1],_bt=_bp[2];_bs=E(_bs);if(_bm>=_bs){if(_bm!=_bs){_bo=_br;continue;}else{_bt=E(_bt);if(_bn>=_bt){if(_bn!=_bt){_bo=_br;continue;}else{return true;}}else{_bo=_bq;continue;}}}else{_bo=_bq;continue;}}else{return false;}}},_bu=function(_bv,_bw,_bx){_bx=E(_bx);if(!_bx[0]){var _by=_bx[2],_bz=_bx[3],_bA=_bx[4];_by=E(_by);var _bB=_by[1],_bC=_by[2];_bv=E(_bv);_bB=E(_bB);if(_bv>=_bB){if(_bv!=_bB){return new F(function(){return _bl(_bv,_bw,_bA);});}else{_bC=E(_bC);if(_bw>=_bC){if(_bw!=_bC){return new F(function(){return _bl(_bv,_bw,_bA);});}else{return true;}}else{return new F(function(){return _bl(_bv,_bw,_bz);});}}}else{return new F(function(){return _bl(_bv,_bw,_bz);});}}else{return false;}},_bD=new T(function(){return toJSStr(_r);}),_bE="rgb(",_bF=44,_bG=[1,_bF,_r],_bH=new T(function(){return toJSStr(_bG);}),_bI="rgba(",_bJ=41,_bK=[1,_bJ,_r],_bL=new T(function(){return toJSStr(_bK);}),_bM=[1,_bL,_r],_bN=function(_bO){_bO=E(_bO);if(!_bO[0]){var _bP=_bO[1],_bQ=_bO[2],_bR=_bO[3],_bS=new T(function(){return String(_bR);}),_bT=new T(function(){return String(_bQ);}),_bU=new T(function(){return String(_bP);});_bD=E(_bD);var _bV=jsCat([1,_bE,[1,_bU,[1,_bH,[1,_bT,[1,_bH,[1,_bS,_bM]]]]]],_bD);return E(_bV);}else{var _bW=_bO[4],_bX=_bO[1],_bY=_bO[2],_bZ=_bO[3],_c0=new T(function(){return String(_bW);}),_c1=new T(function(){return String(_bZ);}),_c2=new T(function(){return String(_bY);}),_c3=new T(function(){return String(_bX);});_bD=E(_bD);var _c4=jsCat([1,_bI,[1,_c3,[1,_bH,[1,_c2,[1,_bH,[1,_c1,[1,_bH,[1,_c0,_bM]]]]]]]],_bD);return E(_c4);}},_c5=0,_c6=function(_c7,_){return _c5;},_c8="strokeStyle",_c9="fillStyle",_ca=function(_cb,_cc){if(_cb<=_cc){var _cd=function(_ce){var _cf=new T(function(){if(_ce!=_cc){return B(_cd(_ce+1|0));}else{return [0];}});return [1,_ce,_cf];};return new F(function(){return _cd(_cb);});}else{return [0];}},_cg=new T(function(){return 20*Math.sqrt(3)/2;}),_ch=[0,0,0,0],_ci=new T(function(){return B(_bN(_ch));}),_cj=new T(function(){return 20*Math.sqrt(3)/2;}),_ck=[0,187,187,187],_cl=[0,187,187,17],_cm=function(_cn,_){return _c5;},_co=function(_cp){_cp=E(_cp);if(!_cp[0]){return E(_cm);}else{var _cq=_cp[1],_cr=_cp[2];_cq=E(_cq);var _cs=_cq[1],_ct=_cq[2],_cu=function(_cv,_){_cv=E(_cv);var _cw=_cv;_cs=E(_cs);_ct=E(_ct);var _cx=jsMoveTo(_cw,_cs,_ct),_cy=_cr,_=_;while(1){_cy=E(_cy);if(!_cy[0]){return _c5;}else{var _cz=_cy[1];_cz=E(_cz);var _cA=_cz[1],_cB=_cz[2];_cA=E(_cA);_cB=E(_cB);var _cC=jsLineTo(_cw,_cA,_cB),_cD=_cy[2];_cy=_cD;continue;}}};return E(_cu);}},_cE=function(_cF,_cG,_cH,_cI,_cJ,_){var _cK=jsResetCanvas(_cG);_c8=E(_c8);_ci=E(_ci);var _cL=jsSet(_cF,_c8,_ci),_cM=_cI-1|0;if(0<=_cM){var _cN=new T(function(){_cH=E(_cH);return B(_ca(0,_cH-1|0));}),_cO=function(_cP){var _cQ=new T(function(){var _cR=2+30*_cP,_cS=_cR+30,_cT=_cR+10,_cU=new T(function(){if(!(_cP%2)){return true;}else{return false;}}),_cV=_cR+40,_cW=_cR,_cX=function(_cY){_cY=E(_cY);if(!_cY[0]){return E(_c6);}else{var _cZ=_cY[1],_d0=_cY[2],_d1=new T(function(){if(!B(_bu(_cZ,_cP,_cJ))){return B(_bN(_ck));}else{return B(_bN(_cl));}}),_d2=new T(function(){var _d3=new T(function(){_cU=E(_cU);if(!_cU){_cZ=E(_cZ);return 2+20*Math.sqrt(3)*(_cZ+0.5);}else{_cZ=E(_cZ);return 2+20*Math.sqrt(3)*_cZ;}}),_d4=new T(function(){_d3=E(_d3);_cj=E(_cj);return _d3+_cj;}),_d5=new T(function(){_d3=E(_d3);return _d3+20*Math.sqrt(3);}),_d6=new T(function(){_d3=E(_d3);return _d3+20*Math.sqrt(3);}),_d7=new T(function(){_d3=E(_d3);_cg=E(_cg);return _d3+_cg;});return B(_co([1,[0,_d3,_cT],[1,[0,_d7,_cW],[1,[0,_d6,_cT],[1,[0,_d5,_cS],[1,[0,_d4,_cV],[1,[0,_d3,_cS],_r]]]]]]));}),_d8=new T(function(){return B(_cX(_d0));}),_d9=function(_da,_){_da=E(_da);_c9=E(_c9);_d1=E(_d1);var _db=jsSet(_da,_c9,_d1),_dc=jsBeginPath(_da),_dd=B(A(_d2,[_da,_])),_de=jsFill(_da),_df=jsBeginPath(_da),_dg=B(A(_d2,[_da,_])),_dh=jsStroke(_da);return new F(function(){return A(_d8,[_da,_]);});};return E(_d9);}};return B(_cX(_cN));}),_di=new T(function(){if(_cP!=_cM){return B(_cO(_cP+1|0));}else{return E(_c6);}}),_dj=function(_dk,_){var _dl=B(A(_cQ,[_dk,_]));return new F(function(){return A(_di,[_dk,_]);});};return E(_dj);};return new F(function(){return A(_cO,[0,_cF,_]);});}else{return _c5;}},_dm=function(_dn){_dn=E(_dn);return E(_dn[1]);},_do=function(_dp){_dp=E(_dp);return E(_dp[2]);},_dq=function(_dr){_dr=E(_dr);return E(_dr[1]);},_ds=function(_){return new F(function(){return nMV(_2P);});},_dt=function(_du){var _dv=B(A(_du,[_]));return E(_dv);},_dw=new T(function(){return B(_dt(_ds));}),_dx=new T(function(){return __jsNull();}),_dy=new T(function(){return E(_dx);}),_dz=new T(function(){return E(_dy);}),_dA=(function(e,name,f){e.addEventListener(name,f,false);return [f];}),_dB=function(_dC,_dD,_dE,_dF,_dG,_dH,_dI,_dJ){_dC=E(_dC);var _dK=_dC[1],_dL=_dC[3],_dM=new T(function(){return B(A(_dm,[_dF,_dH]));}),_dN=new T(function(){return B(A(_dq,[_dG,_dI]));}),_dO=function(_dP){return new F(function(){return A(_dL,[[0,_dN,_dM,_dP]]);});},_dQ=function(_dR){var _dS=new T(function(){var _dT=new T(function(){_dM=E(_dM);var _dU=_dM;_dN=E(_dN);var _dV=_dN;_dR=E(_dR);var _dW=function(_dX,_){var _dY=B(A(_dR,[_dX,_]));return _dz;};_dW=E(_dW);var _dZ=__createJSFunc(2,_dW),_e0=_dZ,_e1=function(_){_dA=E(_dA);return new F(function(){return _dA(_dU,_dV,_e0);});};return E(_e1);});return B(A(_dD,[_dT]));});return new F(function(){return A(_dK,[_dS,_dO]);});},_e2=new T(function(){var _e3=new T(function(){return B(_do(_dG));}),_e4=function(_e5){var _e6=new T(function(){var _e7=function(_){_dw=E(_dw);var _=wMV(_dw,[1,_e5]);return new F(function(){return A(_e3,[_dI,_e5,_]);});};return B(A(_dD,[_e7]));});return new F(function(){return A(_dK,[_e6,_dJ]);});};return B(A(_dE,[_e4]));});return new F(function(){return A(_dK,[_e2,_dQ]);});},_e8=0,_e9=new T(function(){return B(unCStr("Pattern match failure in do expression at Viewer.hs:44:7-17"));}),_ea=function(_eb,_){_eb=E(_eb);if(!_eb[0]){return _r;}else{var _ec=_eb[1];_ec=E(_ec);var _ed=jsFind(_ec),_ee=B(_ea(_eb[2],_));return [1,_ed,_ee];}},_ef=function(_eg,_eh){while(1){_eh=E(_eh);if(!_eh[0]){return false;}else{if(!B(A(_eg,[_eh[1]]))){var _ei=_eh[2];_eh=_ei;continue;}else{return true;}}}},_ej=new T(function(){return B(unCStr("Control.Exception.Base"));}),_ek=new T(function(){return B(unCStr("base"));}),_el=new T(function(){return B(unCStr("PatternMatchFail"));}),_em=new T(function(){var _en=hs_wordToWord64(18445595),_eo=hs_wordToWord64(52003073);return [0,_en,_eo,[0,_en,_eo,_ek,_ej,_el],_r];}),_ep=function(_eq){return E(_em);},_er=function(_es){_es=E(_es);return new F(function(){return _1m(B(_1k(_es[1])),_ep,_es[2]);});},_et=function(_eu){_eu=E(_eu);return E(_eu[1]);},_ev=function(_ew,_ex){_ew=E(_ew);return new F(function(){return _H(_ew[1],_ex);});},_ey=function(_ez,_eA){return new F(function(){return _2p(_ev,_ez,_eA);});},_eB=function(_eC,_eD,_eE){_eD=E(_eD);return new F(function(){return _H(_eD[1],_eE);});},_eF=[0,_eB,_et,_ey],_eG=new T(function(){return [0,_ep,_eF,_eH,_er];}),_eH=function(_eI){return [0,_eG,_eI];},_eJ=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_eK=function(_eL,_eM){var _eN=new T(function(){return B(A(_eM,[_eL]));});return new F(function(){return die(_eN);});},_eO=function(_eP,_eQ){_eQ=E(_eQ);if(!_eQ[0]){return [0,_r,_r];}else{var _eR=_eQ[1],_eS=_eQ[2];if(!B(A(_eP,[_eR]))){return [0,_r,_eQ];}else{var _eT=new T(function(){var _eU=B(_eO(_eP,_eS));return [0,_eU[1],_eU[2]];}),_eV=new T(function(){_eT=E(_eT);return E(_eT[2]);}),_eW=new T(function(){_eT=E(_eT);return E(_eT[1]);});return [0,[1,_eR,_eW],_eV];}}},_eX=32,_eY=10,_eZ=[1,_eY,_r],_f0=function(_f1){_f1=E(_f1);return (_f1==124)?false:true;},_f2=function(_f3,_f4){var _f5=B(_eO(_f0,B(unCStr(_f3)))),_f6=_f5[1],_f7=_f5[2],_f8=function(_f9,_fa){var _fb=new T(function(){var _fc=new T(function(){var _fd=new T(function(){return B(_H(_fa,_eZ));},1);return B(_H(_f4,_fd));});return B(unAppCStr(": ",_fc));},1);return new F(function(){return _H(_f9,_fb);});};_f7=E(_f7);if(!_f7[0]){return new F(function(){return _f8(_f6,_r);});}else{var _fe=_f7[1];_fe=E(_fe);if(_fe==124){return new F(function(){return _f8(_f6,[1,_eX,_f7[2]]);});}else{return new F(function(){return _f8(_f6,_r);});}}},_ff=function(_fg){var _fh=new T(function(){return B(_f2(_fg,_eJ));});return new F(function(){return _eK([0,_fh],_eH);});},_fi=function(_fj){return new F(function(){return _ff("Viewer.hs:(23,40)-(47,13)|lambda");});},_fk=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_fl=new T(function(){return B(err(_fk));}),_fm=function(_fn){_fn=E(_fn);return (_fn[0]==0)?E(_fl):E(_fn[1]);},_fo=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_fp=function(_fq){var _fr=new T(function(){return B(_f2(_fq,_fo));});return new F(function(){return _eK([0,_fr],_eH);});},_fs=new T(function(){return B(_fp("Viewer.hs:42:109-126|Haste.JSON.Num x"));}),_ft=120,_fu=[1,_ft,_r],_fv=new T(function(){return toJSStr(_fu);}),_fw=new T(function(){return B(_fp("Viewer.hs:42:133-150|Haste.JSON.Num y"));}),_fx=121,_fy=[1,_fx,_r],_fz=new T(function(){return toJSStr(_fy);}),_fA=function(_fB){_fB=E(_fB);if(!_fB[0]){return [0];}else{var _fC=_fB[1],_fD=_fB[2],_fE=new T(function(){return B(_fA(_fD));}),_fF=new T(function(){var _fG=B(_n(_fC,_fz));if(!_fG[0]){return jsRound(_fG[1]);}else{return E(_fw);}}),_fH=new T(function(){var _fI=B(_n(_fC,_fv));if(!_fI[0]){return jsRound(_fI[1]);}else{return E(_fs);}});return [1,[0,_fH,_fF],_fE];}},_fJ=function(_fK){_fK=E(_fK);return (_fK[0]==0)?true:false;},_fL=new T(function(){return B(_fp("Viewer.hs:37:11-35|Data.Either.Right json"));}),_fM=new T(function(){return B(_fp("Viewer.hs:42:59-86|Haste.JSON.Arr filled"));}),_fN="filled",_fO=new T(function(){return B(_fp("Viewer.hs:39:11-39|Haste.JSON.Num height\'"));}),_fP="height",_fQ=new T(function(){return B(_fp("Viewer.hs:38:11-38|Haste.JSON.Num width\'"));}),_fR="width",_fS=new T(function(){return B(unCStr("px"));}),_fT=new T(function(){return B(unCStr("Pattern match failure in do expression at Viewer.hs:32:7-17"));}),_fU=new T(function(){return B(unCStr("width"));}),_fV=new T(function(){return B(unCStr("height"));}),_fW="value",_fX=function(_fY,_fZ){_fZ=E(_fZ);if(!_fZ[0]){return [0];}else{var _g0=_fZ[1],_g1=_fZ[2],_g2=new T(function(){return B(_fX(_fY,_g1));}),_g3=new T(function(){return B(A(_fY,[_g0]));});return [1,_g3,_g2];}},_g4=0,_g5=new T(function(){return B(unCStr("board"));}),_g6=new T(function(){return B(unCStr("input"));}),_g7=new T(function(){return B(unCStr("load"));}),_g8=[1,_g7,_r],_g9=[1,_g6,_g8],_ga=[1,_g5,_g9],_gb=function(_gc){_gc=E(_gc);return new F(function(){return toJSStr(_gc);});},_gd=new T(function(){return B(_fX(_gb,_ga));}),_ge=5,_gf=function(_gg,_gh){while(1){var _gi=(function(_gj,_gk){_gj=E(_gj);if(!_gj[0]){return [0];}else{var _gl=_gj[2];_gk=E(_gk);if(!_gk[0]){return [0];}else{var _gm=_gk[1],_gn=_gk[2];_gm=E(_gm);if(!_gm[0]){var _go=new T(function(){return B(_gf(_gl,_gn));});return [1,_gj[1],_go];}else{_gg=_gl;_gh=_gn;return null;}}}})(_gg,_gh);if(_gi!=null){return _gi;}}},_gp=new T(function(){return B(unAppCStr("[]",_r));}),_gq=[1,_2n,_r],_gr=function(_gs){_gs=E(_gs);if(!_gs[0]){return E(_gq);}else{var _gt=_gs[1],_gu=_gs[2],_gv=new T(function(){_gt=E(_gt);var _gw=new T(function(){return B(_gr(_gu));},1);return B(_H(fromJSStr(_gt),_gw));});return [1,_2m,_gv];}},_gx=function(_gy,_gz){var _gA=new T(function(){var _gB=B(_gf(_gz,_gy));if(!_gB[0]){return E(_gp);}else{var _gC=_gB[1],_gD=_gB[2],_gE=new T(function(){_gC=E(_gC);var _gF=new T(function(){return B(_gr(_gD));},1);return B(_H(fromJSStr(_gC),_gF));});return [1,_2o,_gE];}});return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",_gA)));});},_gG=function(_){var _gH=B(_ea(_gd,_));if(!B(_ef(_fJ,_gH))){var _gI=B(_fX(_fm,_gH));if(!_gI[0]){return new F(function(){return _fi(_);});}else{var _gJ=_gI[1],_gK=_gI[2];_gK=E(_gK);if(!_gK[0]){return new F(function(){return _fi(_);});}else{var _gL=_gK[1],_gM=_gK[2];_gM=E(_gM);if(!_gM[0]){return new F(function(){return _fi(_);});}else{var _gN=_gM[2];_gN=E(_gN);if(!_gN[0]){var _gO=function(_gP,_gQ,_){_gJ=E(_gJ);_fU=E(_fU);var _gR=jsSetAttr(_gJ,toJSStr(_fU),toJSStr(B(_H(B(A(_b7,[_4i,_g4,20*Math.sqrt(3)*(_gP+0.5)+5,_r])),_fS))));_fV=E(_fV);var _gS=jsSetAttr(_gJ,toJSStr(_fV),toJSStr(B(_H(B(A(_b7,[_4i,_g4,30*_gQ+20+5,_r])),_fS))));return _c5;},_gT=B(_gO(5,10,_));_gJ=E(_gJ);var _gU=_gJ,_gV=jsHasCtx2D(_gU);_gV=E(_gV);if(!_gV){return new F(function(){return _2X(_fT,_);});}else{var _gW=jsGetCtx2D(_gU),_gX=B(_cE(_gW,_gU,_ge,10,_4m,_)),_gY=function(_gZ,_){_gL=E(_gL);_fW=E(_fW);var _h0=jsGet(_gL,_fW),_h1=jsParseJSON(_h0);if(!_h1[0]){return E(_fL);}else{var _h2=_h1[1],_h3=B(_n(_h2,_fR));if(!_h3[0]){var _h4=jsRound(_h3[1]),_h5=_h4,_h6=B(_n(_h2,_fP));if(!_h6[0]){var _h7=jsRound(_h6[1]),_h8=_h7,_h9=B(_gO(_h5,_h8,_)),_ha=jsHasCtx2D(_gU),_hb=function(_,_hc){_hc=E(_hc);if(!_hc[0]){return new F(function(){return _2X(_e9,_);});}else{var _hd=_hc[1];_hd=E(_hd);var _he=new T(function(){var _hf=B(_n(_h2,_fN));if(_hf[0]==3){return B(_aW(B(_fA(_hf[1]))));}else{return E(_fM);}});return new F(function(){return _cE(_hd[1],_hd[2],_h5,_h8,_he,_);});}};_ha=E(_ha);if(!_ha){return new F(function(){return _hb(_,_2P);});}else{var _hg=jsGetCtx2D(_gU);return new F(function(){return _hb(_,[1,[0,_hg,_gU]]);});}}else{return E(_fO);}}else{return E(_fQ);}}},_hh=B(A(_dB,[_4e,_3X,_48,_3Z,_3U,_gM[1],_e8,_gY,_]));return _c5;}}else{return new F(function(){return _fi(_);});}}}}}else{return new F(function(){return _gx(_gH,_gd);});}},_hi=function(_){return new F(function(){return _gG(_);});};
var hasteMain = function() {B(A(_hi, [0]));};window.onload = hasteMain;