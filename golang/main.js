"use strict";
(function() {

Error.stackTraceLimit = Infinity;

var $global, $module;
if (typeof window !== "undefined") { /* web page */
  $global = window;
} else if (typeof self !== "undefined") { /* web worker */
  $global = self;
} else if (typeof global !== "undefined") { /* Node.js */
  $global = global;
  $global.require = require;
} else { /* others (e.g. Nashorn) */
  $global = this;
}

if ($global === undefined || $global.Array === undefined) {
  throw new Error("no global object found");
}
if (typeof module !== "undefined") {
  $module = module;
}

var $packages = {}, $idCounter = 0;
var $keys = function(m) { return m ? Object.keys(m) : []; };
var $flushConsole = function() {};
var $throwRuntimeError; /* set by package "runtime" */
var $throwNilPointerError = function() { $throwRuntimeError("invalid memory address or nil pointer dereference"); };
var $call = function(fn, rcvr, args) { return fn.apply(rcvr, args); };
var $makeFunc = function(fn) { return function() { return fn(new ($sliceType($jsObjectPtr))($global.Array.prototype.slice.call(arguments, []))); } };

var $mapArray = function(array, f) {
  var newArray = new array.constructor(array.length);
  for (var i = 0; i < array.length; i++) {
    newArray[i] = f(array[i]);
  }
  return newArray;
};

var $methodVal = function(recv, name) {
  var vals = recv.$methodVals || {};
  recv.$methodVals = vals; /* noop for primitives */
  var f = vals[name];
  if (f !== undefined) {
    return f;
  }
  var method = recv[name];
  f = function() {
    $stackDepthOffset--;
    try {
      return method.apply(recv, arguments);
    } finally {
      $stackDepthOffset++;
    }
  };
  vals[name] = f;
  return f;
};

var $methodExpr = function(typ, name) {
  var method = typ.prototype[name];
  if (method.$expr === undefined) {
    method.$expr = function() {
      $stackDepthOffset--;
      try {
        if (typ.wrapped) {
          arguments[0] = new typ(arguments[0]);
        }
        return Function.call.apply(method, arguments);
      } finally {
        $stackDepthOffset++;
      }
    };
  }
  return method.$expr;
};

var $ifaceMethodExprs = {};
var $ifaceMethodExpr = function(name) {
  var expr = $ifaceMethodExprs["$" + name];
  if (expr === undefined) {
    expr = $ifaceMethodExprs["$" + name] = function() {
      $stackDepthOffset--;
      try {
        return Function.call.apply(arguments[0][name], arguments);
      } finally {
        $stackDepthOffset++;
      }
    };
  }
  return expr;
};

var $subslice = function(slice, low, high, max) {
  if (low < 0 || high < low || max < high || high > slice.$capacity || max > slice.$capacity) {
    $throwRuntimeError("slice bounds out of range");
  }
  var s = new slice.constructor(slice.$array);
  s.$offset = slice.$offset + low;
  s.$length = slice.$length - low;
  s.$capacity = slice.$capacity - low;
  if (high !== undefined) {
    s.$length = high - low;
  }
  if (max !== undefined) {
    s.$capacity = max - low;
  }
  return s;
};

var $sliceToArray = function(slice) {
  if (slice.$length === 0) {
    return [];
  }
  if (slice.$array.constructor !== Array) {
    return slice.$array.subarray(slice.$offset, slice.$offset + slice.$length);
  }
  return slice.$array.slice(slice.$offset, slice.$offset + slice.$length);
};

var $decodeRune = function(str, pos) {
  var c0 = str.charCodeAt(pos);

  if (c0 < 0x80) {
    return [c0, 1];
  }

  if (c0 !== c0 || c0 < 0xC0) {
    return [0xFFFD, 1];
  }

  var c1 = str.charCodeAt(pos + 1);
  if (c1 !== c1 || c1 < 0x80 || 0xC0 <= c1) {
    return [0xFFFD, 1];
  }

  if (c0 < 0xE0) {
    var r = (c0 & 0x1F) << 6 | (c1 & 0x3F);
    if (r <= 0x7F) {
      return [0xFFFD, 1];
    }
    return [r, 2];
  }

  var c2 = str.charCodeAt(pos + 2);
  if (c2 !== c2 || c2 < 0x80 || 0xC0 <= c2) {
    return [0xFFFD, 1];
  }

  if (c0 < 0xF0) {
    var r = (c0 & 0x0F) << 12 | (c1 & 0x3F) << 6 | (c2 & 0x3F);
    if (r <= 0x7FF) {
      return [0xFFFD, 1];
    }
    if (0xD800 <= r && r <= 0xDFFF) {
      return [0xFFFD, 1];
    }
    return [r, 3];
  }

  var c3 = str.charCodeAt(pos + 3);
  if (c3 !== c3 || c3 < 0x80 || 0xC0 <= c3) {
    return [0xFFFD, 1];
  }

  if (c0 < 0xF8) {
    var r = (c0 & 0x07) << 18 | (c1 & 0x3F) << 12 | (c2 & 0x3F) << 6 | (c3 & 0x3F);
    if (r <= 0xFFFF || 0x10FFFF < r) {
      return [0xFFFD, 1];
    }
    return [r, 4];
  }

  return [0xFFFD, 1];
};

var $encodeRune = function(r) {
  if (r < 0 || r > 0x10FFFF || (0xD800 <= r && r <= 0xDFFF)) {
    r = 0xFFFD;
  }
  if (r <= 0x7F) {
    return String.fromCharCode(r);
  }
  if (r <= 0x7FF) {
    return String.fromCharCode(0xC0 | r >> 6, 0x80 | (r & 0x3F));
  }
  if (r <= 0xFFFF) {
    return String.fromCharCode(0xE0 | r >> 12, 0x80 | (r >> 6 & 0x3F), 0x80 | (r & 0x3F));
  }
  return String.fromCharCode(0xF0 | r >> 18, 0x80 | (r >> 12 & 0x3F), 0x80 | (r >> 6 & 0x3F), 0x80 | (r & 0x3F));
};

var $stringToBytes = function(str) {
  var array = new Uint8Array(str.length);
  for (var i = 0; i < str.length; i++) {
    array[i] = str.charCodeAt(i);
  }
  return array;
};

var $bytesToString = function(slice) {
  if (slice.$length === 0) {
    return "";
  }
  var str = "";
  for (var i = 0; i < slice.$length; i += 10000) {
    str += String.fromCharCode.apply(undefined, slice.$array.subarray(slice.$offset + i, slice.$offset + Math.min(slice.$length, i + 10000)));
  }
  return str;
};

var $stringToRunes = function(str) {
  var array = new Int32Array(str.length);
  var rune, j = 0;
  for (var i = 0; i < str.length; i += rune[1], j++) {
    rune = $decodeRune(str, i);
    array[j] = rune[0];
  }
  return array.subarray(0, j);
};

var $runesToString = function(slice) {
  if (slice.$length === 0) {
    return "";
  }
  var str = "";
  for (var i = 0; i < slice.$length; i++) {
    str += $encodeRune(slice.$array[slice.$offset + i]);
  }
  return str;
};

var $copyString = function(dst, src) {
  var n = Math.min(src.length, dst.$length);
  for (var i = 0; i < n; i++) {
    dst.$array[dst.$offset + i] = src.charCodeAt(i);
  }
  return n;
};

var $copySlice = function(dst, src) {
  var n = Math.min(src.$length, dst.$length);
  $copyArray(dst.$array, src.$array, dst.$offset, src.$offset, n, dst.constructor.elem);
  return n;
};

var $copyArray = function(dst, src, dstOffset, srcOffset, n, elem) {
  if (n === 0 || (dst === src && dstOffset === srcOffset)) {
    return;
  }

  if (src.subarray) {
    dst.set(src.subarray(srcOffset, srcOffset + n), dstOffset);
    return;
  }

  switch (elem.kind) {
  case $kindArray:
  case $kindStruct:
    if (dst === src && dstOffset > srcOffset) {
      for (var i = n - 1; i >= 0; i--) {
        elem.copy(dst[dstOffset + i], src[srcOffset + i]);
      }
      return;
    }
    for (var i = 0; i < n; i++) {
      elem.copy(dst[dstOffset + i], src[srcOffset + i]);
    }
    return;
  }

  if (dst === src && dstOffset > srcOffset) {
    for (var i = n - 1; i >= 0; i--) {
      dst[dstOffset + i] = src[srcOffset + i];
    }
    return;
  }
  for (var i = 0; i < n; i++) {
    dst[dstOffset + i] = src[srcOffset + i];
  }
};

var $clone = function(src, type) {
  var clone = type.zero();
  type.copy(clone, src);
  return clone;
};

var $pointerOfStructConversion = function(obj, type) {
  if(obj.$proxies === undefined) {
    obj.$proxies = {};
    obj.$proxies[obj.constructor.string] = obj;
  }
  var proxy = obj.$proxies[type.string];
  if (proxy === undefined) {
    var properties = {};
    for (var i = 0; i < type.elem.fields.length; i++) {
      (function(fieldProp) {
        properties[fieldProp] = {
          get: function() { return obj[fieldProp]; },
          set: function(value) { obj[fieldProp] = value; }
        };
      })(type.elem.fields[i].prop);
    }
    proxy = Object.create(type.prototype, properties);
    proxy.$val = proxy;
    obj.$proxies[type.string] = proxy;
    proxy.$proxies = obj.$proxies;
  }
  return proxy;
};

var $append = function(slice) {
  return $internalAppend(slice, arguments, 1, arguments.length - 1);
};

var $appendSlice = function(slice, toAppend) {
  if (toAppend.constructor === String) {
    var bytes = $stringToBytes(toAppend);
    return $internalAppend(slice, bytes, 0, bytes.length);
  }
  return $internalAppend(slice, toAppend.$array, toAppend.$offset, toAppend.$length);
};

var $internalAppend = function(slice, array, offset, length) {
  if (length === 0) {
    return slice;
  }

  var newArray = slice.$array;
  var newOffset = slice.$offset;
  var newLength = slice.$length + length;
  var newCapacity = slice.$capacity;

  if (newLength > newCapacity) {
    newOffset = 0;
    newCapacity = Math.max(newLength, slice.$capacity < 1024 ? slice.$capacity * 2 : Math.floor(slice.$capacity * 5 / 4));

    if (slice.$array.constructor === Array) {
      newArray = slice.$array.slice(slice.$offset, slice.$offset + slice.$length);
      newArray.length = newCapacity;
      var zero = slice.constructor.elem.zero;
      for (var i = slice.$length; i < newCapacity; i++) {
        newArray[i] = zero();
      }
    } else {
      newArray = new slice.$array.constructor(newCapacity);
      newArray.set(slice.$array.subarray(slice.$offset, slice.$offset + slice.$length));
    }
  }

  $copyArray(newArray, array, newOffset + slice.$length, offset, length, slice.constructor.elem);

  var newSlice = new slice.constructor(newArray);
  newSlice.$offset = newOffset;
  newSlice.$length = newLength;
  newSlice.$capacity = newCapacity;
  return newSlice;
};

var $equal = function(a, b, type) {
  if (type === $jsObjectPtr) {
    return a === b;
  }
  switch (type.kind) {
  case $kindComplex64:
  case $kindComplex128:
    return a.$real === b.$real && a.$imag === b.$imag;
  case $kindInt64:
  case $kindUint64:
    return a.$high === b.$high && a.$low === b.$low;
  case $kindArray:
    if (a.length !== b.length) {
      return false;
    }
    for (var i = 0; i < a.length; i++) {
      if (!$equal(a[i], b[i], type.elem)) {
        return false;
      }
    }
    return true;
  case $kindStruct:
    for (var i = 0; i < type.fields.length; i++) {
      var f = type.fields[i];
      if (!$equal(a[f.prop], b[f.prop], f.typ)) {
        return false;
      }
    }
    return true;
  case $kindInterface:
    return $interfaceIsEqual(a, b);
  default:
    return a === b;
  }
};

var $interfaceIsEqual = function(a, b) {
  if (a === $ifaceNil || b === $ifaceNil) {
    return a === b;
  }
  if (a.constructor !== b.constructor) {
    return false;
  }
  if (a.constructor === $jsObjectPtr) {
    return a.object === b.object;
  }
  if (!a.constructor.comparable) {
    $throwRuntimeError("comparing uncomparable type " + a.constructor.string);
  }
  return $equal(a.$val, b.$val, a.constructor);
};

var $min = Math.min;
var $mod = function(x, y) { return x % y; };
var $parseInt = parseInt;
var $parseFloat = function(f) {
  if (f !== undefined && f !== null && f.constructor === Number) {
    return f;
  }
  return parseFloat(f);
};

var $froundBuf = new Float32Array(1);
var $fround = Math.fround || function(f) {
  $froundBuf[0] = f;
  return $froundBuf[0];
};

var $imul = Math.imul || function(a, b) {
  var ah = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) >> 0);
};

var $floatKey = function(f) {
  if (f !== f) {
    $idCounter++;
    return "NaN$" + $idCounter;
  }
  return String(f);
};

var $flatten64 = function(x) {
  return x.$high * 4294967296 + x.$low;
};

var $shiftLeft64 = function(x, y) {
  if (y === 0) {
    return x;
  }
  if (y < 32) {
    return new x.constructor(x.$high << y | x.$low >>> (32 - y), (x.$low << y) >>> 0);
  }
  if (y < 64) {
    return new x.constructor(x.$low << (y - 32), 0);
  }
  return new x.constructor(0, 0);
};

var $shiftRightInt64 = function(x, y) {
  if (y === 0) {
    return x;
  }
  if (y < 32) {
    return new x.constructor(x.$high >> y, (x.$low >>> y | x.$high << (32 - y)) >>> 0);
  }
  if (y < 64) {
    return new x.constructor(x.$high >> 31, (x.$high >> (y - 32)) >>> 0);
  }
  if (x.$high < 0) {
    return new x.constructor(-1, 4294967295);
  }
  return new x.constructor(0, 0);
};

var $shiftRightUint64 = function(x, y) {
  if (y === 0) {
    return x;
  }
  if (y < 32) {
    return new x.constructor(x.$high >>> y, (x.$low >>> y | x.$high << (32 - y)) >>> 0);
  }
  if (y < 64) {
    return new x.constructor(0, x.$high >>> (y - 32));
  }
  return new x.constructor(0, 0);
};

var $mul64 = function(x, y) {
  var high = 0, low = 0;
  if ((y.$low & 1) !== 0) {
    high = x.$high;
    low = x.$low;
  }
  for (var i = 1; i < 32; i++) {
    if ((y.$low & 1<<i) !== 0) {
      high += x.$high << i | x.$low >>> (32 - i);
      low += (x.$low << i) >>> 0;
    }
  }
  for (var i = 0; i < 32; i++) {
    if ((y.$high & 1<<i) !== 0) {
      high += x.$low << i;
    }
  }
  return new x.constructor(high, low);
};

var $div64 = function(x, y, returnRemainder) {
  if (y.$high === 0 && y.$low === 0) {
    $throwRuntimeError("integer divide by zero");
  }

  var s = 1;
  var rs = 1;

  var xHigh = x.$high;
  var xLow = x.$low;
  if (xHigh < 0) {
    s = -1;
    rs = -1;
    xHigh = -xHigh;
    if (xLow !== 0) {
      xHigh--;
      xLow = 4294967296 - xLow;
    }
  }

  var yHigh = y.$high;
  var yLow = y.$low;
  if (y.$high < 0) {
    s *= -1;
    yHigh = -yHigh;
    if (yLow !== 0) {
      yHigh--;
      yLow = 4294967296 - yLow;
    }
  }

  var high = 0, low = 0, n = 0;
  while (yHigh < 2147483648 && ((xHigh > yHigh) || (xHigh === yHigh && xLow > yLow))) {
    yHigh = (yHigh << 1 | yLow >>> 31) >>> 0;
    yLow = (yLow << 1) >>> 0;
    n++;
  }
  for (var i = 0; i <= n; i++) {
    high = high << 1 | low >>> 31;
    low = (low << 1) >>> 0;
    if ((xHigh > yHigh) || (xHigh === yHigh && xLow >= yLow)) {
      xHigh = xHigh - yHigh;
      xLow = xLow - yLow;
      if (xLow < 0) {
        xHigh--;
        xLow += 4294967296;
      }
      low++;
      if (low === 4294967296) {
        high++;
        low = 0;
      }
    }
    yLow = (yLow >>> 1 | yHigh << (32 - 1)) >>> 0;
    yHigh = yHigh >>> 1;
  }

  if (returnRemainder) {
    return new x.constructor(xHigh * rs, xLow * rs);
  }
  return new x.constructor(high * s, low * s);
};

var $divComplex = function(n, d) {
  var ninf = n.$real === Infinity || n.$real === -Infinity || n.$imag === Infinity || n.$imag === -Infinity;
  var dinf = d.$real === Infinity || d.$real === -Infinity || d.$imag === Infinity || d.$imag === -Infinity;
  var nnan = !ninf && (n.$real !== n.$real || n.$imag !== n.$imag);
  var dnan = !dinf && (d.$real !== d.$real || d.$imag !== d.$imag);
  if(nnan || dnan) {
    return new n.constructor(NaN, NaN);
  }
  if (ninf && !dinf) {
    return new n.constructor(Infinity, Infinity);
  }
  if (!ninf && dinf) {
    return new n.constructor(0, 0);
  }
  if (d.$real === 0 && d.$imag === 0) {
    if (n.$real === 0 && n.$imag === 0) {
      return new n.constructor(NaN, NaN);
    }
    return new n.constructor(Infinity, Infinity);
  }
  var a = Math.abs(d.$real);
  var b = Math.abs(d.$imag);
  if (a <= b) {
    var ratio = d.$real / d.$imag;
    var denom = d.$real * ratio + d.$imag;
    return new n.constructor((n.$real * ratio + n.$imag) / denom, (n.$imag * ratio - n.$real) / denom);
  }
  var ratio = d.$imag / d.$real;
  var denom = d.$imag * ratio + d.$real;
  return new n.constructor((n.$imag * ratio + n.$real) / denom, (n.$imag - n.$real * ratio) / denom);
};

var $kindBool = 1;
var $kindInt = 2;
var $kindInt8 = 3;
var $kindInt16 = 4;
var $kindInt32 = 5;
var $kindInt64 = 6;
var $kindUint = 7;
var $kindUint8 = 8;
var $kindUint16 = 9;
var $kindUint32 = 10;
var $kindUint64 = 11;
var $kindUintptr = 12;
var $kindFloat32 = 13;
var $kindFloat64 = 14;
var $kindComplex64 = 15;
var $kindComplex128 = 16;
var $kindArray = 17;
var $kindChan = 18;
var $kindFunc = 19;
var $kindInterface = 20;
var $kindMap = 21;
var $kindPtr = 22;
var $kindSlice = 23;
var $kindString = 24;
var $kindStruct = 25;
var $kindUnsafePointer = 26;

var $methodSynthesizers = [];
var $addMethodSynthesizer = function(f) {
  if ($methodSynthesizers === null) {
    f();
    return;
  }
  $methodSynthesizers.push(f);
};
var $synthesizeMethods = function() {
  $methodSynthesizers.forEach(function(f) { f(); });
  $methodSynthesizers = null;
};

var $ifaceKeyFor = function(x) {
  if (x === $ifaceNil) {
    return 'nil';
  }
  var c = x.constructor;
  return c.string + '$' + c.keyFor(x.$val);
};

var $identity = function(x) { return x; };

var $typeIDCounter = 0;

var $idKey = function(x) {
  if (x.$id === undefined) {
    $idCounter++;
    x.$id = $idCounter;
  }
  return String(x.$id);
};

var $newType = function(size, kind, string, name, pkg, constructor) {
  var typ;
  switch(kind) {
  case $kindBool:
  case $kindInt:
  case $kindInt8:
  case $kindInt16:
  case $kindInt32:
  case $kindUint:
  case $kindUint8:
  case $kindUint16:
  case $kindUint32:
  case $kindUintptr:
  case $kindUnsafePointer:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.keyFor = $identity;
    break;

  case $kindString:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.keyFor = function(x) { return "$" + x; };
    break;

  case $kindFloat32:
  case $kindFloat64:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.keyFor = function(x) { return $floatKey(x); };
    break;

  case $kindInt64:
    typ = function(high, low) {
      this.$high = (high + Math.floor(Math.ceil(low) / 4294967296)) >> 0;
      this.$low = low >>> 0;
      this.$val = this;
    };
    typ.keyFor = function(x) { return x.$high + "$" + x.$low; };
    break;

  case $kindUint64:
    typ = function(high, low) {
      this.$high = (high + Math.floor(Math.ceil(low) / 4294967296)) >>> 0;
      this.$low = low >>> 0;
      this.$val = this;
    };
    typ.keyFor = function(x) { return x.$high + "$" + x.$low; };
    break;

  case $kindComplex64:
    typ = function(real, imag) {
      this.$real = $fround(real);
      this.$imag = $fround(imag);
      this.$val = this;
    };
    typ.keyFor = function(x) { return x.$real + "$" + x.$imag; };
    break;

  case $kindComplex128:
    typ = function(real, imag) {
      this.$real = real;
      this.$imag = imag;
      this.$val = this;
    };
    typ.keyFor = function(x) { return x.$real + "$" + x.$imag; };
    break;

  case $kindArray:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.ptr = $newType(4, $kindPtr, "*" + string, "", "", function(array) {
      this.$get = function() { return array; };
      this.$set = function(v) { typ.copy(this, v); };
      this.$val = array;
    });
    typ.init = function(elem, len) {
      typ.elem = elem;
      typ.len = len;
      typ.comparable = elem.comparable;
      typ.keyFor = function(x) {
        return Array.prototype.join.call($mapArray(x, function(e) {
          return String(elem.keyFor(e)).replace(/\\/g, "\\\\").replace(/\$/g, "\\$");
        }), "$");
      };
      typ.copy = function(dst, src) {
        $copyArray(dst, src, 0, 0, src.length, elem);
      };
      typ.ptr.init(typ);
      Object.defineProperty(typ.ptr.nil, "nilCheck", { get: $throwNilPointerError });
    };
    break;

  case $kindChan:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.keyFor = $idKey;
    typ.init = function(elem, sendOnly, recvOnly) {
      typ.elem = elem;
      typ.sendOnly = sendOnly;
      typ.recvOnly = recvOnly;
    };
    break;

  case $kindFunc:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.init = function(params, results, variadic) {
      typ.params = params;
      typ.results = results;
      typ.variadic = variadic;
      typ.comparable = false;
    };
    break;

  case $kindInterface:
    typ = { implementedBy: {}, missingMethodFor: {} };
    typ.keyFor = $ifaceKeyFor;
    typ.init = function(methods) {
      typ.methods = methods;
      methods.forEach(function(m) {
        $ifaceNil[m.prop] = $throwNilPointerError;
      });
    };
    break;

  case $kindMap:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.init = function(key, elem) {
      typ.key = key;
      typ.elem = elem;
      typ.comparable = false;
    };
    break;

  case $kindPtr:
    typ = constructor || function(getter, setter, target) {
      this.$get = getter;
      this.$set = setter;
      this.$target = target;
      this.$val = this;
    };
    typ.keyFor = $idKey;
    typ.init = function(elem) {
      typ.elem = elem;
      typ.wrapped = (elem.kind === $kindArray);
      typ.nil = new typ($throwNilPointerError, $throwNilPointerError);
    };
    break;

  case $kindSlice:
    typ = function(array) {
      if (array.constructor !== typ.nativeArray) {
        array = new typ.nativeArray(array);
      }
      this.$array = array;
      this.$offset = 0;
      this.$length = array.length;
      this.$capacity = array.length;
      this.$val = this;
    };
    typ.init = function(elem) {
      typ.elem = elem;
      typ.comparable = false;
      typ.nativeArray = $nativeArray(elem.kind);
      typ.nil = new typ([]);
    };
    break;

  case $kindStruct:
    typ = function(v) { this.$val = v; };
    typ.wrapped = true;
    typ.ptr = $newType(4, $kindPtr, "*" + string, "", "", constructor);
    typ.ptr.elem = typ;
    typ.ptr.prototype.$get = function() { return this; };
    typ.ptr.prototype.$set = function(v) { typ.copy(this, v); };
    typ.init = function(fields) {
      typ.fields = fields;
      fields.forEach(function(f) {
        if (!f.typ.comparable) {
          typ.comparable = false;
        }
      });
      typ.keyFor = function(x) {
        var val = x.$val;
        return $mapArray(fields, function(f) {
          return String(f.typ.keyFor(val[f.prop])).replace(/\\/g, "\\\\").replace(/\$/g, "\\$");
        }).join("$");
      };
      typ.copy = function(dst, src) {
        for (var i = 0; i < fields.length; i++) {
          var f = fields[i];
          switch (f.typ.kind) {
          case $kindArray:
          case $kindStruct:
            f.typ.copy(dst[f.prop], src[f.prop]);
            continue;
          default:
            dst[f.prop] = src[f.prop];
            continue;
          }
        }
      };
      /* nil value */
      var properties = {};
      fields.forEach(function(f) {
        properties[f.prop] = { get: $throwNilPointerError, set: $throwNilPointerError };
      });
      typ.ptr.nil = Object.create(constructor.prototype, properties);
      typ.ptr.nil.$val = typ.ptr.nil;
      /* methods for embedded fields */
      $addMethodSynthesizer(function() {
        var synthesizeMethod = function(target, m, f) {
          if (target.prototype[m.prop] !== undefined) { return; }
          target.prototype[m.prop] = function() {
            var v = this.$val[f.prop];
            if (f.typ === $jsObjectPtr) {
              v = new $jsObjectPtr(v);
            }
            if (v.$val === undefined) {
              v = new f.typ(v);
            }
            return v[m.prop].apply(v, arguments);
          };
        };
        fields.forEach(function(f) {
          if (f.name === "") {
            $methodSet(f.typ).forEach(function(m) {
              synthesizeMethod(typ, m, f);
              synthesizeMethod(typ.ptr, m, f);
            });
            $methodSet($ptrType(f.typ)).forEach(function(m) {
              synthesizeMethod(typ.ptr, m, f);
            });
          }
        });
      });
    };
    break;

  default:
    $panic(new $String("invalid kind: " + kind));
  }

  switch (kind) {
  case $kindBool:
  case $kindMap:
    typ.zero = function() { return false; };
    break;

  case $kindInt:
  case $kindInt8:
  case $kindInt16:
  case $kindInt32:
  case $kindUint:
  case $kindUint8 :
  case $kindUint16:
  case $kindUint32:
  case $kindUintptr:
  case $kindUnsafePointer:
  case $kindFloat32:
  case $kindFloat64:
    typ.zero = function() { return 0; };
    break;

  case $kindString:
    typ.zero = function() { return ""; };
    break;

  case $kindInt64:
  case $kindUint64:
  case $kindComplex64:
  case $kindComplex128:
    var zero = new typ(0, 0);
    typ.zero = function() { return zero; };
    break;

  case $kindPtr:
  case $kindSlice:
    typ.zero = function() { return typ.nil; };
    break;

  case $kindChan:
    typ.zero = function() { return $chanNil; };
    break;

  case $kindFunc:
    typ.zero = function() { return $throwNilPointerError; };
    break;

  case $kindInterface:
    typ.zero = function() { return $ifaceNil; };
    break;

  case $kindArray:
    typ.zero = function() {
      var arrayClass = $nativeArray(typ.elem.kind);
      if (arrayClass !== Array) {
        return new arrayClass(typ.len);
      }
      var array = new Array(typ.len);
      for (var i = 0; i < typ.len; i++) {
        array[i] = typ.elem.zero();
      }
      return array;
    };
    break;

  case $kindStruct:
    typ.zero = function() { return new typ.ptr(); };
    break;

  default:
    $panic(new $String("invalid kind: " + kind));
  }

  typ.id = $typeIDCounter;
  $typeIDCounter++;
  typ.size = size;
  typ.kind = kind;
  typ.string = string;
  typ.typeName = name;
  typ.pkg = pkg;
  typ.methods = [];
  typ.methodSetCache = null;
  typ.comparable = true;
  return typ;
};

var $methodSet = function(typ) {
  if (typ.methodSetCache !== null) {
    return typ.methodSetCache;
  }
  var base = {};

  var isPtr = (typ.kind === $kindPtr);
  if (isPtr && typ.elem.kind === $kindInterface) {
    typ.methodSetCache = [];
    return [];
  }

  var current = [{typ: isPtr ? typ.elem : typ, indirect: isPtr}];

  var seen = {};

  while (current.length > 0) {
    var next = [];
    var mset = [];

    current.forEach(function(e) {
      if (seen[e.typ.string]) {
        return;
      }
      seen[e.typ.string] = true;

      if(e.typ.typeName !== "") {
        mset = mset.concat(e.typ.methods);
        if (e.indirect) {
          mset = mset.concat($ptrType(e.typ).methods);
        }
      }

      switch (e.typ.kind) {
      case $kindStruct:
        e.typ.fields.forEach(function(f) {
          if (f.name === "") {
            var fTyp = f.typ;
            var fIsPtr = (fTyp.kind === $kindPtr);
            next.push({typ: fIsPtr ? fTyp.elem : fTyp, indirect: e.indirect || fIsPtr});
          }
        });
        break;

      case $kindInterface:
        mset = mset.concat(e.typ.methods);
        break;
      }
    });

    mset.forEach(function(m) {
      if (base[m.name] === undefined) {
        base[m.name] = m;
      }
    });

    current = next;
  }

  typ.methodSetCache = [];
  Object.keys(base).sort().forEach(function(name) {
    typ.methodSetCache.push(base[name]);
  });
  return typ.methodSetCache;
};

var $Bool          = $newType( 1, $kindBool,          "bool",           "bool",       "", null);
var $Int           = $newType( 4, $kindInt,           "int",            "int",        "", null);
var $Int8          = $newType( 1, $kindInt8,          "int8",           "int8",       "", null);
var $Int16         = $newType( 2, $kindInt16,         "int16",          "int16",      "", null);
var $Int32         = $newType( 4, $kindInt32,         "int32",          "int32",      "", null);
var $Int64         = $newType( 8, $kindInt64,         "int64",          "int64",      "", null);
var $Uint          = $newType( 4, $kindUint,          "uint",           "uint",       "", null);
var $Uint8         = $newType( 1, $kindUint8,         "uint8",          "uint8",      "", null);
var $Uint16        = $newType( 2, $kindUint16,        "uint16",         "uint16",     "", null);
var $Uint32        = $newType( 4, $kindUint32,        "uint32",         "uint32",     "", null);
var $Uint64        = $newType( 8, $kindUint64,        "uint64",         "uint64",     "", null);
var $Uintptr       = $newType( 4, $kindUintptr,       "uintptr",        "uintptr",    "", null);
var $Float32       = $newType( 4, $kindFloat32,       "float32",        "float32",    "", null);
var $Float64       = $newType( 8, $kindFloat64,       "float64",        "float64",    "", null);
var $Complex64     = $newType( 8, $kindComplex64,     "complex64",      "complex64",  "", null);
var $Complex128    = $newType(16, $kindComplex128,    "complex128",     "complex128", "", null);
var $String        = $newType( 8, $kindString,        "string",         "string",     "", null);
var $UnsafePointer = $newType( 4, $kindUnsafePointer, "unsafe.Pointer", "Pointer",    "", null);

var $nativeArray = function(elemKind) {
  switch (elemKind) {
  case $kindInt:
    return Int32Array;
  case $kindInt8:
    return Int8Array;
  case $kindInt16:
    return Int16Array;
  case $kindInt32:
    return Int32Array;
  case $kindUint:
    return Uint32Array;
  case $kindUint8:
    return Uint8Array;
  case $kindUint16:
    return Uint16Array;
  case $kindUint32:
    return Uint32Array;
  case $kindUintptr:
    return Uint32Array;
  case $kindFloat32:
    return Float32Array;
  case $kindFloat64:
    return Float64Array;
  default:
    return Array;
  }
};
var $toNativeArray = function(elemKind, array) {
  var nativeArray = $nativeArray(elemKind);
  if (nativeArray === Array) {
    return array;
  }
  return new nativeArray(array);
};
var $arrayTypes = {};
var $arrayType = function(elem, len) {
  var typeKey = elem.id + "$" + len;
  var typ = $arrayTypes[typeKey];
  if (typ === undefined) {
    typ = $newType(12, $kindArray, "[" + len + "]" + elem.string, "", "", null);
    $arrayTypes[typeKey] = typ;
    typ.init(elem, len);
  }
  return typ;
};

var $chanType = function(elem, sendOnly, recvOnly) {
  var string = (recvOnly ? "<-" : "") + "chan" + (sendOnly ? "<- " : " ") + elem.string;
  var field = sendOnly ? "SendChan" : (recvOnly ? "RecvChan" : "Chan");
  var typ = elem[field];
  if (typ === undefined) {
    typ = $newType(4, $kindChan, string, "", "", null);
    elem[field] = typ;
    typ.init(elem, sendOnly, recvOnly);
  }
  return typ;
};
var $Chan = function(elem, capacity) {
  if (capacity < 0 || capacity > 2147483647) {
    $throwRuntimeError("makechan: size out of range");
  }
  this.$elem = elem;
  this.$capacity = capacity;
  this.$buffer = [];
  this.$sendQueue = [];
  this.$recvQueue = [];
  this.$closed = false;
};
var $chanNil = new $Chan(null, 0);
$chanNil.$sendQueue = $chanNil.$recvQueue = { length: 0, push: function() {}, shift: function() { return undefined; }, indexOf: function() { return -1; } };

var $funcTypes = {};
var $funcType = function(params, results, variadic) {
  var typeKey = $mapArray(params, function(p) { return p.id; }).join(",") + "$" + $mapArray(results, function(r) { return r.id; }).join(",") + "$" + variadic;
  var typ = $funcTypes[typeKey];
  if (typ === undefined) {
    var paramTypes = $mapArray(params, function(p) { return p.string; });
    if (variadic) {
      paramTypes[paramTypes.length - 1] = "..." + paramTypes[paramTypes.length - 1].substr(2);
    }
    var string = "func(" + paramTypes.join(", ") + ")";
    if (results.length === 1) {
      string += " " + results[0].string;
    } else if (results.length > 1) {
      string += " (" + $mapArray(results, function(r) { return r.string; }).join(", ") + ")";
    }
    typ = $newType(4, $kindFunc, string, "", "", null);
    $funcTypes[typeKey] = typ;
    typ.init(params, results, variadic);
  }
  return typ;
};

var $interfaceTypes = {};
var $interfaceType = function(methods) {
  var typeKey = $mapArray(methods, function(m) { return m.pkg + "," + m.name + "," + m.typ.id; }).join("$");
  var typ = $interfaceTypes[typeKey];
  if (typ === undefined) {
    var string = "interface {}";
    if (methods.length !== 0) {
      string = "interface { " + $mapArray(methods, function(m) {
        return (m.pkg !== "" ? m.pkg + "." : "") + m.name + m.typ.string.substr(4);
      }).join("; ") + " }";
    }
    typ = $newType(8, $kindInterface, string, "", "", null);
    $interfaceTypes[typeKey] = typ;
    typ.init(methods);
  }
  return typ;
};
var $emptyInterface = $interfaceType([]);
var $ifaceNil = {};
var $error = $newType(8, $kindInterface, "error", "error", "", null);
$error.init([{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}]);

var $mapTypes = {};
var $mapType = function(key, elem) {
  var typeKey = key.id + "$" + elem.id;
  var typ = $mapTypes[typeKey];
  if (typ === undefined) {
    typ = $newType(4, $kindMap, "map[" + key.string + "]" + elem.string, "", "", null);
    $mapTypes[typeKey] = typ;
    typ.init(key, elem);
  }
  return typ;
};
var $makeMap = function(keyForFunc, entries) {
  var m = {};
  for (var i = 0; i < entries.length; i++) {
    var e = entries[i];
    m[keyForFunc(e.k)] = e;
  }
  return m;
};

var $ptrType = function(elem) {
  var typ = elem.ptr;
  if (typ === undefined) {
    typ = $newType(4, $kindPtr, "*" + elem.string, "", "", null);
    elem.ptr = typ;
    typ.init(elem);
  }
  return typ;
};

var $newDataPointer = function(data, constructor) {
  if (constructor.elem.kind === $kindStruct) {
    return data;
  }
  return new constructor(function() { return data; }, function(v) { data = v; });
};

var $indexPtr = function(array, index, constructor) {
  array.$ptr = array.$ptr || {};
  return array.$ptr[index] || (array.$ptr[index] = new constructor(function() { return array[index]; }, function(v) { array[index] = v; }));
};

var $sliceType = function(elem) {
  var typ = elem.slice;
  if (typ === undefined) {
    typ = $newType(12, $kindSlice, "[]" + elem.string, "", "", null);
    elem.slice = typ;
    typ.init(elem);
  }
  return typ;
};
var $makeSlice = function(typ, length, capacity) {
  capacity = capacity || length;
  if (length < 0 || length > 2147483647) {
    $throwRuntimeError("makeslice: len out of range");
  }
  if (capacity < 0 || capacity < length || capacity > 2147483647) {
    $throwRuntimeError("makeslice: cap out of range");
  }
  var array = new typ.nativeArray(capacity);
  if (typ.nativeArray === Array) {
    for (var i = 0; i < capacity; i++) {
      array[i] = typ.elem.zero();
    }
  }
  var slice = new typ(array);
  slice.$length = length;
  return slice;
};

var $structTypes = {};
var $structType = function(fields) {
  var typeKey = $mapArray(fields, function(f) { return f.name + "," + f.typ.id + "," + f.tag; }).join("$");
  var typ = $structTypes[typeKey];
  if (typ === undefined) {
    var string = "struct { " + $mapArray(fields, function(f) {
      return f.name + " " + f.typ.string + (f.tag !== "" ? (" \"" + f.tag.replace(/\\/g, "\\\\").replace(/"/g, "\\\"") + "\"") : "");
    }).join("; ") + " }";
    if (fields.length === 0) {
      string = "struct {}";
    }
    typ = $newType(0, $kindStruct, string, "", "", function() {
      this.$val = this;
      for (var i = 0; i < fields.length; i++) {
        var f = fields[i];
        var arg = arguments[i];
        this[f.prop] = arg !== undefined ? arg : f.typ.zero();
      }
    });
    $structTypes[typeKey] = typ;
    typ.init(fields);
  }
  return typ;
};

var $assertType = function(value, type, returnTuple) {
  var isInterface = (type.kind === $kindInterface), ok, missingMethod = "";
  if (value === $ifaceNil) {
    ok = false;
  } else if (!isInterface) {
    ok = value.constructor === type;
  } else {
    var valueTypeString = value.constructor.string;
    ok = type.implementedBy[valueTypeString];
    if (ok === undefined) {
      ok = true;
      var valueMethodSet = $methodSet(value.constructor);
      var interfaceMethods = type.methods;
      for (var i = 0; i < interfaceMethods.length; i++) {
        var tm = interfaceMethods[i];
        var found = false;
        for (var j = 0; j < valueMethodSet.length; j++) {
          var vm = valueMethodSet[j];
          if (vm.name === tm.name && vm.pkg === tm.pkg && vm.typ === tm.typ) {
            found = true;
            break;
          }
        }
        if (!found) {
          ok = false;
          type.missingMethodFor[valueTypeString] = tm.name;
          break;
        }
      }
      type.implementedBy[valueTypeString] = ok;
    }
    if (!ok) {
      missingMethod = type.missingMethodFor[valueTypeString];
    }
  }

  if (!ok) {
    if (returnTuple) {
      return [type.zero(), false];
    }
    $panic(new $packages["runtime"].TypeAssertionError.ptr("", (value === $ifaceNil ? "" : value.constructor.string), type.string, missingMethod));
  }

  if (!isInterface) {
    value = value.$val;
  }
  if (type === $jsObjectPtr) {
    value = value.object;
  }
  return returnTuple ? [value, true] : value;
};

var $stackDepthOffset = 0;
var $getStackDepth = function() {
  var err = new Error();
  if (err.stack === undefined) {
    return undefined;
  }
  return $stackDepthOffset + err.stack.split("\n").length;
};

var $panicStackDepth = null, $panicValue;
var $callDeferred = function(deferred, jsErr, fromPanic) {
  if (!fromPanic && deferred !== null && deferred.index >= $curGoroutine.deferStack.length) {
    throw jsErr;
  }
  if (jsErr !== null) {
    var newErr = null;
    try {
      $curGoroutine.deferStack.push(deferred);
      $panic(new $jsErrorPtr(jsErr));
    } catch (err) {
      newErr = err;
    }
    $curGoroutine.deferStack.pop();
    $callDeferred(deferred, newErr);
    return;
  }
  if ($curGoroutine.asleep) {
    return;
  }

  $stackDepthOffset--;
  var outerPanicStackDepth = $panicStackDepth;
  var outerPanicValue = $panicValue;

  var localPanicValue = $curGoroutine.panicStack.pop();
  if (localPanicValue !== undefined) {
    $panicStackDepth = $getStackDepth();
    $panicValue = localPanicValue;
  }

  try {
    while (true) {
      if (deferred === null) {
        deferred = $curGoroutine.deferStack[$curGoroutine.deferStack.length - 1];
        if (deferred === undefined) {
          /* The panic reached the top of the stack. Clear it and throw it as a JavaScript error. */
          $panicStackDepth = null;
          if (localPanicValue.Object instanceof Error) {
            throw localPanicValue.Object;
          }
          var msg;
          if (localPanicValue.constructor === $String) {
            msg = localPanicValue.$val;
          } else if (localPanicValue.Error !== undefined) {
            msg = localPanicValue.Error();
          } else if (localPanicValue.String !== undefined) {
            msg = localPanicValue.String();
          } else {
            msg = localPanicValue;
          }
          throw new Error(msg);
        }
      }
      var call = deferred.pop();
      if (call === undefined) {
        $curGoroutine.deferStack.pop();
        if (localPanicValue !== undefined) {
          deferred = null;
          continue;
        }
        return;
      }
      var r = call[0].apply(call[2], call[1]);
      if (r && r.$blk !== undefined) {
        deferred.push([r.$blk, [], r]);
        if (fromPanic) {
          throw null;
        }
        return;
      }

      if (localPanicValue !== undefined && $panicStackDepth === null) {
        throw null; /* error was recovered */
      }
    }
  } finally {
    if (localPanicValue !== undefined) {
      if ($panicStackDepth !== null) {
        $curGoroutine.panicStack.push(localPanicValue);
      }
      $panicStackDepth = outerPanicStackDepth;
      $panicValue = outerPanicValue;
    }
    $stackDepthOffset++;
  }
};

var $panic = function(value) {
  $curGoroutine.panicStack.push(value);
  $callDeferred(null, null, true);
};
var $recover = function() {
  if ($panicStackDepth === null || ($panicStackDepth !== undefined && $panicStackDepth !== $getStackDepth() - 2)) {
    return $ifaceNil;
  }
  $panicStackDepth = null;
  return $panicValue;
};
var $throw = function(err) { throw err; };

var $dummyGoroutine = { asleep: false, exit: false, deferStack: [], panicStack: [], canBlock: false };
var $curGoroutine = $dummyGoroutine, $totalGoroutines = 0, $awakeGoroutines = 0, $checkForDeadlock = true;
var $mainFinished = false;
var $go = function(fun, args, direct) {
  $totalGoroutines++;
  $awakeGoroutines++;
  var $goroutine = function() {
    try {
      $curGoroutine = $goroutine;
      var r = fun.apply(undefined, args);
      if (r && r.$blk !== undefined) {
        fun = function() { return r.$blk(); };
        args = [];
        return;
      }
      $goroutine.exit = true;
    } catch (err) {
      if (!$goroutine.exit) {
        throw err;
      }
    } finally {
      $curGoroutine = $dummyGoroutine;
      if ($goroutine.exit) { /* also set by runtime.Goexit() */
        $totalGoroutines--;
        $goroutine.asleep = true;
      }
      if ($goroutine.asleep) {
        $awakeGoroutines--;
        if (!$mainFinished && $awakeGoroutines === 0 && $checkForDeadlock) {
          console.error("fatal error: all goroutines are asleep - deadlock!");
          if ($global.process !== undefined) {
            $global.process.exit(2);
          }
        }
      }
    }
  };
  $goroutine.asleep = false;
  $goroutine.exit = false;
  $goroutine.deferStack = [];
  $goroutine.panicStack = [];
  $goroutine.canBlock = true;
  $schedule($goroutine, direct);
};

var $scheduled = [], $schedulerActive = false;
var $runScheduled = function() {
  try {
    var r;
    while ((r = $scheduled.shift()) !== undefined) {
      r();
    }
    $schedulerActive = false;
  } finally {
    if ($schedulerActive) {
      setTimeout($runScheduled, 0);
    }
  }
};
var $schedule = function(goroutine, direct) {
  if (goroutine.asleep) {
    goroutine.asleep = false;
    $awakeGoroutines++;
  }

  if (direct) {
    goroutine();
    return;
  }

  $scheduled.push(goroutine);
  if (!$schedulerActive) {
    $schedulerActive = true;
    setTimeout($runScheduled, 0);
  }
};

var $setTimeout = function(f, t) {
  $awakeGoroutines++;
  return setTimeout(function() {
    $awakeGoroutines--;
    f();
  }, t);
};

var $block = function() {
  if (!$curGoroutine.canBlock) {
    $throwRuntimeError("cannot block in JavaScript callback, fix by wrapping code in goroutine");
  }
  $curGoroutine.asleep = true;
};

var $send = function(chan, value) {
  if (chan.$closed) {
    $throwRuntimeError("send on closed channel");
  }
  var queuedRecv = chan.$recvQueue.shift();
  if (queuedRecv !== undefined) {
    queuedRecv([value, true]);
    return;
  }
  if (chan.$buffer.length < chan.$capacity) {
    chan.$buffer.push(value);
    return;
  }

  var thisGoroutine = $curGoroutine;
  chan.$sendQueue.push(function() {
    $schedule(thisGoroutine);
    return value;
  });
  $block();
  return {
    $blk: function() {
      if (chan.$closed) {
        $throwRuntimeError("send on closed channel");
      }
    }
  };
};
var $recv = function(chan) {
  var queuedSend = chan.$sendQueue.shift();
  if (queuedSend !== undefined) {
    chan.$buffer.push(queuedSend());
  }
  var bufferedValue = chan.$buffer.shift();
  if (bufferedValue !== undefined) {
    return [bufferedValue, true];
  }
  if (chan.$closed) {
    return [chan.$elem.zero(), false];
  }

  var thisGoroutine = $curGoroutine;
  var f = { $blk: function() { return this.value; } };
  var queueEntry = function(v) {
    f.value = v;
    $schedule(thisGoroutine);
  };
  chan.$recvQueue.push(queueEntry);
  $block();
  return f;
};
var $close = function(chan) {
  if (chan.$closed) {
    $throwRuntimeError("close of closed channel");
  }
  chan.$closed = true;
  while (true) {
    var queuedSend = chan.$sendQueue.shift();
    if (queuedSend === undefined) {
      break;
    }
    queuedSend(); /* will panic because of closed channel */
  }
  while (true) {
    var queuedRecv = chan.$recvQueue.shift();
    if (queuedRecv === undefined) {
      break;
    }
    queuedRecv([chan.$elem.zero(), false]);
  }
};
var $select = function(comms) {
  var ready = [];
  var selection = -1;
  for (var i = 0; i < comms.length; i++) {
    var comm = comms[i];
    var chan = comm[0];
    switch (comm.length) {
    case 0: /* default */
      selection = i;
      break;
    case 1: /* recv */
      if (chan.$sendQueue.length !== 0 || chan.$buffer.length !== 0 || chan.$closed) {
        ready.push(i);
      }
      break;
    case 2: /* send */
      if (chan.$closed) {
        $throwRuntimeError("send on closed channel");
      }
      if (chan.$recvQueue.length !== 0 || chan.$buffer.length < chan.$capacity) {
        ready.push(i);
      }
      break;
    }
  }

  if (ready.length !== 0) {
    selection = ready[Math.floor(Math.random() * ready.length)];
  }
  if (selection !== -1) {
    var comm = comms[selection];
    switch (comm.length) {
    case 0: /* default */
      return [selection];
    case 1: /* recv */
      return [selection, $recv(comm[0])];
    case 2: /* send */
      $send(comm[0], comm[1]);
      return [selection];
    }
  }

  var entries = [];
  var thisGoroutine = $curGoroutine;
  var f = { $blk: function() { return this.selection; } };
  var removeFromQueues = function() {
    for (var i = 0; i < entries.length; i++) {
      var entry = entries[i];
      var queue = entry[0];
      var index = queue.indexOf(entry[1]);
      if (index !== -1) {
        queue.splice(index, 1);
      }
    }
  };
  for (var i = 0; i < comms.length; i++) {
    (function(i) {
      var comm = comms[i];
      switch (comm.length) {
      case 1: /* recv */
        var queueEntry = function(value) {
          f.selection = [i, value];
          removeFromQueues();
          $schedule(thisGoroutine);
        };
        entries.push([comm[0].$recvQueue, queueEntry]);
        comm[0].$recvQueue.push(queueEntry);
        break;
      case 2: /* send */
        var queueEntry = function() {
          if (comm[0].$closed) {
            $throwRuntimeError("send on closed channel");
          }
          f.selection = [i];
          removeFromQueues();
          $schedule(thisGoroutine);
          return comm[1];
        };
        entries.push([comm[0].$sendQueue, queueEntry]);
        comm[0].$sendQueue.push(queueEntry);
        break;
      }
    })(i);
  }
  $block();
  return f;
};

var $jsObjectPtr, $jsErrorPtr;

var $needsExternalization = function(t) {
  switch (t.kind) {
    case $kindBool:
    case $kindInt:
    case $kindInt8:
    case $kindInt16:
    case $kindInt32:
    case $kindUint:
    case $kindUint8:
    case $kindUint16:
    case $kindUint32:
    case $kindUintptr:
    case $kindFloat32:
    case $kindFloat64:
      return false;
    default:
      return t !== $jsObjectPtr;
  }
};

var $externalize = function(v, t) {
  if (t === $jsObjectPtr) {
    return v;
  }
  switch (t.kind) {
  case $kindBool:
  case $kindInt:
  case $kindInt8:
  case $kindInt16:
  case $kindInt32:
  case $kindUint:
  case $kindUint8:
  case $kindUint16:
  case $kindUint32:
  case $kindUintptr:
  case $kindFloat32:
  case $kindFloat64:
    return v;
  case $kindInt64:
  case $kindUint64:
    return $flatten64(v);
  case $kindArray:
    if ($needsExternalization(t.elem)) {
      return $mapArray(v, function(e) { return $externalize(e, t.elem); });
    }
    return v;
  case $kindFunc:
    return $externalizeFunction(v, t, false);
  case $kindInterface:
    if (v === $ifaceNil) {
      return null;
    }
    if (v.constructor === $jsObjectPtr) {
      return v.$val.object;
    }
    return $externalize(v.$val, v.constructor);
  case $kindMap:
    var m = {};
    var keys = $keys(v);
    for (var i = 0; i < keys.length; i++) {
      var entry = v[keys[i]];
      m[$externalize(entry.k, t.key)] = $externalize(entry.v, t.elem);
    }
    return m;
  case $kindPtr:
    if (v === t.nil) {
      return null;
    }
    return $externalize(v.$get(), t.elem);
  case $kindSlice:
    if ($needsExternalization(t.elem)) {
      return $mapArray($sliceToArray(v), function(e) { return $externalize(e, t.elem); });
    }
    return $sliceToArray(v);
  case $kindString:
    if (v.search(/^[\x00-\x7F]*$/) !== -1) {
      return v;
    }
    var s = "", r;
    for (var i = 0; i < v.length; i += r[1]) {
      r = $decodeRune(v, i);
      var c = r[0];
      if (c > 0xFFFF) {
        var h = Math.floor((c - 0x10000) / 0x400) + 0xD800;
        var l = (c - 0x10000) % 0x400 + 0xDC00;
        s += String.fromCharCode(h, l);
        continue;
      }
      s += String.fromCharCode(c);
    }
    return s;
  case $kindStruct:
    var timePkg = $packages["time"];
    if (timePkg !== undefined && v.constructor === timePkg.Time.ptr) {
      var milli = $div64(v.UnixNano(), new $Int64(0, 1000000));
      return new Date($flatten64(milli));
    }

    var noJsObject = {};
    var searchJsObject = function(v, t) {
      if (t === $jsObjectPtr) {
        return v;
      }
      switch (t.kind) {
      case $kindPtr:
        if (v === t.nil) {
          return noJsObject;
        }
        return searchJsObject(v.$get(), t.elem);
      case $kindStruct:
        var f = t.fields[0];
        return searchJsObject(v[f.prop], f.typ);
      case $kindInterface:
        return searchJsObject(v.$val, v.constructor);
      default:
        return noJsObject;
      }
    };
    var o = searchJsObject(v, t);
    if (o !== noJsObject) {
      return o;
    }

    o = {};
    for (var i = 0; i < t.fields.length; i++) {
      var f = t.fields[i];
      if (f.pkg !== "") { /* not exported */
        continue;
      }
      o[f.name] = $externalize(v[f.prop], f.typ);
    }
    return o;
  }
  $throwRuntimeError("cannot externalize " + t.string);
};

var $externalizeFunction = function(v, t, passThis) {
  if (v === $throwNilPointerError) {
    return null;
  }
  if (v.$externalizeWrapper === undefined) {
    $checkForDeadlock = false;
    v.$externalizeWrapper = function() {
      var args = [];
      for (var i = 0; i < t.params.length; i++) {
        if (t.variadic && i === t.params.length - 1) {
          var vt = t.params[i].elem, varargs = [];
          for (var j = i; j < arguments.length; j++) {
            varargs.push($internalize(arguments[j], vt));
          }
          args.push(new (t.params[i])(varargs));
          break;
        }
        args.push($internalize(arguments[i], t.params[i]));
      }
      var canBlock = $curGoroutine.canBlock;
      $curGoroutine.canBlock = false;
      try {
        var result = v.apply(passThis ? this : undefined, args);
      } finally {
        $curGoroutine.canBlock = canBlock;
      }
      switch (t.results.length) {
      case 0:
        return;
      case 1:
        return $externalize(result, t.results[0]);
      default:
        for (var i = 0; i < t.results.length; i++) {
          result[i] = $externalize(result[i], t.results[i]);
        }
        return result;
      }
    };
  }
  return v.$externalizeWrapper;
};

var $internalize = function(v, t, recv) {
  if (t === $jsObjectPtr) {
    return v;
  }
  if (t === $jsObjectPtr.elem) {
    $throwRuntimeError("cannot internalize js.Object, use *js.Object instead");
  }
  if (v && v.__internal_object__ !== undefined) {
    return $assertType(v.__internal_object__, t, false);
  }
  var timePkg = $packages["time"];
  if (timePkg !== undefined && t === timePkg.Time) {
    if (!(v !== null && v !== undefined && v.constructor === Date)) {
      $throwRuntimeError("cannot internalize time.Time from " + typeof v + ", must be Date");
    }
    return timePkg.Unix(new $Int64(0, 0), new $Int64(0, v.getTime() * 1000000));
  }
  switch (t.kind) {
  case $kindBool:
    return !!v;
  case $kindInt:
    return parseInt(v);
  case $kindInt8:
    return parseInt(v) << 24 >> 24;
  case $kindInt16:
    return parseInt(v) << 16 >> 16;
  case $kindInt32:
    return parseInt(v) >> 0;
  case $kindUint:
    return parseInt(v);
  case $kindUint8:
    return parseInt(v) << 24 >>> 24;
  case $kindUint16:
    return parseInt(v) << 16 >>> 16;
  case $kindUint32:
  case $kindUintptr:
    return parseInt(v) >>> 0;
  case $kindInt64:
  case $kindUint64:
    return new t(0, v);
  case $kindFloat32:
  case $kindFloat64:
    return parseFloat(v);
  case $kindArray:
    if (v.length !== t.len) {
      $throwRuntimeError("got array with wrong size from JavaScript native");
    }
    return $mapArray(v, function(e) { return $internalize(e, t.elem); });
  case $kindFunc:
    return function() {
      var args = [];
      for (var i = 0; i < t.params.length; i++) {
        if (t.variadic && i === t.params.length - 1) {
          var vt = t.params[i].elem, varargs = arguments[i];
          for (var j = 0; j < varargs.$length; j++) {
            args.push($externalize(varargs.$array[varargs.$offset + j], vt));
          }
          break;
        }
        args.push($externalize(arguments[i], t.params[i]));
      }
      var result = v.apply(recv, args);
      switch (t.results.length) {
      case 0:
        return;
      case 1:
        return $internalize(result, t.results[0]);
      default:
        for (var i = 0; i < t.results.length; i++) {
          result[i] = $internalize(result[i], t.results[i]);
        }
        return result;
      }
    };
  case $kindInterface:
    if (t.methods.length !== 0) {
      $throwRuntimeError("cannot internalize " + t.string);
    }
    if (v === null) {
      return $ifaceNil;
    }
    if (v === undefined) {
      return new $jsObjectPtr(undefined);
    }
    switch (v.constructor) {
    case Int8Array:
      return new ($sliceType($Int8))(v);
    case Int16Array:
      return new ($sliceType($Int16))(v);
    case Int32Array:
      return new ($sliceType($Int))(v);
    case Uint8Array:
      return new ($sliceType($Uint8))(v);
    case Uint16Array:
      return new ($sliceType($Uint16))(v);
    case Uint32Array:
      return new ($sliceType($Uint))(v);
    case Float32Array:
      return new ($sliceType($Float32))(v);
    case Float64Array:
      return new ($sliceType($Float64))(v);
    case Array:
      return $internalize(v, $sliceType($emptyInterface));
    case Boolean:
      return new $Bool(!!v);
    case Date:
      if (timePkg === undefined) {
        /* time package is not present, internalize as &js.Object{Date} so it can be externalized into original Date. */
        return new $jsObjectPtr(v);
      }
      return new timePkg.Time($internalize(v, timePkg.Time));
    case Function:
      var funcType = $funcType([$sliceType($emptyInterface)], [$jsObjectPtr], true);
      return new funcType($internalize(v, funcType));
    case Number:
      return new $Float64(parseFloat(v));
    case String:
      return new $String($internalize(v, $String));
    default:
      if ($global.Node && v instanceof $global.Node) {
        return new $jsObjectPtr(v);
      }
      var mapType = $mapType($String, $emptyInterface);
      return new mapType($internalize(v, mapType));
    }
  case $kindMap:
    var m = {};
    var keys = $keys(v);
    for (var i = 0; i < keys.length; i++) {
      var k = $internalize(keys[i], t.key);
      m[t.key.keyFor(k)] = { k: k, v: $internalize(v[keys[i]], t.elem) };
    }
    return m;
  case $kindPtr:
    if (t.elem.kind === $kindStruct) {
      return $internalize(v, t.elem);
    }
  case $kindSlice:
    return new t($mapArray(v, function(e) { return $internalize(e, t.elem); }));
  case $kindString:
    v = String(v);
    if (v.search(/^[\x00-\x7F]*$/) !== -1) {
      return v;
    }
    var s = "";
    var i = 0;
    while (i < v.length) {
      var h = v.charCodeAt(i);
      if (0xD800 <= h && h <= 0xDBFF) {
        var l = v.charCodeAt(i + 1);
        var c = (h - 0xD800) * 0x400 + l - 0xDC00 + 0x10000;
        s += $encodeRune(c);
        i += 2;
        continue;
      }
      s += $encodeRune(h);
      i++;
    }
    return s;
  case $kindStruct:
    var noJsObject = {};
    var searchJsObject = function(t) {
      if (t === $jsObjectPtr) {
        return v;
      }
      if (t === $jsObjectPtr.elem) {
        $throwRuntimeError("cannot internalize js.Object, use *js.Object instead");
      }
      switch (t.kind) {
      case $kindPtr:
        return searchJsObject(t.elem);
      case $kindStruct:
        var f = t.fields[0];
        var o = searchJsObject(f.typ);
        if (o !== noJsObject) {
          var n = new t.ptr();
          n[f.prop] = o;
          return n;
        }
        return noJsObject;
      default:
        return noJsObject;
      }
    };
    var o = searchJsObject(t);
    if (o !== noJsObject) {
      return o;
    }
  }
  $throwRuntimeError("cannot internalize " + t.string);
};

$packages["github.com/gopherjs/gopherjs/js"] = (function() {
	var $pkg = {}, $init, Object, Error, sliceType, ptrType, ptrType$1, init;
	Object = $pkg.Object = $newType(0, $kindStruct, "js.Object", "Object", "github.com/gopherjs/gopherjs/js", function(object_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.object = null;
			return;
		}
		this.object = object_;
	});
	Error = $pkg.Error = $newType(0, $kindStruct, "js.Error", "Error", "github.com/gopherjs/gopherjs/js", function(Object_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Object = null;
			return;
		}
		this.Object = Object_;
	});
	sliceType = $sliceType($emptyInterface);
	ptrType = $ptrType(Object);
	ptrType$1 = $ptrType(Error);
	Object.ptr.prototype.Get = function(key) {
		var $ptr, key, o;
		o = this;
		return o.object[$externalize(key, $String)];
	};
	Object.prototype.Get = function(key) { return this.$val.Get(key); };
	Object.ptr.prototype.Set = function(key, value) {
		var $ptr, key, o, value;
		o = this;
		o.object[$externalize(key, $String)] = $externalize(value, $emptyInterface);
	};
	Object.prototype.Set = function(key, value) { return this.$val.Set(key, value); };
	Object.ptr.prototype.Delete = function(key) {
		var $ptr, key, o;
		o = this;
		delete o.object[$externalize(key, $String)];
	};
	Object.prototype.Delete = function(key) { return this.$val.Delete(key); };
	Object.ptr.prototype.Length = function() {
		var $ptr, o;
		o = this;
		return $parseInt(o.object.length);
	};
	Object.prototype.Length = function() { return this.$val.Length(); };
	Object.ptr.prototype.Index = function(i) {
		var $ptr, i, o;
		o = this;
		return o.object[i];
	};
	Object.prototype.Index = function(i) { return this.$val.Index(i); };
	Object.ptr.prototype.SetIndex = function(i, value) {
		var $ptr, i, o, value;
		o = this;
		o.object[i] = $externalize(value, $emptyInterface);
	};
	Object.prototype.SetIndex = function(i, value) { return this.$val.SetIndex(i, value); };
	Object.ptr.prototype.Call = function(name, args) {
		var $ptr, args, name, o, obj;
		o = this;
		return (obj = o.object, obj[$externalize(name, $String)].apply(obj, $externalize(args, sliceType)));
	};
	Object.prototype.Call = function(name, args) { return this.$val.Call(name, args); };
	Object.ptr.prototype.Invoke = function(args) {
		var $ptr, args, o;
		o = this;
		return o.object.apply(undefined, $externalize(args, sliceType));
	};
	Object.prototype.Invoke = function(args) { return this.$val.Invoke(args); };
	Object.ptr.prototype.New = function(args) {
		var $ptr, args, o;
		o = this;
		return new ($global.Function.prototype.bind.apply(o.object, [undefined].concat($externalize(args, sliceType))));
	};
	Object.prototype.New = function(args) { return this.$val.New(args); };
	Object.ptr.prototype.Bool = function() {
		var $ptr, o;
		o = this;
		return !!(o.object);
	};
	Object.prototype.Bool = function() { return this.$val.Bool(); };
	Object.ptr.prototype.String = function() {
		var $ptr, o;
		o = this;
		return $internalize(o.object, $String);
	};
	Object.prototype.String = function() { return this.$val.String(); };
	Object.ptr.prototype.Int = function() {
		var $ptr, o;
		o = this;
		return $parseInt(o.object) >> 0;
	};
	Object.prototype.Int = function() { return this.$val.Int(); };
	Object.ptr.prototype.Int64 = function() {
		var $ptr, o;
		o = this;
		return $internalize(o.object, $Int64);
	};
	Object.prototype.Int64 = function() { return this.$val.Int64(); };
	Object.ptr.prototype.Uint64 = function() {
		var $ptr, o;
		o = this;
		return $internalize(o.object, $Uint64);
	};
	Object.prototype.Uint64 = function() { return this.$val.Uint64(); };
	Object.ptr.prototype.Float = function() {
		var $ptr, o;
		o = this;
		return $parseFloat(o.object);
	};
	Object.prototype.Float = function() { return this.$val.Float(); };
	Object.ptr.prototype.Interface = function() {
		var $ptr, o;
		o = this;
		return $internalize(o.object, $emptyInterface);
	};
	Object.prototype.Interface = function() { return this.$val.Interface(); };
	Object.ptr.prototype.Unsafe = function() {
		var $ptr, o;
		o = this;
		return o.object;
	};
	Object.prototype.Unsafe = function() { return this.$val.Unsafe(); };
	Error.ptr.prototype.Error = function() {
		var $ptr, err;
		err = this;
		return "JavaScript error: " + $internalize(err.Object.message, $String);
	};
	Error.prototype.Error = function() { return this.$val.Error(); };
	Error.ptr.prototype.Stack = function() {
		var $ptr, err;
		err = this;
		return $internalize(err.Object.stack, $String);
	};
	Error.prototype.Stack = function() { return this.$val.Stack(); };
	init = function() {
		var $ptr, e;
		e = new Error.ptr(null);
	};
	ptrType.methods = [{prop: "Get", name: "Get", pkg: "", typ: $funcType([$String], [ptrType], false)}, {prop: "Set", name: "Set", pkg: "", typ: $funcType([$String, $emptyInterface], [], false)}, {prop: "Delete", name: "Delete", pkg: "", typ: $funcType([$String], [], false)}, {prop: "Length", name: "Length", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Index", name: "Index", pkg: "", typ: $funcType([$Int], [ptrType], false)}, {prop: "SetIndex", name: "SetIndex", pkg: "", typ: $funcType([$Int, $emptyInterface], [], false)}, {prop: "Call", name: "Call", pkg: "", typ: $funcType([$String, sliceType], [ptrType], true)}, {prop: "Invoke", name: "Invoke", pkg: "", typ: $funcType([sliceType], [ptrType], true)}, {prop: "New", name: "New", pkg: "", typ: $funcType([sliceType], [ptrType], true)}, {prop: "Bool", name: "Bool", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Int", name: "Int", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Int64", name: "Int64", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "Uint64", name: "Uint64", pkg: "", typ: $funcType([], [$Uint64], false)}, {prop: "Float", name: "Float", pkg: "", typ: $funcType([], [$Float64], false)}, {prop: "Interface", name: "Interface", pkg: "", typ: $funcType([], [$emptyInterface], false)}, {prop: "Unsafe", name: "Unsafe", pkg: "", typ: $funcType([], [$Uintptr], false)}];
	ptrType$1.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Stack", name: "Stack", pkg: "", typ: $funcType([], [$String], false)}];
	Object.init([{prop: "object", name: "object", pkg: "github.com/gopherjs/gopherjs/js", typ: ptrType, tag: ""}]);
	Error.init([{prop: "Object", name: "", pkg: "", typ: ptrType, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		init();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["runtime/internal/sys"] = (function() {
	var $pkg = {}, $init;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["runtime"] = (function() {
	var $pkg = {}, $init, js, sys, TypeAssertionError, errorString, ptrType$4, init, Goexit, SetFinalizer;
	js = $packages["github.com/gopherjs/gopherjs/js"];
	sys = $packages["runtime/internal/sys"];
	TypeAssertionError = $pkg.TypeAssertionError = $newType(0, $kindStruct, "runtime.TypeAssertionError", "TypeAssertionError", "runtime", function(interfaceString_, concreteString_, assertedString_, missingMethod_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.interfaceString = "";
			this.concreteString = "";
			this.assertedString = "";
			this.missingMethod = "";
			return;
		}
		this.interfaceString = interfaceString_;
		this.concreteString = concreteString_;
		this.assertedString = assertedString_;
		this.missingMethod = missingMethod_;
	});
	errorString = $pkg.errorString = $newType(8, $kindString, "runtime.errorString", "errorString", "runtime", null);
	ptrType$4 = $ptrType(TypeAssertionError);
	init = function() {
		var $ptr, e, jsPkg;
		jsPkg = $packages[$externalize("github.com/gopherjs/gopherjs/js", $String)];
		$jsObjectPtr = jsPkg.Object.ptr;
		$jsErrorPtr = jsPkg.Error.ptr;
		$throwRuntimeError = (function(msg) {
			var $ptr, msg;
			$panic(new errorString(msg));
		});
		e = $ifaceNil;
		e = new TypeAssertionError.ptr("", "", "", "");
	};
	Goexit = function() {
		var $ptr;
		$curGoroutine.exit = $externalize(true, $Bool);
		$throw(null);
	};
	$pkg.Goexit = Goexit;
	SetFinalizer = function(x, f) {
		var $ptr, f, x;
	};
	$pkg.SetFinalizer = SetFinalizer;
	TypeAssertionError.ptr.prototype.RuntimeError = function() {
		var $ptr;
	};
	TypeAssertionError.prototype.RuntimeError = function() { return this.$val.RuntimeError(); };
	TypeAssertionError.ptr.prototype.Error = function() {
		var $ptr, e, inter;
		e = this;
		inter = e.interfaceString;
		if (inter === "") {
			inter = "interface";
		}
		if (e.concreteString === "") {
			return "interface conversion: " + inter + " is nil, not " + e.assertedString;
		}
		if (e.missingMethod === "") {
			return "interface conversion: " + inter + " is " + e.concreteString + ", not " + e.assertedString;
		}
		return "interface conversion: " + e.concreteString + " is not " + e.assertedString + ": missing method " + e.missingMethod;
	};
	TypeAssertionError.prototype.Error = function() { return this.$val.Error(); };
	errorString.prototype.RuntimeError = function() {
		var $ptr, e;
		e = this.$val;
	};
	$ptrType(errorString).prototype.RuntimeError = function() { return new errorString(this.$get()).RuntimeError(); };
	errorString.prototype.Error = function() {
		var $ptr, e;
		e = this.$val;
		return "runtime error: " + e;
	};
	$ptrType(errorString).prototype.Error = function() { return new errorString(this.$get()).Error(); };
	ptrType$4.methods = [{prop: "RuntimeError", name: "RuntimeError", pkg: "", typ: $funcType([], [], false)}, {prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	errorString.methods = [{prop: "RuntimeError", name: "RuntimeError", pkg: "", typ: $funcType([], [], false)}, {prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	TypeAssertionError.init([{prop: "interfaceString", name: "interfaceString", pkg: "runtime", typ: $String, tag: ""}, {prop: "concreteString", name: "concreteString", pkg: "runtime", typ: $String, tag: ""}, {prop: "assertedString", name: "assertedString", pkg: "runtime", typ: $String, tag: ""}, {prop: "missingMethod", name: "missingMethod", pkg: "runtime", typ: $String, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = js.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sys.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		init();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["errors"] = (function() {
	var $pkg = {}, $init, errorString, ptrType, New;
	errorString = $pkg.errorString = $newType(0, $kindStruct, "errors.errorString", "errorString", "errors", function(s_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.s = "";
			return;
		}
		this.s = s_;
	});
	ptrType = $ptrType(errorString);
	New = function(text) {
		var $ptr, text;
		return new errorString.ptr(text);
	};
	$pkg.New = New;
	errorString.ptr.prototype.Error = function() {
		var $ptr, e;
		e = this;
		return e.s;
	};
	errorString.prototype.Error = function() { return this.$val.Error(); };
	ptrType.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	errorString.init([{prop: "s", name: "s", pkg: "errors", typ: $String, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["internal/race"] = (function() {
	var $pkg = {}, $init, Acquire, Release, ReleaseMerge, Disable, Enable, ReadRange, WriteRange;
	Acquire = function(addr) {
		var $ptr, addr;
	};
	$pkg.Acquire = Acquire;
	Release = function(addr) {
		var $ptr, addr;
	};
	$pkg.Release = Release;
	ReleaseMerge = function(addr) {
		var $ptr, addr;
	};
	$pkg.ReleaseMerge = ReleaseMerge;
	Disable = function() {
		var $ptr;
	};
	$pkg.Disable = Disable;
	Enable = function() {
		var $ptr;
	};
	$pkg.Enable = Enable;
	ReadRange = function(addr, len) {
		var $ptr, addr, len;
	};
	$pkg.ReadRange = ReadRange;
	WriteRange = function(addr, len) {
		var $ptr, addr, len;
	};
	$pkg.WriteRange = WriteRange;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["sync/atomic"] = (function() {
	var $pkg = {}, $init, js, CompareAndSwapInt32, AddInt32, LoadUint32, StoreUint32;
	js = $packages["github.com/gopherjs/gopherjs/js"];
	CompareAndSwapInt32 = function(addr, old, new$1) {
		var $ptr, addr, new$1, old;
		if (addr.$get() === old) {
			addr.$set(new$1);
			return true;
		}
		return false;
	};
	$pkg.CompareAndSwapInt32 = CompareAndSwapInt32;
	AddInt32 = function(addr, delta) {
		var $ptr, addr, delta, new$1;
		new$1 = addr.$get() + delta >> 0;
		addr.$set(new$1);
		return new$1;
	};
	$pkg.AddInt32 = AddInt32;
	LoadUint32 = function(addr) {
		var $ptr, addr;
		return addr.$get();
	};
	$pkg.LoadUint32 = LoadUint32;
	StoreUint32 = function(addr, val) {
		var $ptr, addr, val;
		addr.$set(val);
	};
	$pkg.StoreUint32 = StoreUint32;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = js.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["sync"] = (function() {
	var $pkg = {}, $init, race, runtime, atomic, Pool, Mutex, Locker, Once, poolLocal, syncSema, RWMutex, rlocker, ptrType, sliceType, ptrType$1, chanType, sliceType$1, ptrType$4, ptrType$6, sliceType$3, ptrType$7, ptrType$8, funcType, ptrType$12, funcType$1, ptrType$13, arrayType$1, semWaiters, allPools, runtime_Syncsemcheck, runtime_registerPoolCleanup, runtime_Semacquire, runtime_Semrelease, runtime_canSpin, poolCleanup, init, indexLocal, init$1, runtime_doSpin;
	race = $packages["internal/race"];
	runtime = $packages["runtime"];
	atomic = $packages["sync/atomic"];
	Pool = $pkg.Pool = $newType(0, $kindStruct, "sync.Pool", "Pool", "sync", function(local_, localSize_, store_, New_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.local = 0;
			this.localSize = 0;
			this.store = sliceType$3.nil;
			this.New = $throwNilPointerError;
			return;
		}
		this.local = local_;
		this.localSize = localSize_;
		this.store = store_;
		this.New = New_;
	});
	Mutex = $pkg.Mutex = $newType(0, $kindStruct, "sync.Mutex", "Mutex", "sync", function(state_, sema_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.state = 0;
			this.sema = 0;
			return;
		}
		this.state = state_;
		this.sema = sema_;
	});
	Locker = $pkg.Locker = $newType(8, $kindInterface, "sync.Locker", "Locker", "sync", null);
	Once = $pkg.Once = $newType(0, $kindStruct, "sync.Once", "Once", "sync", function(m_, done_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.m = new Mutex.ptr(0, 0);
			this.done = 0;
			return;
		}
		this.m = m_;
		this.done = done_;
	});
	poolLocal = $pkg.poolLocal = $newType(0, $kindStruct, "sync.poolLocal", "poolLocal", "sync", function(private$0_, shared_, Mutex_, pad_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.private$0 = $ifaceNil;
			this.shared = sliceType$3.nil;
			this.Mutex = new Mutex.ptr(0, 0);
			this.pad = arrayType$1.zero();
			return;
		}
		this.private$0 = private$0_;
		this.shared = shared_;
		this.Mutex = Mutex_;
		this.pad = pad_;
	});
	syncSema = $pkg.syncSema = $newType(0, $kindStruct, "sync.syncSema", "syncSema", "sync", function(lock_, head_, tail_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.lock = 0;
			this.head = 0;
			this.tail = 0;
			return;
		}
		this.lock = lock_;
		this.head = head_;
		this.tail = tail_;
	});
	RWMutex = $pkg.RWMutex = $newType(0, $kindStruct, "sync.RWMutex", "RWMutex", "sync", function(w_, writerSem_, readerSem_, readerCount_, readerWait_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.w = new Mutex.ptr(0, 0);
			this.writerSem = 0;
			this.readerSem = 0;
			this.readerCount = 0;
			this.readerWait = 0;
			return;
		}
		this.w = w_;
		this.writerSem = writerSem_;
		this.readerSem = readerSem_;
		this.readerCount = readerCount_;
		this.readerWait = readerWait_;
	});
	rlocker = $pkg.rlocker = $newType(0, $kindStruct, "sync.rlocker", "rlocker", "sync", function(w_, writerSem_, readerSem_, readerCount_, readerWait_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.w = new Mutex.ptr(0, 0);
			this.writerSem = 0;
			this.readerSem = 0;
			this.readerCount = 0;
			this.readerWait = 0;
			return;
		}
		this.w = w_;
		this.writerSem = writerSem_;
		this.readerSem = readerSem_;
		this.readerCount = readerCount_;
		this.readerWait = readerWait_;
	});
	ptrType = $ptrType(Pool);
	sliceType = $sliceType(ptrType);
	ptrType$1 = $ptrType($Uint32);
	chanType = $chanType($Bool, false, false);
	sliceType$1 = $sliceType(chanType);
	ptrType$4 = $ptrType($Int32);
	ptrType$6 = $ptrType(poolLocal);
	sliceType$3 = $sliceType($emptyInterface);
	ptrType$7 = $ptrType(rlocker);
	ptrType$8 = $ptrType(RWMutex);
	funcType = $funcType([], [$emptyInterface], false);
	ptrType$12 = $ptrType(Mutex);
	funcType$1 = $funcType([], [], false);
	ptrType$13 = $ptrType(Once);
	arrayType$1 = $arrayType($Uint8, 128);
	runtime_Syncsemcheck = function(size) {
		var $ptr, size;
	};
	Pool.ptr.prototype.Get = function() {
		var $ptr, _r, p, x, x$1, x$2, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; p = $f.p; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
		/* */ if (p.store.$length === 0) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (p.store.$length === 0) { */ case 1:
			/* */ if (!(p.New === $throwNilPointerError)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!(p.New === $throwNilPointerError)) { */ case 3:
				_r = p.New(); /* */ $s = 5; case 5: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				/* */ $s = 6; case 6:
				return _r;
			/* } */ case 4:
			return $ifaceNil;
		/* } */ case 2:
		x$2 = (x = p.store, x$1 = p.store.$length - 1 >> 0, ((x$1 < 0 || x$1 >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + x$1]));
		p.store = $subslice(p.store, 0, (p.store.$length - 1 >> 0));
		return x$2;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Pool.ptr.prototype.Get }; } $f.$ptr = $ptr; $f._r = _r; $f.p = p; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.$s = $s; $f.$r = $r; return $f;
	};
	Pool.prototype.Get = function() { return this.$val.Get(); };
	Pool.ptr.prototype.Put = function(x) {
		var $ptr, p, x;
		p = this;
		if ($interfaceIsEqual(x, $ifaceNil)) {
			return;
		}
		p.store = $append(p.store, x);
	};
	Pool.prototype.Put = function(x) { return this.$val.Put(x); };
	runtime_registerPoolCleanup = function(cleanup) {
		var $ptr, cleanup;
	};
	runtime_Semacquire = function(s) {
		var $ptr, _entry, _key, _r, ch, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _entry = $f._entry; _key = $f._key; _r = $f._r; ch = $f.ch; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ if (s.$get() === 0) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (s.$get() === 0) { */ case 1:
			ch = new $Chan($Bool, 0);
			_key = s; (semWaiters || $throwRuntimeError("assignment to entry in nil map"))[ptrType$1.keyFor(_key)] = { k: _key, v: $append((_entry = semWaiters[ptrType$1.keyFor(s)], _entry !== undefined ? _entry.v : sliceType$1.nil), ch) };
			_r = $recv(ch); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			_r[0];
		/* } */ case 2:
		s.$set(s.$get() - (1) >>> 0);
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: runtime_Semacquire }; } $f.$ptr = $ptr; $f._entry = _entry; $f._key = _key; $f._r = _r; $f.ch = ch; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	runtime_Semrelease = function(s) {
		var $ptr, _entry, _key, ch, s, w, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _entry = $f._entry; _key = $f._key; ch = $f.ch; s = $f.s; w = $f.w; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		s.$set(s.$get() + (1) >>> 0);
		w = (_entry = semWaiters[ptrType$1.keyFor(s)], _entry !== undefined ? _entry.v : sliceType$1.nil);
		if (w.$length === 0) {
			return;
		}
		ch = (0 >= w.$length ? $throwRuntimeError("index out of range") : w.$array[w.$offset + 0]);
		w = $subslice(w, 1);
		_key = s; (semWaiters || $throwRuntimeError("assignment to entry in nil map"))[ptrType$1.keyFor(_key)] = { k: _key, v: w };
		if (w.$length === 0) {
			delete semWaiters[ptrType$1.keyFor(s)];
		}
		$r = $send(ch, true); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: runtime_Semrelease }; } $f.$ptr = $ptr; $f._entry = _entry; $f._key = _key; $f.ch = ch; $f.s = s; $f.w = w; $f.$s = $s; $f.$r = $r; return $f;
	};
	runtime_canSpin = function(i) {
		var $ptr, i;
		return false;
	};
	Mutex.ptr.prototype.Lock = function() {
		var $ptr, awoke, iter, m, new$1, old, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; awoke = $f.awoke; iter = $f.iter; m = $f.m; new$1 = $f.new$1; old = $f.old; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		m = this;
		if (atomic.CompareAndSwapInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), 0, 1)) {
			if (false) {
				race.Acquire(m);
			}
			return;
		}
		awoke = false;
		iter = 0;
		/* while (true) { */ case 1:
			old = m.state;
			new$1 = old | 1;
			/* */ if (!(((old & 1) === 0))) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!(((old & 1) === 0))) { */ case 3:
				if (runtime_canSpin(iter)) {
					if (!awoke && ((old & 2) === 0) && !(((old >> 2 >> 0) === 0)) && atomic.CompareAndSwapInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), old, old | 2)) {
						awoke = true;
					}
					runtime_doSpin();
					iter = iter + (1) >> 0;
					/* continue; */ $s = 1; continue;
				}
				new$1 = old + 4 >> 0;
			/* } */ case 4:
			if (awoke) {
				if ((new$1 & 2) === 0) {
					$panic(new $String("sync: inconsistent mutex state"));
				}
				new$1 = (new$1 & ~(2)) >> 0;
			}
			/* */ if (atomic.CompareAndSwapInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), old, new$1)) { $s = 5; continue; }
			/* */ $s = 6; continue;
			/* if (atomic.CompareAndSwapInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), old, new$1)) { */ case 5:
				if ((old & 1) === 0) {
					/* break; */ $s = 2; continue;
				}
				$r = runtime_Semacquire((m.$ptr_sema || (m.$ptr_sema = new ptrType$1(function() { return this.$target.sema; }, function($v) { this.$target.sema = $v; }, m)))); /* */ $s = 7; case 7: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				awoke = true;
				iter = 0;
			/* } */ case 6:
		/* } */ $s = 1; continue; case 2:
		if (false) {
			race.Acquire(m);
		}
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Mutex.ptr.prototype.Lock }; } $f.$ptr = $ptr; $f.awoke = awoke; $f.iter = iter; $f.m = m; $f.new$1 = new$1; $f.old = old; $f.$s = $s; $f.$r = $r; return $f;
	};
	Mutex.prototype.Lock = function() { return this.$val.Lock(); };
	Mutex.ptr.prototype.Unlock = function() {
		var $ptr, m, new$1, old, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; m = $f.m; new$1 = $f.new$1; old = $f.old; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		m = this;
		if (false) {
			race.Release(m);
		}
		new$1 = atomic.AddInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), -1);
		if ((((new$1 + 1 >> 0)) & 1) === 0) {
			$panic(new $String("sync: unlock of unlocked mutex"));
		}
		old = new$1;
		/* while (true) { */ case 1:
			if (((old >> 2 >> 0) === 0) || !(((old & 3) === 0))) {
				return;
			}
			new$1 = ((old - 4 >> 0)) | 2;
			/* */ if (atomic.CompareAndSwapInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), old, new$1)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (atomic.CompareAndSwapInt32((m.$ptr_state || (m.$ptr_state = new ptrType$4(function() { return this.$target.state; }, function($v) { this.$target.state = $v; }, m))), old, new$1)) { */ case 3:
				$r = runtime_Semrelease((m.$ptr_sema || (m.$ptr_sema = new ptrType$1(function() { return this.$target.sema; }, function($v) { this.$target.sema = $v; }, m)))); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				return;
			/* } */ case 4:
			old = m.state;
		/* } */ $s = 1; continue; case 2:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Mutex.ptr.prototype.Unlock }; } $f.$ptr = $ptr; $f.m = m; $f.new$1 = new$1; $f.old = old; $f.$s = $s; $f.$r = $r; return $f;
	};
	Mutex.prototype.Unlock = function() { return this.$val.Unlock(); };
	Once.ptr.prototype.Do = function(f) {
		var $ptr, f, o, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; f = $f.f; o = $f.o; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		o = this;
		if (atomic.LoadUint32((o.$ptr_done || (o.$ptr_done = new ptrType$1(function() { return this.$target.done; }, function($v) { this.$target.done = $v; }, o)))) === 1) {
			return;
		}
		$r = o.m.Lock(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$deferred.push([$methodVal(o.m, "Unlock"), []]);
		/* */ if (o.done === 0) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (o.done === 0) { */ case 2:
			$deferred.push([atomic.StoreUint32, [(o.$ptr_done || (o.$ptr_done = new ptrType$1(function() { return this.$target.done; }, function($v) { this.$target.done = $v; }, o))), 1]]);
			$r = f(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* } */ case 3:
		/* */ $s = -1; case -1: } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: Once.ptr.prototype.Do }; } $f.$ptr = $ptr; $f.f = f; $f.o = o; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	Once.prototype.Do = function(f) { return this.$val.Do(f); };
	poolCleanup = function() {
		var $ptr, _i, _i$1, _ref, _ref$1, i, i$1, j, l, p, x;
		_ref = allPools;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			p = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			((i < 0 || i >= allPools.$length) ? $throwRuntimeError("index out of range") : allPools.$array[allPools.$offset + i] = ptrType.nil);
			i$1 = 0;
			while (true) {
				if (!(i$1 < (p.localSize >> 0))) { break; }
				l = indexLocal(p.local, i$1);
				l.private$0 = $ifaceNil;
				_ref$1 = l.shared;
				_i$1 = 0;
				while (true) {
					if (!(_i$1 < _ref$1.$length)) { break; }
					j = _i$1;
					(x = l.shared, ((j < 0 || j >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + j] = $ifaceNil));
					_i$1++;
				}
				l.shared = sliceType$3.nil;
				i$1 = i$1 + (1) >> 0;
			}
			p.local = 0;
			p.localSize = 0;
			_i++;
		}
		allPools = new sliceType([]);
	};
	init = function() {
		var $ptr;
		runtime_registerPoolCleanup(poolCleanup);
	};
	indexLocal = function(l, i) {
		var $ptr, i, l, x;
		return (x = l, (x.nilCheck, ((i < 0 || i >= x.length) ? $throwRuntimeError("index out of range") : x[i])));
	};
	init$1 = function() {
		var $ptr, s;
		s = new syncSema.ptr(0, 0, 0);
		runtime_Syncsemcheck(12);
	};
	runtime_doSpin = function() {
		$throwRuntimeError("native function not implemented: sync.runtime_doSpin");
	};
	RWMutex.ptr.prototype.RLock = function() {
		var $ptr, rw, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; rw = $f.rw; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		rw = this;
		if (false) {
			race.Disable();
		}
		/* */ if (atomic.AddInt32((rw.$ptr_readerCount || (rw.$ptr_readerCount = new ptrType$4(function() { return this.$target.readerCount; }, function($v) { this.$target.readerCount = $v; }, rw))), 1) < 0) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (atomic.AddInt32((rw.$ptr_readerCount || (rw.$ptr_readerCount = new ptrType$4(function() { return this.$target.readerCount; }, function($v) { this.$target.readerCount = $v; }, rw))), 1) < 0) { */ case 1:
			$r = runtime_Semacquire((rw.$ptr_readerSem || (rw.$ptr_readerSem = new ptrType$1(function() { return this.$target.readerSem; }, function($v) { this.$target.readerSem = $v; }, rw)))); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* } */ case 2:
		if (false) {
			race.Enable();
			race.Acquire((rw.$ptr_readerSem || (rw.$ptr_readerSem = new ptrType$1(function() { return this.$target.readerSem; }, function($v) { this.$target.readerSem = $v; }, rw))));
		}
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: RWMutex.ptr.prototype.RLock }; } $f.$ptr = $ptr; $f.rw = rw; $f.$s = $s; $f.$r = $r; return $f;
	};
	RWMutex.prototype.RLock = function() { return this.$val.RLock(); };
	RWMutex.ptr.prototype.RUnlock = function() {
		var $ptr, r, rw, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; r = $f.r; rw = $f.rw; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		rw = this;
		if (false) {
			race.ReleaseMerge((rw.$ptr_writerSem || (rw.$ptr_writerSem = new ptrType$1(function() { return this.$target.writerSem; }, function($v) { this.$target.writerSem = $v; }, rw))));
			race.Disable();
		}
		r = atomic.AddInt32((rw.$ptr_readerCount || (rw.$ptr_readerCount = new ptrType$4(function() { return this.$target.readerCount; }, function($v) { this.$target.readerCount = $v; }, rw))), -1);
		/* */ if (r < 0) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (r < 0) { */ case 1:
			if (((r + 1 >> 0) === 0) || ((r + 1 >> 0) === -1073741824)) {
				race.Enable();
				$panic(new $String("sync: RUnlock of unlocked RWMutex"));
			}
			/* */ if (atomic.AddInt32((rw.$ptr_readerWait || (rw.$ptr_readerWait = new ptrType$4(function() { return this.$target.readerWait; }, function($v) { this.$target.readerWait = $v; }, rw))), -1) === 0) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (atomic.AddInt32((rw.$ptr_readerWait || (rw.$ptr_readerWait = new ptrType$4(function() { return this.$target.readerWait; }, function($v) { this.$target.readerWait = $v; }, rw))), -1) === 0) { */ case 3:
				$r = runtime_Semrelease((rw.$ptr_writerSem || (rw.$ptr_writerSem = new ptrType$1(function() { return this.$target.writerSem; }, function($v) { this.$target.writerSem = $v; }, rw)))); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 4:
		/* } */ case 2:
		if (false) {
			race.Enable();
		}
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: RWMutex.ptr.prototype.RUnlock }; } $f.$ptr = $ptr; $f.r = r; $f.rw = rw; $f.$s = $s; $f.$r = $r; return $f;
	};
	RWMutex.prototype.RUnlock = function() { return this.$val.RUnlock(); };
	RWMutex.ptr.prototype.Lock = function() {
		var $ptr, r, rw, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; r = $f.r; rw = $f.rw; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		rw = this;
		if (false) {
			race.Disable();
		}
		$r = rw.w.Lock(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		r = atomic.AddInt32((rw.$ptr_readerCount || (rw.$ptr_readerCount = new ptrType$4(function() { return this.$target.readerCount; }, function($v) { this.$target.readerCount = $v; }, rw))), -1073741824) + 1073741824 >> 0;
		/* */ if (!((r === 0)) && !((atomic.AddInt32((rw.$ptr_readerWait || (rw.$ptr_readerWait = new ptrType$4(function() { return this.$target.readerWait; }, function($v) { this.$target.readerWait = $v; }, rw))), r) === 0))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!((r === 0)) && !((atomic.AddInt32((rw.$ptr_readerWait || (rw.$ptr_readerWait = new ptrType$4(function() { return this.$target.readerWait; }, function($v) { this.$target.readerWait = $v; }, rw))), r) === 0))) { */ case 2:
			$r = runtime_Semacquire((rw.$ptr_writerSem || (rw.$ptr_writerSem = new ptrType$1(function() { return this.$target.writerSem; }, function($v) { this.$target.writerSem = $v; }, rw)))); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* } */ case 3:
		if (false) {
			race.Enable();
			race.Acquire((rw.$ptr_readerSem || (rw.$ptr_readerSem = new ptrType$1(function() { return this.$target.readerSem; }, function($v) { this.$target.readerSem = $v; }, rw))));
			race.Acquire((rw.$ptr_writerSem || (rw.$ptr_writerSem = new ptrType$1(function() { return this.$target.writerSem; }, function($v) { this.$target.writerSem = $v; }, rw))));
		}
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: RWMutex.ptr.prototype.Lock }; } $f.$ptr = $ptr; $f.r = r; $f.rw = rw; $f.$s = $s; $f.$r = $r; return $f;
	};
	RWMutex.prototype.Lock = function() { return this.$val.Lock(); };
	RWMutex.ptr.prototype.Unlock = function() {
		var $ptr, i, r, rw, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; i = $f.i; r = $f.r; rw = $f.rw; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		rw = this;
		if (false) {
			race.Release((rw.$ptr_readerSem || (rw.$ptr_readerSem = new ptrType$1(function() { return this.$target.readerSem; }, function($v) { this.$target.readerSem = $v; }, rw))));
			race.Release((rw.$ptr_writerSem || (rw.$ptr_writerSem = new ptrType$1(function() { return this.$target.writerSem; }, function($v) { this.$target.writerSem = $v; }, rw))));
			race.Disable();
		}
		r = atomic.AddInt32((rw.$ptr_readerCount || (rw.$ptr_readerCount = new ptrType$4(function() { return this.$target.readerCount; }, function($v) { this.$target.readerCount = $v; }, rw))), 1073741824);
		if (r >= 1073741824) {
			race.Enable();
			$panic(new $String("sync: Unlock of unlocked RWMutex"));
		}
		i = 0;
		/* while (true) { */ case 1:
			/* if (!(i < (r >> 0))) { break; } */ if(!(i < (r >> 0))) { $s = 2; continue; }
			$r = runtime_Semrelease((rw.$ptr_readerSem || (rw.$ptr_readerSem = new ptrType$1(function() { return this.$target.readerSem; }, function($v) { this.$target.readerSem = $v; }, rw)))); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			i = i + (1) >> 0;
		/* } */ $s = 1; continue; case 2:
		$r = rw.w.Unlock(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		if (false) {
			race.Enable();
		}
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: RWMutex.ptr.prototype.Unlock }; } $f.$ptr = $ptr; $f.i = i; $f.r = r; $f.rw = rw; $f.$s = $s; $f.$r = $r; return $f;
	};
	RWMutex.prototype.Unlock = function() { return this.$val.Unlock(); };
	RWMutex.ptr.prototype.RLocker = function() {
		var $ptr, rw;
		rw = this;
		return $pointerOfStructConversion(rw, ptrType$7);
	};
	RWMutex.prototype.RLocker = function() { return this.$val.RLocker(); };
	rlocker.ptr.prototype.Lock = function() {
		var $ptr, r, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; r = $f.r; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		r = this;
		$r = $pointerOfStructConversion(r, ptrType$8).RLock(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: rlocker.ptr.prototype.Lock }; } $f.$ptr = $ptr; $f.r = r; $f.$s = $s; $f.$r = $r; return $f;
	};
	rlocker.prototype.Lock = function() { return this.$val.Lock(); };
	rlocker.ptr.prototype.Unlock = function() {
		var $ptr, r, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; r = $f.r; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		r = this;
		$r = $pointerOfStructConversion(r, ptrType$8).RUnlock(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: rlocker.ptr.prototype.Unlock }; } $f.$ptr = $ptr; $f.r = r; $f.$s = $s; $f.$r = $r; return $f;
	};
	rlocker.prototype.Unlock = function() { return this.$val.Unlock(); };
	ptrType.methods = [{prop: "Get", name: "Get", pkg: "", typ: $funcType([], [$emptyInterface], false)}, {prop: "Put", name: "Put", pkg: "", typ: $funcType([$emptyInterface], [], false)}, {prop: "getSlow", name: "getSlow", pkg: "sync", typ: $funcType([], [$emptyInterface], false)}, {prop: "pin", name: "pin", pkg: "sync", typ: $funcType([], [ptrType$6], false)}, {prop: "pinSlow", name: "pinSlow", pkg: "sync", typ: $funcType([], [ptrType$6], false)}];
	ptrType$12.methods = [{prop: "Lock", name: "Lock", pkg: "", typ: $funcType([], [], false)}, {prop: "Unlock", name: "Unlock", pkg: "", typ: $funcType([], [], false)}];
	ptrType$13.methods = [{prop: "Do", name: "Do", pkg: "", typ: $funcType([funcType$1], [], false)}];
	ptrType$8.methods = [{prop: "RLock", name: "RLock", pkg: "", typ: $funcType([], [], false)}, {prop: "RUnlock", name: "RUnlock", pkg: "", typ: $funcType([], [], false)}, {prop: "Lock", name: "Lock", pkg: "", typ: $funcType([], [], false)}, {prop: "Unlock", name: "Unlock", pkg: "", typ: $funcType([], [], false)}, {prop: "RLocker", name: "RLocker", pkg: "", typ: $funcType([], [Locker], false)}];
	ptrType$7.methods = [{prop: "Lock", name: "Lock", pkg: "", typ: $funcType([], [], false)}, {prop: "Unlock", name: "Unlock", pkg: "", typ: $funcType([], [], false)}];
	Pool.init([{prop: "local", name: "local", pkg: "sync", typ: $UnsafePointer, tag: ""}, {prop: "localSize", name: "localSize", pkg: "sync", typ: $Uintptr, tag: ""}, {prop: "store", name: "store", pkg: "sync", typ: sliceType$3, tag: ""}, {prop: "New", name: "New", pkg: "", typ: funcType, tag: ""}]);
	Mutex.init([{prop: "state", name: "state", pkg: "sync", typ: $Int32, tag: ""}, {prop: "sema", name: "sema", pkg: "sync", typ: $Uint32, tag: ""}]);
	Locker.init([{prop: "Lock", name: "Lock", pkg: "", typ: $funcType([], [], false)}, {prop: "Unlock", name: "Unlock", pkg: "", typ: $funcType([], [], false)}]);
	Once.init([{prop: "m", name: "m", pkg: "sync", typ: Mutex, tag: ""}, {prop: "done", name: "done", pkg: "sync", typ: $Uint32, tag: ""}]);
	poolLocal.init([{prop: "private$0", name: "private", pkg: "sync", typ: $emptyInterface, tag: ""}, {prop: "shared", name: "shared", pkg: "sync", typ: sliceType$3, tag: ""}, {prop: "Mutex", name: "", pkg: "", typ: Mutex, tag: ""}, {prop: "pad", name: "pad", pkg: "sync", typ: arrayType$1, tag: ""}]);
	syncSema.init([{prop: "lock", name: "lock", pkg: "sync", typ: $Uintptr, tag: ""}, {prop: "head", name: "head", pkg: "sync", typ: $UnsafePointer, tag: ""}, {prop: "tail", name: "tail", pkg: "sync", typ: $UnsafePointer, tag: ""}]);
	RWMutex.init([{prop: "w", name: "w", pkg: "sync", typ: Mutex, tag: ""}, {prop: "writerSem", name: "writerSem", pkg: "sync", typ: $Uint32, tag: ""}, {prop: "readerSem", name: "readerSem", pkg: "sync", typ: $Uint32, tag: ""}, {prop: "readerCount", name: "readerCount", pkg: "sync", typ: $Int32, tag: ""}, {prop: "readerWait", name: "readerWait", pkg: "sync", typ: $Int32, tag: ""}]);
	rlocker.init([{prop: "w", name: "w", pkg: "sync", typ: Mutex, tag: ""}, {prop: "writerSem", name: "writerSem", pkg: "sync", typ: $Uint32, tag: ""}, {prop: "readerSem", name: "readerSem", pkg: "sync", typ: $Uint32, tag: ""}, {prop: "readerCount", name: "readerCount", pkg: "sync", typ: $Int32, tag: ""}, {prop: "readerWait", name: "readerWait", pkg: "sync", typ: $Int32, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = race.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = runtime.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = atomic.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		allPools = sliceType.nil;
		semWaiters = {};
		init();
		init$1();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["io"] = (function() {
	var $pkg = {}, $init, errors, sync, Reader, Writer, RuneReader, sliceType, errWhence, errOffset;
	errors = $packages["errors"];
	sync = $packages["sync"];
	Reader = $pkg.Reader = $newType(8, $kindInterface, "io.Reader", "Reader", "io", null);
	Writer = $pkg.Writer = $newType(8, $kindInterface, "io.Writer", "Writer", "io", null);
	RuneReader = $pkg.RuneReader = $newType(8, $kindInterface, "io.RuneReader", "RuneReader", "io", null);
	sliceType = $sliceType($Uint8);
	Reader.init([{prop: "Read", name: "Read", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}]);
	Writer.init([{prop: "Write", name: "Write", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}]);
	RuneReader.init([{prop: "ReadRune", name: "ReadRune", pkg: "", typ: $funcType([], [$Int32, $Int, $error], false)}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sync.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.ErrShortWrite = errors.New("short write");
		$pkg.ErrShortBuffer = errors.New("short buffer");
		$pkg.EOF = errors.New("EOF");
		$pkg.ErrUnexpectedEOF = errors.New("unexpected EOF");
		$pkg.ErrNoProgress = errors.New("multiple Read calls return no data or error");
		errWhence = errors.New("Seek: invalid whence");
		errOffset = errors.New("Seek: invalid offset");
		$pkg.ErrClosedPipe = errors.New("io: read/write on closed pipe");
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["math"] = (function() {
	var $pkg = {}, $init, js, arrayType, arrayType$1, arrayType$2, structType, arrayType$3, math, zero, posInf, negInf, buf, pow10tab, IsInf, init, Float32bits, Float64bits, init$1;
	js = $packages["github.com/gopherjs/gopherjs/js"];
	arrayType = $arrayType($Uint32, 2);
	arrayType$1 = $arrayType($Float32, 2);
	arrayType$2 = $arrayType($Float64, 1);
	structType = $structType([{prop: "uint32array", name: "uint32array", pkg: "math", typ: arrayType, tag: ""}, {prop: "float32array", name: "float32array", pkg: "math", typ: arrayType$1, tag: ""}, {prop: "float64array", name: "float64array", pkg: "math", typ: arrayType$2, tag: ""}]);
	arrayType$3 = $arrayType($Float64, 70);
	IsInf = function(f, sign) {
		var $ptr, f, sign;
		if (f === posInf) {
			return sign >= 0;
		}
		if (f === negInf) {
			return sign <= 0;
		}
		return false;
	};
	$pkg.IsInf = IsInf;
	init = function() {
		var $ptr, ab;
		ab = new ($global.ArrayBuffer)(8);
		buf.uint32array = new ($global.Uint32Array)(ab);
		buf.float32array = new ($global.Float32Array)(ab);
		buf.float64array = new ($global.Float64Array)(ab);
	};
	Float32bits = function(f) {
		var $ptr, f;
		buf.float32array[0] = f;
		return buf.uint32array[0];
	};
	$pkg.Float32bits = Float32bits;
	Float64bits = function(f) {
		var $ptr, f, x, x$1;
		buf.float64array[0] = f;
		return (x = $shiftLeft64(new $Uint64(0, buf.uint32array[1]), 32), x$1 = new $Uint64(0, buf.uint32array[0]), new $Uint64(x.$high + x$1.$high, x.$low + x$1.$low));
	};
	$pkg.Float64bits = Float64bits;
	init$1 = function() {
		var $ptr, _q, i, m, x;
		pow10tab[0] = 1;
		pow10tab[1] = 10;
		i = 2;
		while (true) {
			if (!(i < 70)) { break; }
			m = (_q = i / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
			((i < 0 || i >= pow10tab.length) ? $throwRuntimeError("index out of range") : pow10tab[i] = ((m < 0 || m >= pow10tab.length) ? $throwRuntimeError("index out of range") : pow10tab[m]) * (x = i - m >> 0, ((x < 0 || x >= pow10tab.length) ? $throwRuntimeError("index out of range") : pow10tab[x])));
			i = i + (1) >> 0;
		}
	};
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = js.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		buf = new structType.ptr(arrayType.zero(), arrayType$1.zero(), arrayType$2.zero());
		pow10tab = arrayType$3.zero();
		math = $global.Math;
		zero = 0;
		posInf = 1 / zero;
		negInf = -1 / zero;
		init();
		init$1();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["unicode"] = (function() {
	var $pkg = {}, $init;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["unicode/utf8"] = (function() {
	var $pkg = {}, $init, acceptRange, first, acceptRanges, DecodeRune, DecodeRuneInString, DecodeLastRune, RuneLen, EncodeRune, RuneCount, RuneCountInString, RuneStart;
	acceptRange = $pkg.acceptRange = $newType(0, $kindStruct, "utf8.acceptRange", "acceptRange", "unicode/utf8", function(lo_, hi_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.lo = 0;
			this.hi = 0;
			return;
		}
		this.lo = lo_;
		this.hi = hi_;
	});
	DecodeRune = function(p) {
		var $ptr, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$12, _tmp$13, _tmp$14, _tmp$15, _tmp$16, _tmp$17, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, accept, b1, b2, b3, mask, n, p, p0, r, size, sz, x, x$1;
		r = 0;
		size = 0;
		n = p.$length;
		if (n < 1) {
			_tmp = 65533;
			_tmp$1 = 0;
			r = _tmp;
			size = _tmp$1;
			return [r, size];
		}
		p0 = (0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0]);
		x = ((p0 < 0 || p0 >= first.length) ? $throwRuntimeError("index out of range") : first[p0]);
		if (x >= 240) {
			mask = ((x >> 0) << 31 >> 0) >> 31 >> 0;
			_tmp$2 = ((((0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0]) >> 0) & ~mask) >> 0) | (65533 & mask);
			_tmp$3 = 1;
			r = _tmp$2;
			size = _tmp$3;
			return [r, size];
		}
		sz = (x & 7) >>> 0;
		accept = $clone((x$1 = x >>> 4 << 24 >>> 24, ((x$1 < 0 || x$1 >= acceptRanges.length) ? $throwRuntimeError("index out of range") : acceptRanges[x$1])), acceptRange);
		if (n < (sz >> 0)) {
			_tmp$4 = 65533;
			_tmp$5 = 1;
			r = _tmp$4;
			size = _tmp$5;
			return [r, size];
		}
		b1 = (1 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 1]);
		if (b1 < accept.lo || accept.hi < b1) {
			_tmp$6 = 65533;
			_tmp$7 = 1;
			r = _tmp$6;
			size = _tmp$7;
			return [r, size];
		}
		if (sz === 2) {
			_tmp$8 = ((((p0 & 31) >>> 0) >> 0) << 6 >> 0) | (((b1 & 63) >>> 0) >> 0);
			_tmp$9 = 2;
			r = _tmp$8;
			size = _tmp$9;
			return [r, size];
		}
		b2 = (2 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 2]);
		if (b2 < 128 || 191 < b2) {
			_tmp$10 = 65533;
			_tmp$11 = 1;
			r = _tmp$10;
			size = _tmp$11;
			return [r, size];
		}
		if (sz === 3) {
			_tmp$12 = (((((p0 & 15) >>> 0) >> 0) << 12 >> 0) | ((((b1 & 63) >>> 0) >> 0) << 6 >> 0)) | (((b2 & 63) >>> 0) >> 0);
			_tmp$13 = 3;
			r = _tmp$12;
			size = _tmp$13;
			return [r, size];
		}
		b3 = (3 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 3]);
		if (b3 < 128 || 191 < b3) {
			_tmp$14 = 65533;
			_tmp$15 = 1;
			r = _tmp$14;
			size = _tmp$15;
			return [r, size];
		}
		_tmp$16 = ((((((p0 & 7) >>> 0) >> 0) << 18 >> 0) | ((((b1 & 63) >>> 0) >> 0) << 12 >> 0)) | ((((b2 & 63) >>> 0) >> 0) << 6 >> 0)) | (((b3 & 63) >>> 0) >> 0);
		_tmp$17 = 4;
		r = _tmp$16;
		size = _tmp$17;
		return [r, size];
	};
	$pkg.DecodeRune = DecodeRune;
	DecodeRuneInString = function(s) {
		var $ptr, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$12, _tmp$13, _tmp$14, _tmp$15, _tmp$16, _tmp$17, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, accept, mask, n, r, s, s0, s1, s2, s3, size, sz, x, x$1;
		r = 0;
		size = 0;
		n = s.length;
		if (n < 1) {
			_tmp = 65533;
			_tmp$1 = 0;
			r = _tmp;
			size = _tmp$1;
			return [r, size];
		}
		s0 = s.charCodeAt(0);
		x = ((s0 < 0 || s0 >= first.length) ? $throwRuntimeError("index out of range") : first[s0]);
		if (x >= 240) {
			mask = ((x >> 0) << 31 >> 0) >> 31 >> 0;
			_tmp$2 = (((s.charCodeAt(0) >> 0) & ~mask) >> 0) | (65533 & mask);
			_tmp$3 = 1;
			r = _tmp$2;
			size = _tmp$3;
			return [r, size];
		}
		sz = (x & 7) >>> 0;
		accept = $clone((x$1 = x >>> 4 << 24 >>> 24, ((x$1 < 0 || x$1 >= acceptRanges.length) ? $throwRuntimeError("index out of range") : acceptRanges[x$1])), acceptRange);
		if (n < (sz >> 0)) {
			_tmp$4 = 65533;
			_tmp$5 = 1;
			r = _tmp$4;
			size = _tmp$5;
			return [r, size];
		}
		s1 = s.charCodeAt(1);
		if (s1 < accept.lo || accept.hi < s1) {
			_tmp$6 = 65533;
			_tmp$7 = 1;
			r = _tmp$6;
			size = _tmp$7;
			return [r, size];
		}
		if (sz === 2) {
			_tmp$8 = ((((s0 & 31) >>> 0) >> 0) << 6 >> 0) | (((s1 & 63) >>> 0) >> 0);
			_tmp$9 = 2;
			r = _tmp$8;
			size = _tmp$9;
			return [r, size];
		}
		s2 = s.charCodeAt(2);
		if (s2 < 128 || 191 < s2) {
			_tmp$10 = 65533;
			_tmp$11 = 1;
			r = _tmp$10;
			size = _tmp$11;
			return [r, size];
		}
		if (sz === 3) {
			_tmp$12 = (((((s0 & 15) >>> 0) >> 0) << 12 >> 0) | ((((s1 & 63) >>> 0) >> 0) << 6 >> 0)) | (((s2 & 63) >>> 0) >> 0);
			_tmp$13 = 3;
			r = _tmp$12;
			size = _tmp$13;
			return [r, size];
		}
		s3 = s.charCodeAt(3);
		if (s3 < 128 || 191 < s3) {
			_tmp$14 = 65533;
			_tmp$15 = 1;
			r = _tmp$14;
			size = _tmp$15;
			return [r, size];
		}
		_tmp$16 = ((((((s0 & 7) >>> 0) >> 0) << 18 >> 0) | ((((s1 & 63) >>> 0) >> 0) << 12 >> 0)) | ((((s2 & 63) >>> 0) >> 0) << 6 >> 0)) | (((s3 & 63) >>> 0) >> 0);
		_tmp$17 = 4;
		r = _tmp$16;
		size = _tmp$17;
		return [r, size];
	};
	$pkg.DecodeRuneInString = DecodeRuneInString;
	DecodeLastRune = function(p) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tuple, end, lim, p, r, size, start;
		r = 0;
		size = 0;
		end = p.$length;
		if (end === 0) {
			_tmp = 65533;
			_tmp$1 = 0;
			r = _tmp;
			size = _tmp$1;
			return [r, size];
		}
		start = end - 1 >> 0;
		r = (((start < 0 || start >= p.$length) ? $throwRuntimeError("index out of range") : p.$array[p.$offset + start]) >> 0);
		if (r < 128) {
			_tmp$2 = r;
			_tmp$3 = 1;
			r = _tmp$2;
			size = _tmp$3;
			return [r, size];
		}
		lim = end - 4 >> 0;
		if (lim < 0) {
			lim = 0;
		}
		start = start - (1) >> 0;
		while (true) {
			if (!(start >= lim)) { break; }
			if (RuneStart(((start < 0 || start >= p.$length) ? $throwRuntimeError("index out of range") : p.$array[p.$offset + start]))) {
				break;
			}
			start = start - (1) >> 0;
		}
		if (start < 0) {
			start = 0;
		}
		_tuple = DecodeRune($subslice(p, start, end));
		r = _tuple[0];
		size = _tuple[1];
		if (!(((start + size >> 0) === end))) {
			_tmp$4 = 65533;
			_tmp$5 = 1;
			r = _tmp$4;
			size = _tmp$5;
			return [r, size];
		}
		_tmp$6 = r;
		_tmp$7 = size;
		r = _tmp$6;
		size = _tmp$7;
		return [r, size];
	};
	$pkg.DecodeLastRune = DecodeLastRune;
	RuneLen = function(r) {
		var $ptr, r;
		if (r < 0) {
			return -1;
		} else if (r <= 127) {
			return 1;
		} else if (r <= 2047) {
			return 2;
		} else if (55296 <= r && r <= 57343) {
			return -1;
		} else if (r <= 65535) {
			return 3;
		} else if (r <= 1114111) {
			return 4;
		}
		return -1;
	};
	$pkg.RuneLen = RuneLen;
	EncodeRune = function(p, r) {
		var $ptr, i, p, r;
		i = (r >>> 0);
		if (i <= 127) {
			(0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0] = (r << 24 >>> 24));
			return 1;
		} else if (i <= 2047) {
			(0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0] = ((192 | ((r >> 6 >> 0) << 24 >>> 24)) >>> 0));
			(1 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 1] = ((128 | (((r << 24 >>> 24) & 63) >>> 0)) >>> 0));
			return 2;
		} else if ((i > 1114111) || (55296 <= i && i <= 57343)) {
			r = 65533;
			(0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0] = ((224 | ((r >> 12 >> 0) << 24 >>> 24)) >>> 0));
			(1 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 1] = ((128 | ((((r >> 6 >> 0) << 24 >>> 24) & 63) >>> 0)) >>> 0));
			(2 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 2] = ((128 | (((r << 24 >>> 24) & 63) >>> 0)) >>> 0));
			return 3;
		} else if (i <= 65535) {
			(0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0] = ((224 | ((r >> 12 >> 0) << 24 >>> 24)) >>> 0));
			(1 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 1] = ((128 | ((((r >> 6 >> 0) << 24 >>> 24) & 63) >>> 0)) >>> 0));
			(2 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 2] = ((128 | (((r << 24 >>> 24) & 63) >>> 0)) >>> 0));
			return 3;
		} else {
			(0 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 0] = ((240 | ((r >> 18 >> 0) << 24 >>> 24)) >>> 0));
			(1 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 1] = ((128 | ((((r >> 12 >> 0) << 24 >>> 24) & 63) >>> 0)) >>> 0));
			(2 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 2] = ((128 | ((((r >> 6 >> 0) << 24 >>> 24) & 63) >>> 0)) >>> 0));
			(3 >= p.$length ? $throwRuntimeError("index out of range") : p.$array[p.$offset + 3] = ((128 | (((r << 24 >>> 24) & 63) >>> 0)) >>> 0));
			return 4;
		}
	};
	$pkg.EncodeRune = EncodeRune;
	RuneCount = function(p) {
		var $ptr, accept, c, c$1, c$2, c$3, i, n, np, p, size, x, x$1, x$2, x$3, x$4;
		np = p.$length;
		n = 0;
		i = 0;
		while (true) {
			if (!(i < np)) { break; }
			n = n + (1) >> 0;
			c = ((i < 0 || i >= p.$length) ? $throwRuntimeError("index out of range") : p.$array[p.$offset + i]);
			if (c < 128) {
				i = i + (1) >> 0;
				continue;
			}
			x = ((c < 0 || c >= first.length) ? $throwRuntimeError("index out of range") : first[c]);
			if (x === 241) {
				i = i + (1) >> 0;
				continue;
			}
			size = (((x & 7) >>> 0) >> 0);
			if ((i + size >> 0) > np) {
				i = i + (1) >> 0;
				continue;
			}
			accept = $clone((x$1 = x >>> 4 << 24 >>> 24, ((x$1 < 0 || x$1 >= acceptRanges.length) ? $throwRuntimeError("index out of range") : acceptRanges[x$1])), acceptRange);
			c$1 = (x$2 = i + 1 >> 0, ((x$2 < 0 || x$2 >= p.$length) ? $throwRuntimeError("index out of range") : p.$array[p.$offset + x$2]));
			if (c$1 < accept.lo || accept.hi < c$1) {
				size = 1;
			} else if (size === 2) {
			} else {
				c$2 = (x$3 = i + 2 >> 0, ((x$3 < 0 || x$3 >= p.$length) ? $throwRuntimeError("index out of range") : p.$array[p.$offset + x$3]));
				if (c$2 < 128 || 191 < c$2) {
					size = 1;
				} else if (size === 3) {
				} else {
					c$3 = (x$4 = i + 3 >> 0, ((x$4 < 0 || x$4 >= p.$length) ? $throwRuntimeError("index out of range") : p.$array[p.$offset + x$4]));
					if (c$3 < 128 || 191 < c$3) {
						size = 1;
					}
				}
			}
			i = i + (size) >> 0;
		}
		return n;
	};
	$pkg.RuneCount = RuneCount;
	RuneCountInString = function(s) {
		var $ptr, accept, c, c$1, c$2, c$3, i, n, ns, s, size, x, x$1;
		n = 0;
		ns = s.length;
		i = 0;
		while (true) {
			if (!(i < ns)) { break; }
			c = s.charCodeAt(i);
			if (c < 128) {
				i = i + (1) >> 0;
				n = n + (1) >> 0;
				continue;
			}
			x = ((c < 0 || c >= first.length) ? $throwRuntimeError("index out of range") : first[c]);
			if (x === 241) {
				i = i + (1) >> 0;
				n = n + (1) >> 0;
				continue;
			}
			size = (((x & 7) >>> 0) >> 0);
			if ((i + size >> 0) > ns) {
				i = i + (1) >> 0;
				n = n + (1) >> 0;
				continue;
			}
			accept = $clone((x$1 = x >>> 4 << 24 >>> 24, ((x$1 < 0 || x$1 >= acceptRanges.length) ? $throwRuntimeError("index out of range") : acceptRanges[x$1])), acceptRange);
			c$1 = s.charCodeAt((i + 1 >> 0));
			if (c$1 < accept.lo || accept.hi < c$1) {
				size = 1;
			} else if (size === 2) {
			} else {
				c$2 = s.charCodeAt((i + 2 >> 0));
				if (c$2 < 128 || 191 < c$2) {
					size = 1;
				} else if (size === 3) {
				} else {
					c$3 = s.charCodeAt((i + 3 >> 0));
					if (c$3 < 128 || 191 < c$3) {
						size = 1;
					}
				}
			}
			i = i + (size) >> 0;
			n = n + (1) >> 0;
		}
		n = n;
		return n;
	};
	$pkg.RuneCountInString = RuneCountInString;
	RuneStart = function(b) {
		var $ptr, b;
		return !((((b & 192) >>> 0) === 128));
	};
	$pkg.RuneStart = RuneStart;
	acceptRange.init([{prop: "lo", name: "lo", pkg: "unicode/utf8", typ: $Uint8, tag: ""}, {prop: "hi", name: "hi", pkg: "unicode/utf8", typ: $Uint8, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		first = $toNativeArray($kindUint8, [240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 19, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 35, 3, 3, 52, 4, 4, 4, 68, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241, 241]);
		acceptRanges = $toNativeArray($kindStruct, [new acceptRange.ptr(128, 191), new acceptRange.ptr(160, 191), new acceptRange.ptr(128, 159), new acceptRange.ptr(144, 191), new acceptRange.ptr(128, 143)]);
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["bytes"] = (function() {
	var $pkg = {}, $init, errors, io, unicode, utf8, Buffer, readOp, ptrType, sliceType, arrayType, arrayType$1, IndexByte, makeSlice;
	errors = $packages["errors"];
	io = $packages["io"];
	unicode = $packages["unicode"];
	utf8 = $packages["unicode/utf8"];
	Buffer = $pkg.Buffer = $newType(0, $kindStruct, "bytes.Buffer", "Buffer", "bytes", function(buf_, off_, runeBytes_, bootstrap_, lastRead_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.buf = sliceType.nil;
			this.off = 0;
			this.runeBytes = arrayType.zero();
			this.bootstrap = arrayType$1.zero();
			this.lastRead = 0;
			return;
		}
		this.buf = buf_;
		this.off = off_;
		this.runeBytes = runeBytes_;
		this.bootstrap = bootstrap_;
		this.lastRead = lastRead_;
	});
	readOp = $pkg.readOp = $newType(4, $kindInt, "bytes.readOp", "readOp", "bytes", null);
	ptrType = $ptrType(Buffer);
	sliceType = $sliceType($Uint8);
	arrayType = $arrayType($Uint8, 4);
	arrayType$1 = $arrayType($Uint8, 64);
	IndexByte = function(s, c) {
		var $ptr, _i, _ref, b, c, i, s;
		_ref = s;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			b = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			if (b === c) {
				return i;
			}
			_i++;
		}
		return -1;
	};
	$pkg.IndexByte = IndexByte;
	Buffer.ptr.prototype.Bytes = function() {
		var $ptr, b;
		b = this;
		return $subslice(b.buf, b.off);
	};
	Buffer.prototype.Bytes = function() { return this.$val.Bytes(); };
	Buffer.ptr.prototype.String = function() {
		var $ptr, b;
		b = this;
		if (b === ptrType.nil) {
			return "<nil>";
		}
		return $bytesToString($subslice(b.buf, b.off));
	};
	Buffer.prototype.String = function() { return this.$val.String(); };
	Buffer.ptr.prototype.Len = function() {
		var $ptr, b;
		b = this;
		return b.buf.$length - b.off >> 0;
	};
	Buffer.prototype.Len = function() { return this.$val.Len(); };
	Buffer.ptr.prototype.Cap = function() {
		var $ptr, b;
		b = this;
		return b.buf.$capacity;
	};
	Buffer.prototype.Cap = function() { return this.$val.Cap(); };
	Buffer.ptr.prototype.Truncate = function(n) {
		var $ptr, b, n;
		b = this;
		b.lastRead = 0;
		if (n < 0 || n > b.Len()) {
			$panic(new $String("bytes.Buffer: truncation out of range"));
		} else if ((n === 0)) {
			b.off = 0;
		}
		b.buf = $subslice(b.buf, 0, (b.off + n >> 0));
	};
	Buffer.prototype.Truncate = function(n) { return this.$val.Truncate(n); };
	Buffer.ptr.prototype.Reset = function() {
		var $ptr, b;
		b = this;
		b.Truncate(0);
	};
	Buffer.prototype.Reset = function() { return this.$val.Reset(); };
	Buffer.ptr.prototype.grow = function(n) {
		var $ptr, _q, b, buf, m, n;
		b = this;
		m = b.Len();
		if ((m === 0) && !((b.off === 0))) {
			b.Truncate(0);
		}
		if ((b.buf.$length + n >> 0) > b.buf.$capacity) {
			buf = sliceType.nil;
			if (b.buf === sliceType.nil && n <= 64) {
				buf = $subslice(new sliceType(b.bootstrap), 0);
			} else if ((m + n >> 0) <= (_q = b.buf.$capacity / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"))) {
				$copySlice(b.buf, $subslice(b.buf, b.off));
				buf = $subslice(b.buf, 0, m);
			} else {
				buf = makeSlice(($imul(2, b.buf.$capacity)) + n >> 0);
				$copySlice(buf, $subslice(b.buf, b.off));
			}
			b.buf = buf;
			b.off = 0;
		}
		b.buf = $subslice(b.buf, 0, ((b.off + m >> 0) + n >> 0));
		return b.off + m >> 0;
	};
	Buffer.prototype.grow = function(n) { return this.$val.grow(n); };
	Buffer.ptr.prototype.Grow = function(n) {
		var $ptr, b, m, n;
		b = this;
		if (n < 0) {
			$panic(new $String("bytes.Buffer.Grow: negative count"));
		}
		m = b.grow(n);
		b.buf = $subslice(b.buf, 0, m);
	};
	Buffer.prototype.Grow = function(n) { return this.$val.Grow(n); };
	Buffer.ptr.prototype.Write = function(p) {
		var $ptr, _tmp, _tmp$1, b, err, m, n, p;
		n = 0;
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		m = b.grow(p.$length);
		_tmp = $copySlice($subslice(b.buf, m), p);
		_tmp$1 = $ifaceNil;
		n = _tmp;
		err = _tmp$1;
		return [n, err];
	};
	Buffer.prototype.Write = function(p) { return this.$val.Write(p); };
	Buffer.ptr.prototype.WriteString = function(s) {
		var $ptr, _tmp, _tmp$1, b, err, m, n, s;
		n = 0;
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		m = b.grow(s.length);
		_tmp = $copyString($subslice(b.buf, m), s);
		_tmp$1 = $ifaceNil;
		n = _tmp;
		err = _tmp$1;
		return [n, err];
	};
	Buffer.prototype.WriteString = function(s) { return this.$val.WriteString(s); };
	Buffer.ptr.prototype.ReadFrom = function(r) {
		var $ptr, _r, _tmp, _tmp$1, _tmp$2, _tmp$3, _tuple, b, e, err, free, m, n, newBuf, r, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tuple = $f._tuple; b = $f.b; e = $f.e; err = $f.err; free = $f.free; m = $f.m; n = $f.n; newBuf = $f.newBuf; r = $f.r; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		n = new $Int64(0, 0);
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		if (b.off >= b.buf.$length) {
			b.Truncate(0);
		}
		/* while (true) { */ case 1:
			free = b.buf.$capacity - b.buf.$length >> 0;
			if (free < 512) {
				newBuf = b.buf;
				if ((b.off + free >> 0) < 512) {
					newBuf = makeSlice(($imul(2, b.buf.$capacity)) + 512 >> 0);
				}
				$copySlice(newBuf, $subslice(b.buf, b.off));
				b.buf = $subslice(newBuf, 0, (b.buf.$length - b.off >> 0));
				b.off = 0;
			}
			_r = r.Read($subslice(b.buf, b.buf.$length, b.buf.$capacity)); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			_tuple = _r;
			m = _tuple[0];
			e = _tuple[1];
			b.buf = $subslice(b.buf, 0, (b.buf.$length + m >> 0));
			n = (x = new $Int64(0, m), new $Int64(n.$high + x.$high, n.$low + x.$low));
			if ($interfaceIsEqual(e, io.EOF)) {
				/* break; */ $s = 2; continue;
			}
			if (!($interfaceIsEqual(e, $ifaceNil))) {
				_tmp = n;
				_tmp$1 = e;
				n = _tmp;
				err = _tmp$1;
				return [n, err];
			}
		/* } */ $s = 1; continue; case 2:
		_tmp$2 = n;
		_tmp$3 = $ifaceNil;
		n = _tmp$2;
		err = _tmp$3;
		return [n, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Buffer.ptr.prototype.ReadFrom }; } $f.$ptr = $ptr; $f._r = _r; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tuple = _tuple; $f.b = b; $f.e = e; $f.err = err; $f.free = free; $f.m = m; $f.n = n; $f.newBuf = newBuf; $f.r = r; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Buffer.prototype.ReadFrom = function(r) { return this.$val.ReadFrom(r); };
	makeSlice = function(n) {
		var $ptr, n, $deferred;
		/* */ var $err = null; try { $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		$deferred.push([(function() {
			var $ptr;
			if (!($interfaceIsEqual($recover(), $ifaceNil))) {
				$panic($pkg.ErrTooLarge);
			}
		}), []]);
		return $makeSlice(sliceType, n);
		/* */ } catch(err) { $err = err; return sliceType.nil; } finally { $callDeferred($deferred, $err); }
	};
	Buffer.ptr.prototype.WriteTo = function(w) {
		var $ptr, _r, _tmp, _tmp$1, _tmp$2, _tmp$3, _tuple, b, e, err, m, n, nBytes, w, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tuple = $f._tuple; b = $f.b; e = $f.e; err = $f.err; m = $f.m; n = $f.n; nBytes = $f.nBytes; w = $f.w; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		n = new $Int64(0, 0);
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		/* */ if (b.off < b.buf.$length) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (b.off < b.buf.$length) { */ case 1:
			nBytes = b.Len();
			_r = w.Write($subslice(b.buf, b.off)); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			_tuple = _r;
			m = _tuple[0];
			e = _tuple[1];
			if (m > nBytes) {
				$panic(new $String("bytes.Buffer.WriteTo: invalid Write count"));
			}
			b.off = b.off + (m) >> 0;
			n = new $Int64(0, m);
			if (!($interfaceIsEqual(e, $ifaceNil))) {
				_tmp = n;
				_tmp$1 = e;
				n = _tmp;
				err = _tmp$1;
				return [n, err];
			}
			if (!((m === nBytes))) {
				_tmp$2 = n;
				_tmp$3 = io.ErrShortWrite;
				n = _tmp$2;
				err = _tmp$3;
				return [n, err];
			}
		/* } */ case 2:
		b.Truncate(0);
		return [n, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Buffer.ptr.prototype.WriteTo }; } $f.$ptr = $ptr; $f._r = _r; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tuple = _tuple; $f.b = b; $f.e = e; $f.err = err; $f.m = m; $f.n = n; $f.nBytes = nBytes; $f.w = w; $f.$s = $s; $f.$r = $r; return $f;
	};
	Buffer.prototype.WriteTo = function(w) { return this.$val.WriteTo(w); };
	Buffer.ptr.prototype.WriteByte = function(c) {
		var $ptr, b, c, m, x;
		b = this;
		b.lastRead = 0;
		m = b.grow(1);
		(x = b.buf, ((m < 0 || m >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + m] = c));
		return $ifaceNil;
	};
	Buffer.prototype.WriteByte = function(c) { return this.$val.WriteByte(c); };
	Buffer.ptr.prototype.WriteRune = function(r) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, b, err, n, r;
		n = 0;
		err = $ifaceNil;
		b = this;
		if (r < 128) {
			b.WriteByte((r << 24 >>> 24));
			_tmp = 1;
			_tmp$1 = $ifaceNil;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		n = utf8.EncodeRune($subslice(new sliceType(b.runeBytes), 0), r);
		b.Write($subslice(new sliceType(b.runeBytes), 0, n));
		_tmp$2 = n;
		_tmp$3 = $ifaceNil;
		n = _tmp$2;
		err = _tmp$3;
		return [n, err];
	};
	Buffer.prototype.WriteRune = function(r) { return this.$val.WriteRune(r); };
	Buffer.ptr.prototype.Read = function(p) {
		var $ptr, _tmp, _tmp$1, b, err, n, p;
		n = 0;
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		if (b.off >= b.buf.$length) {
			b.Truncate(0);
			if (p.$length === 0) {
				return [n, err];
			}
			_tmp = 0;
			_tmp$1 = io.EOF;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		n = $copySlice(p, $subslice(b.buf, b.off));
		b.off = b.off + (n) >> 0;
		if (n > 0) {
			b.lastRead = 2;
		}
		return [n, err];
	};
	Buffer.prototype.Read = function(p) { return this.$val.Read(p); };
	Buffer.ptr.prototype.Next = function(n) {
		var $ptr, b, data, m, n;
		b = this;
		b.lastRead = 0;
		m = b.Len();
		if (n > m) {
			n = m;
		}
		data = $subslice(b.buf, b.off, (b.off + n >> 0));
		b.off = b.off + (n) >> 0;
		if (n > 0) {
			b.lastRead = 2;
		}
		return data;
	};
	Buffer.prototype.Next = function(n) { return this.$val.Next(n); };
	Buffer.ptr.prototype.ReadByte = function() {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, b, c, err, x, x$1;
		c = 0;
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		if (b.off >= b.buf.$length) {
			b.Truncate(0);
			_tmp = 0;
			_tmp$1 = io.EOF;
			c = _tmp;
			err = _tmp$1;
			return [c, err];
		}
		c = (x = b.buf, x$1 = b.off, ((x$1 < 0 || x$1 >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + x$1]));
		b.off = b.off + (1) >> 0;
		b.lastRead = 2;
		_tmp$2 = c;
		_tmp$3 = $ifaceNil;
		c = _tmp$2;
		err = _tmp$3;
		return [c, err];
	};
	Buffer.prototype.ReadByte = function() { return this.$val.ReadByte(); };
	Buffer.ptr.prototype.ReadRune = function() {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tuple, b, c, err, n, r, size, x, x$1;
		r = 0;
		size = 0;
		err = $ifaceNil;
		b = this;
		b.lastRead = 0;
		if (b.off >= b.buf.$length) {
			b.Truncate(0);
			_tmp = 0;
			_tmp$1 = 0;
			_tmp$2 = io.EOF;
			r = _tmp;
			size = _tmp$1;
			err = _tmp$2;
			return [r, size, err];
		}
		b.lastRead = 1;
		c = (x = b.buf, x$1 = b.off, ((x$1 < 0 || x$1 >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + x$1]));
		if (c < 128) {
			b.off = b.off + (1) >> 0;
			_tmp$3 = (c >> 0);
			_tmp$4 = 1;
			_tmp$5 = $ifaceNil;
			r = _tmp$3;
			size = _tmp$4;
			err = _tmp$5;
			return [r, size, err];
		}
		_tuple = utf8.DecodeRune($subslice(b.buf, b.off));
		r = _tuple[0];
		n = _tuple[1];
		b.off = b.off + (n) >> 0;
		_tmp$6 = r;
		_tmp$7 = n;
		_tmp$8 = $ifaceNil;
		r = _tmp$6;
		size = _tmp$7;
		err = _tmp$8;
		return [r, size, err];
	};
	Buffer.prototype.ReadRune = function() { return this.$val.ReadRune(); };
	Buffer.ptr.prototype.UnreadRune = function() {
		var $ptr, _tuple, b, n;
		b = this;
		if (!((b.lastRead === 1))) {
			return errors.New("bytes.Buffer: UnreadRune: previous operation was not ReadRune");
		}
		b.lastRead = 0;
		if (b.off > 0) {
			_tuple = utf8.DecodeLastRune($subslice(b.buf, 0, b.off));
			n = _tuple[1];
			b.off = b.off - (n) >> 0;
		}
		return $ifaceNil;
	};
	Buffer.prototype.UnreadRune = function() { return this.$val.UnreadRune(); };
	Buffer.ptr.prototype.UnreadByte = function() {
		var $ptr, b;
		b = this;
		if (!((b.lastRead === 1)) && !((b.lastRead === 2))) {
			return errors.New("bytes.Buffer: UnreadByte: previous operation was not a read");
		}
		b.lastRead = 0;
		if (b.off > 0) {
			b.off = b.off - (1) >> 0;
		}
		return $ifaceNil;
	};
	Buffer.prototype.UnreadByte = function() { return this.$val.UnreadByte(); };
	Buffer.ptr.prototype.ReadBytes = function(delim) {
		var $ptr, _tuple, b, delim, err, line, slice;
		line = sliceType.nil;
		err = $ifaceNil;
		b = this;
		_tuple = b.readSlice(delim);
		slice = _tuple[0];
		err = _tuple[1];
		line = $appendSlice(line, slice);
		return [line, err];
	};
	Buffer.prototype.ReadBytes = function(delim) { return this.$val.ReadBytes(delim); };
	Buffer.ptr.prototype.readSlice = function(delim) {
		var $ptr, _tmp, _tmp$1, b, delim, end, err, i, line;
		line = sliceType.nil;
		err = $ifaceNil;
		b = this;
		i = IndexByte($subslice(b.buf, b.off), delim);
		end = (b.off + i >> 0) + 1 >> 0;
		if (i < 0) {
			end = b.buf.$length;
			err = io.EOF;
		}
		line = $subslice(b.buf, b.off, end);
		b.off = end;
		b.lastRead = 2;
		_tmp = line;
		_tmp$1 = err;
		line = _tmp;
		err = _tmp$1;
		return [line, err];
	};
	Buffer.prototype.readSlice = function(delim) { return this.$val.readSlice(delim); };
	Buffer.ptr.prototype.ReadString = function(delim) {
		var $ptr, _tmp, _tmp$1, _tuple, b, delim, err, line, slice;
		line = "";
		err = $ifaceNil;
		b = this;
		_tuple = b.readSlice(delim);
		slice = _tuple[0];
		err = _tuple[1];
		_tmp = $bytesToString(slice);
		_tmp$1 = err;
		line = _tmp;
		err = _tmp$1;
		return [line, err];
	};
	Buffer.prototype.ReadString = function(delim) { return this.$val.ReadString(delim); };
	ptrType.methods = [{prop: "Bytes", name: "Bytes", pkg: "", typ: $funcType([], [sliceType], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Len", name: "Len", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Cap", name: "Cap", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Truncate", name: "Truncate", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "Reset", name: "Reset", pkg: "", typ: $funcType([], [], false)}, {prop: "grow", name: "grow", pkg: "bytes", typ: $funcType([$Int], [$Int], false)}, {prop: "Grow", name: "Grow", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "Write", name: "Write", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}, {prop: "WriteString", name: "WriteString", pkg: "", typ: $funcType([$String], [$Int, $error], false)}, {prop: "ReadFrom", name: "ReadFrom", pkg: "", typ: $funcType([io.Reader], [$Int64, $error], false)}, {prop: "WriteTo", name: "WriteTo", pkg: "", typ: $funcType([io.Writer], [$Int64, $error], false)}, {prop: "WriteByte", name: "WriteByte", pkg: "", typ: $funcType([$Uint8], [$error], false)}, {prop: "WriteRune", name: "WriteRune", pkg: "", typ: $funcType([$Int32], [$Int, $error], false)}, {prop: "Read", name: "Read", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}, {prop: "Next", name: "Next", pkg: "", typ: $funcType([$Int], [sliceType], false)}, {prop: "ReadByte", name: "ReadByte", pkg: "", typ: $funcType([], [$Uint8, $error], false)}, {prop: "ReadRune", name: "ReadRune", pkg: "", typ: $funcType([], [$Int32, $Int, $error], false)}, {prop: "UnreadRune", name: "UnreadRune", pkg: "", typ: $funcType([], [$error], false)}, {prop: "UnreadByte", name: "UnreadByte", pkg: "", typ: $funcType([], [$error], false)}, {prop: "ReadBytes", name: "ReadBytes", pkg: "", typ: $funcType([$Uint8], [sliceType, $error], false)}, {prop: "readSlice", name: "readSlice", pkg: "bytes", typ: $funcType([$Uint8], [sliceType, $error], false)}, {prop: "ReadString", name: "ReadString", pkg: "", typ: $funcType([$Uint8], [$String, $error], false)}];
	Buffer.init([{prop: "buf", name: "buf", pkg: "bytes", typ: sliceType, tag: ""}, {prop: "off", name: "off", pkg: "bytes", typ: $Int, tag: ""}, {prop: "runeBytes", name: "runeBytes", pkg: "bytes", typ: arrayType, tag: ""}, {prop: "bootstrap", name: "bootstrap", pkg: "bytes", typ: arrayType$1, tag: ""}, {prop: "lastRead", name: "lastRead", pkg: "bytes", typ: readOp, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = io.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = unicode.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = utf8.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.ErrTooLarge = errors.New("bytes.Buffer: too large");
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["syscall"] = (function() {
	var $pkg = {}, $init, bytes, errors, js, race, runtime, sync, mmapper, Errno, _C_int, Timespec, Stat_t, Dirent, sliceType, sliceType$1, ptrType$2, arrayType$1, sliceType$5, ptrType$12, arrayType$10, arrayType$13, arrayType$14, arrayType$15, structType, ptrType$20, ptrType$27, mapType, funcType, funcType$1, ptrType$31, arrayType$17, warningPrinted, lineBuffer, syscallModule, alreadyTriedToLoad, minusOne, envOnce, envLock, env, envs, freebsdConfArch, minRoutingSockaddrLen, mapper, errEAGAIN, errEINVAL, errENOENT, ioSync, ioSync$24ptr, errors$1, init, printWarning, printToConsole, use, runtime_envs, syscall, Syscall, Syscall6, BytePtrFromString, copyenv, Getenv, msanRead, msanWrite, rsaAlignOf, itoa, uitoa, ByteSliceFromString, ReadDirent, Sysctl, nametomib, ParseDirent, errnoErr, Read, Write, sysctl, Close, Fchdir, Fchmod, Fchown, Fstat, Fsync, Ftruncate, Getdirentries, Lstat, Pread, Pwrite, read, Seek, write, mmap, munmap;
	bytes = $packages["bytes"];
	errors = $packages["errors"];
	js = $packages["github.com/gopherjs/gopherjs/js"];
	race = $packages["internal/race"];
	runtime = $packages["runtime"];
	sync = $packages["sync"];
	mmapper = $pkg.mmapper = $newType(0, $kindStruct, "syscall.mmapper", "mmapper", "syscall", function(Mutex_, active_, mmap_, munmap_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Mutex = new sync.Mutex.ptr(0, 0);
			this.active = false;
			this.mmap = $throwNilPointerError;
			this.munmap = $throwNilPointerError;
			return;
		}
		this.Mutex = Mutex_;
		this.active = active_;
		this.mmap = mmap_;
		this.munmap = munmap_;
	});
	Errno = $pkg.Errno = $newType(4, $kindUintptr, "syscall.Errno", "Errno", "syscall", null);
	_C_int = $pkg._C_int = $newType(4, $kindInt32, "syscall._C_int", "_C_int", "syscall", null);
	Timespec = $pkg.Timespec = $newType(0, $kindStruct, "syscall.Timespec", "Timespec", "syscall", function(Sec_, Nsec_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Sec = new $Int64(0, 0);
			this.Nsec = new $Int64(0, 0);
			return;
		}
		this.Sec = Sec_;
		this.Nsec = Nsec_;
	});
	Stat_t = $pkg.Stat_t = $newType(0, $kindStruct, "syscall.Stat_t", "Stat_t", "syscall", function(Dev_, Mode_, Nlink_, Ino_, Uid_, Gid_, Rdev_, Pad_cgo_0_, Atimespec_, Mtimespec_, Ctimespec_, Birthtimespec_, Size_, Blocks_, Blksize_, Flags_, Gen_, Lspare_, Qspare_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Dev = 0;
			this.Mode = 0;
			this.Nlink = 0;
			this.Ino = new $Uint64(0, 0);
			this.Uid = 0;
			this.Gid = 0;
			this.Rdev = 0;
			this.Pad_cgo_0 = arrayType$1.zero();
			this.Atimespec = new Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0));
			this.Mtimespec = new Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0));
			this.Ctimespec = new Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0));
			this.Birthtimespec = new Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0));
			this.Size = new $Int64(0, 0);
			this.Blocks = new $Int64(0, 0);
			this.Blksize = 0;
			this.Flags = 0;
			this.Gen = 0;
			this.Lspare = 0;
			this.Qspare = arrayType$17.zero();
			return;
		}
		this.Dev = Dev_;
		this.Mode = Mode_;
		this.Nlink = Nlink_;
		this.Ino = Ino_;
		this.Uid = Uid_;
		this.Gid = Gid_;
		this.Rdev = Rdev_;
		this.Pad_cgo_0 = Pad_cgo_0_;
		this.Atimespec = Atimespec_;
		this.Mtimespec = Mtimespec_;
		this.Ctimespec = Ctimespec_;
		this.Birthtimespec = Birthtimespec_;
		this.Size = Size_;
		this.Blocks = Blocks_;
		this.Blksize = Blksize_;
		this.Flags = Flags_;
		this.Gen = Gen_;
		this.Lspare = Lspare_;
		this.Qspare = Qspare_;
	});
	Dirent = $pkg.Dirent = $newType(0, $kindStruct, "syscall.Dirent", "Dirent", "syscall", function(Ino_, Seekoff_, Reclen_, Namlen_, Type_, Name_, Pad_cgo_0_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Ino = new $Uint64(0, 0);
			this.Seekoff = new $Uint64(0, 0);
			this.Reclen = 0;
			this.Namlen = 0;
			this.Type = 0;
			this.Name = arrayType$14.zero();
			this.Pad_cgo_0 = arrayType$15.zero();
			return;
		}
		this.Ino = Ino_;
		this.Seekoff = Seekoff_;
		this.Reclen = Reclen_;
		this.Namlen = Namlen_;
		this.Type = Type_;
		this.Name = Name_;
		this.Pad_cgo_0 = Pad_cgo_0_;
	});
	sliceType = $sliceType($Uint8);
	sliceType$1 = $sliceType($String);
	ptrType$2 = $ptrType($Uint8);
	arrayType$1 = $arrayType($Uint8, 4);
	sliceType$5 = $sliceType(_C_int);
	ptrType$12 = $ptrType($Uintptr);
	arrayType$10 = $arrayType($Uint8, 32);
	arrayType$13 = $arrayType(_C_int, 14);
	arrayType$14 = $arrayType($Int8, 1024);
	arrayType$15 = $arrayType($Uint8, 3);
	structType = $structType([{prop: "addr", name: "addr", pkg: "syscall", typ: $Uintptr, tag: ""}, {prop: "len", name: "len", pkg: "syscall", typ: $Int, tag: ""}, {prop: "cap", name: "cap", pkg: "syscall", typ: $Int, tag: ""}]);
	ptrType$20 = $ptrType($Int64);
	ptrType$27 = $ptrType(mmapper);
	mapType = $mapType(ptrType$2, sliceType);
	funcType = $funcType([$Uintptr, $Uintptr, $Int, $Int, $Int, $Int64], [$Uintptr, $error], false);
	funcType$1 = $funcType([$Uintptr, $Uintptr], [$error], false);
	ptrType$31 = $ptrType(Timespec);
	arrayType$17 = $arrayType($Int64, 2);
	init = function() {
		var $ptr;
		$flushConsole = (function() {
			var $ptr;
			if (!((lineBuffer.$length === 0))) {
				$global.console.log($externalize($bytesToString(lineBuffer), $String));
				lineBuffer = sliceType.nil;
			}
		});
	};
	printWarning = function() {
		var $ptr;
		if (!warningPrinted) {
			$global.console.error($externalize("warning: system calls not available, see https://github.com/gopherjs/gopherjs/blob/master/doc/syscalls.md", $String));
		}
		warningPrinted = true;
	};
	printToConsole = function(b) {
		var $ptr, b, goPrintToConsole, i;
		goPrintToConsole = $global.goPrintToConsole;
		if (!(goPrintToConsole === undefined)) {
			goPrintToConsole(b);
			return;
		}
		lineBuffer = $appendSlice(lineBuffer, b);
		while (true) {
			i = bytes.IndexByte(lineBuffer, 10);
			if (i === -1) {
				break;
			}
			$global.console.log($externalize($bytesToString($subslice(lineBuffer, 0, i)), $String));
			lineBuffer = $subslice(lineBuffer, (i + 1 >> 0));
		}
	};
	use = function(p) {
		var $ptr, p;
	};
	runtime_envs = function() {
		var $ptr, envkeys, envs$1, i, jsEnv, key, process;
		process = $global.process;
		if (process === undefined) {
			return sliceType$1.nil;
		}
		jsEnv = process.env;
		envkeys = $global.Object.keys(jsEnv);
		envs$1 = $makeSlice(sliceType$1, $parseInt(envkeys.length));
		i = 0;
		while (true) {
			if (!(i < $parseInt(envkeys.length))) { break; }
			key = $internalize(envkeys[i], $String);
			((i < 0 || i >= envs$1.$length) ? $throwRuntimeError("index out of range") : envs$1.$array[envs$1.$offset + i] = key + "=" + $internalize(jsEnv[$externalize(key, $String)], $String));
			i = i + (1) >> 0;
		}
		return envs$1;
	};
	syscall = function(name) {
		var $ptr, name, require, $deferred;
		/* */ var $err = null; try { $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		$deferred.push([(function() {
			var $ptr;
			$recover();
		}), []]);
		if (syscallModule === null) {
			if (alreadyTriedToLoad) {
				return null;
			}
			alreadyTriedToLoad = true;
			require = $global.require;
			if (require === undefined) {
				$panic(new $String(""));
			}
			syscallModule = require($externalize("syscall", $String));
		}
		return syscallModule[$externalize(name, $String)];
		/* */ } catch(err) { $err = err; return null; } finally { $callDeferred($deferred, $err); }
	};
	Syscall = function(trap, a1, a2, a3) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, a1, a2, a3, array, err, f, r, r1, r2, slice, trap;
		r1 = 0;
		r2 = 0;
		err = 0;
		f = syscall("Syscall");
		if (!(f === null)) {
			r = f(trap, a1, a2, a3);
			_tmp = (($parseInt(r[0]) >> 0) >>> 0);
			_tmp$1 = (($parseInt(r[1]) >> 0) >>> 0);
			_tmp$2 = (($parseInt(r[2]) >> 0) >>> 0);
			r1 = _tmp;
			r2 = _tmp$1;
			err = _tmp$2;
			return [r1, r2, err];
		}
		if ((trap === 4) && ((a1 === 1) || (a1 === 2))) {
			array = a2;
			slice = $makeSlice(sliceType, $parseInt(array.length));
			slice.$array = array;
			printToConsole(slice);
			_tmp$3 = ($parseInt(array.length) >>> 0);
			_tmp$4 = 0;
			_tmp$5 = 0;
			r1 = _tmp$3;
			r2 = _tmp$4;
			err = _tmp$5;
			return [r1, r2, err];
		}
		if (trap === 1) {
			runtime.Goexit();
		}
		printWarning();
		_tmp$6 = (minusOne >>> 0);
		_tmp$7 = 0;
		_tmp$8 = 13;
		r1 = _tmp$6;
		r2 = _tmp$7;
		err = _tmp$8;
		return [r1, r2, err];
	};
	$pkg.Syscall = Syscall;
	Syscall6 = function(trap, a1, a2, a3, a4, a5, a6) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, a1, a2, a3, a4, a5, a6, err, f, r, r1, r2, trap;
		r1 = 0;
		r2 = 0;
		err = 0;
		f = syscall("Syscall6");
		if (!(f === null)) {
			r = f(trap, a1, a2, a3, a4, a5, a6);
			_tmp = (($parseInt(r[0]) >> 0) >>> 0);
			_tmp$1 = (($parseInt(r[1]) >> 0) >>> 0);
			_tmp$2 = (($parseInt(r[2]) >> 0) >>> 0);
			r1 = _tmp;
			r2 = _tmp$1;
			err = _tmp$2;
			return [r1, r2, err];
		}
		if (!((trap === 202))) {
			printWarning();
		}
		_tmp$3 = (minusOne >>> 0);
		_tmp$4 = 0;
		_tmp$5 = 13;
		r1 = _tmp$3;
		r2 = _tmp$4;
		err = _tmp$5;
		return [r1, r2, err];
	};
	$pkg.Syscall6 = Syscall6;
	BytePtrFromString = function(s) {
		var $ptr, _i, _ref, array, b, i, s;
		array = new ($global.Uint8Array)(s.length + 1 >> 0);
		_ref = new sliceType($stringToBytes(s));
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			b = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			if (b === 0) {
				return [ptrType$2.nil, new Errno(22)];
			}
			array[i] = b;
			_i++;
		}
		array[s.length] = 0;
		return [array, $ifaceNil];
	};
	$pkg.BytePtrFromString = BytePtrFromString;
	copyenv = function() {
		var $ptr, _entry, _i, _key, _ref, _tuple, i, j, key, ok, s;
		env = {};
		_ref = envs;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			s = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			j = 0;
			while (true) {
				if (!(j < s.length)) { break; }
				if (s.charCodeAt(j) === 61) {
					key = s.substring(0, j);
					_tuple = (_entry = env[$String.keyFor(key)], _entry !== undefined ? [_entry.v, true] : [0, false]);
					ok = _tuple[1];
					if (!ok) {
						_key = key; (env || $throwRuntimeError("assignment to entry in nil map"))[$String.keyFor(_key)] = { k: _key, v: i };
					} else {
						((i < 0 || i >= envs.$length) ? $throwRuntimeError("index out of range") : envs.$array[envs.$offset + i] = "");
					}
					break;
				}
				j = j + (1) >> 0;
			}
			_i++;
		}
	};
	Getenv = function(key) {
		var $ptr, _entry, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tuple, found, i, i$1, key, ok, s, value, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _entry = $f._entry; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tmp$4 = $f._tmp$4; _tmp$5 = $f._tmp$5; _tmp$6 = $f._tmp$6; _tmp$7 = $f._tmp$7; _tuple = $f._tuple; found = $f.found; i = $f.i; i$1 = $f.i$1; key = $f.key; ok = $f.ok; s = $f.s; value = $f.value; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		value = "";
		found = false;
		$r = envOnce.Do(copyenv); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		if (key.length === 0) {
			_tmp = "";
			_tmp$1 = false;
			value = _tmp;
			found = _tmp$1;
			return [value, found];
		}
		$r = envLock.RLock(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$deferred.push([$methodVal(envLock, "RUnlock"), []]);
		_tuple = (_entry = env[$String.keyFor(key)], _entry !== undefined ? [_entry.v, true] : [0, false]);
		i = _tuple[0];
		ok = _tuple[1];
		if (!ok) {
			_tmp$2 = "";
			_tmp$3 = false;
			value = _tmp$2;
			found = _tmp$3;
			return [value, found];
		}
		s = ((i < 0 || i >= envs.$length) ? $throwRuntimeError("index out of range") : envs.$array[envs.$offset + i]);
		i$1 = 0;
		while (true) {
			if (!(i$1 < s.length)) { break; }
			if (s.charCodeAt(i$1) === 61) {
				_tmp$4 = s.substring((i$1 + 1 >> 0));
				_tmp$5 = true;
				value = _tmp$4;
				found = _tmp$5;
				return [value, found];
			}
			i$1 = i$1 + (1) >> 0;
		}
		_tmp$6 = "";
		_tmp$7 = false;
		value = _tmp$6;
		found = _tmp$7;
		return [value, found];
		/* */ } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if (!$curGoroutine.asleep) { return  [value, found]; } if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: Getenv }; } $f.$ptr = $ptr; $f._entry = _entry; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tmp$4 = _tmp$4; $f._tmp$5 = _tmp$5; $f._tmp$6 = _tmp$6; $f._tmp$7 = _tmp$7; $f._tuple = _tuple; $f.found = found; $f.i = i; $f.i$1 = i$1; $f.key = key; $f.ok = ok; $f.s = s; $f.value = value; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	$pkg.Getenv = Getenv;
	msanRead = function(addr, len) {
		var $ptr, addr, len;
	};
	msanWrite = function(addr, len) {
		var $ptr, addr, len;
	};
	rsaAlignOf = function(salen) {
		var $ptr, salen, salign;
		salign = 8;
		if (true) {
			salign = 4;
		} else if (false) {
			salign = 8;
		} else if (false) {
			if (freebsdConfArch === "amd64") {
				salign = 8;
			}
		}
		if (salen === 0) {
			return salign;
		}
		return (((salen + salign >> 0) - 1 >> 0)) & (~((salign - 1 >> 0)) >> 0);
	};
	itoa = function(val) {
		var $ptr, val;
		if (val < 0) {
			return "-" + uitoa((-val >>> 0));
		}
		return uitoa((val >>> 0));
	};
	uitoa = function(val) {
		var $ptr, _q, _r, buf, i, val;
		buf = arrayType$10.zero();
		i = 31;
		while (true) {
			if (!(val >= 10)) { break; }
			((i < 0 || i >= buf.length) ? $throwRuntimeError("index out of range") : buf[i] = (((_r = val % 10, _r === _r ? _r : $throwRuntimeError("integer divide by zero")) + 48 >>> 0) << 24 >>> 24));
			i = i - (1) >> 0;
			val = (_q = val / (10), (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
		}
		((i < 0 || i >= buf.length) ? $throwRuntimeError("index out of range") : buf[i] = ((val + 48 >>> 0) << 24 >>> 24));
		return $bytesToString($subslice(new sliceType(buf), i));
	};
	ByteSliceFromString = function(s) {
		var $ptr, a, i, s;
		i = 0;
		while (true) {
			if (!(i < s.length)) { break; }
			if (s.charCodeAt(i) === 0) {
				return [sliceType.nil, new Errno(22)];
			}
			i = i + (1) >> 0;
		}
		a = $makeSlice(sliceType, (s.length + 1 >> 0));
		$copyString(a, s);
		return [a, $ifaceNil];
	};
	$pkg.ByteSliceFromString = ByteSliceFromString;
	Timespec.ptr.prototype.Unix = function() {
		var $ptr, _tmp, _tmp$1, nsec, sec, ts;
		sec = new $Int64(0, 0);
		nsec = new $Int64(0, 0);
		ts = this;
		_tmp = ts.Sec;
		_tmp$1 = ts.Nsec;
		sec = _tmp;
		nsec = _tmp$1;
		return [sec, nsec];
	};
	Timespec.prototype.Unix = function() { return this.$val.Unix(); };
	Timespec.ptr.prototype.Nano = function() {
		var $ptr, ts, x, x$1;
		ts = this;
		return (x = $mul64(ts.Sec, new $Int64(0, 1000000000)), x$1 = ts.Nsec, new $Int64(x.$high + x$1.$high, x.$low + x$1.$low));
	};
	Timespec.prototype.Nano = function() { return this.$val.Nano(); };
	ReadDirent = function(fd, buf) {
		var $ptr, _tuple, base, buf, err, fd, n;
		n = 0;
		err = $ifaceNil;
		base = new Uint8Array(8);
		_tuple = Getdirentries(fd, buf, base);
		n = _tuple[0];
		err = _tuple[1];
		return [n, err];
	};
	$pkg.ReadDirent = ReadDirent;
	Sysctl = function(name) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, _tuple, buf, err, mib, n, n$24ptr, name, value, x;
		value = "";
		err = $ifaceNil;
		_tuple = nametomib(name);
		mib = _tuple[0];
		err = _tuple[1];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			_tmp = "";
			_tmp$1 = err;
			value = _tmp;
			err = _tmp$1;
			return [value, err];
		}
		n = 0;
		err = sysctl(mib, ptrType$2.nil, (n$24ptr || (n$24ptr = new ptrType$12(function() { return n; }, function($v) { n = $v; }))), ptrType$2.nil, 0);
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			_tmp$2 = "";
			_tmp$3 = err;
			value = _tmp$2;
			err = _tmp$3;
			return [value, err];
		}
		if (n === 0) {
			_tmp$4 = "";
			_tmp$5 = $ifaceNil;
			value = _tmp$4;
			err = _tmp$5;
			return [value, err];
		}
		buf = $makeSlice(sliceType, n);
		err = sysctl(mib, $indexPtr(buf.$array, buf.$offset + 0, ptrType$2), (n$24ptr || (n$24ptr = new ptrType$12(function() { return n; }, function($v) { n = $v; }))), ptrType$2.nil, 0);
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			_tmp$6 = "";
			_tmp$7 = err;
			value = _tmp$6;
			err = _tmp$7;
			return [value, err];
		}
		if (n > 0 && ((x = n - 1 >>> 0, ((x < 0 || x >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + x])) === 0)) {
			n = n - (1) >>> 0;
		}
		_tmp$8 = $bytesToString($subslice(buf, 0, n));
		_tmp$9 = $ifaceNil;
		value = _tmp$8;
		err = _tmp$9;
		return [value, err];
	};
	$pkg.Sysctl = Sysctl;
	nametomib = function(name) {
		var $ptr, _q, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tuple, buf, bytes$1, err, mib, n, n$24ptr, name, p;
		mib = sliceType$5.nil;
		err = $ifaceNil;
		buf = arrayType$13.zero();
		n = 48;
		p = $sliceToArray(new sliceType(buf));
		_tuple = ByteSliceFromString(name);
		bytes$1 = _tuple[0];
		err = _tuple[1];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			_tmp = sliceType$5.nil;
			_tmp$1 = err;
			mib = _tmp;
			err = _tmp$1;
			return [mib, err];
		}
		err = sysctl(new sliceType$5([0, 3]), p, (n$24ptr || (n$24ptr = new ptrType$12(function() { return n; }, function($v) { n = $v; }))), $indexPtr(bytes$1.$array, bytes$1.$offset + 0, ptrType$2), (name.length >>> 0));
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			_tmp$2 = sliceType$5.nil;
			_tmp$3 = err;
			mib = _tmp$2;
			err = _tmp$3;
			return [mib, err];
		}
		_tmp$4 = $subslice(new sliceType$5(buf), 0, (_q = n / 4, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero")));
		_tmp$5 = $ifaceNil;
		mib = _tmp$4;
		err = _tmp$5;
		return [mib, err];
	};
	ParseDirent = function(buf, max, names) {
		var $ptr, _array, _struct, _tmp, _tmp$1, _tmp$2, _view, buf, bytes$1, consumed, count, dirent, max, name, names, newnames, origlen, x;
		consumed = 0;
		count = 0;
		newnames = sliceType$1.nil;
		origlen = buf.$length;
		while (true) {
			if (!(!((max === 0)) && buf.$length > 0)) { break; }
			dirent = (_array = $sliceToArray(buf), _struct = new Dirent.ptr(new $Uint64(0, 0), new $Uint64(0, 0), 0, 0, 0, arrayType$14.zero(), arrayType$15.zero()), _view = new DataView(_array.buffer, _array.byteOffset), _struct.Ino = new $Uint64(_view.getUint32(4, true), _view.getUint32(0, true)), _struct.Seekoff = new $Uint64(_view.getUint32(12, true), _view.getUint32(8, true)), _struct.Reclen = _view.getUint16(16, true), _struct.Namlen = _view.getUint16(18, true), _struct.Type = _view.getUint8(20, true), _struct.Name = new ($nativeArray($kindInt8))(_array.buffer, $min(_array.byteOffset + 21, _array.buffer.byteLength)), _struct.Pad_cgo_0 = new ($nativeArray($kindUint8))(_array.buffer, $min(_array.byteOffset + 1045, _array.buffer.byteLength)), _struct);
			if (dirent.Reclen === 0) {
				buf = sliceType.nil;
				break;
			}
			buf = $subslice(buf, dirent.Reclen);
			if ((x = dirent.Ino, (x.$high === 0 && x.$low === 0))) {
				continue;
			}
			bytes$1 = $sliceToArray(new sliceType(dirent.Name));
			name = $bytesToString($subslice(new sliceType(bytes$1), 0, dirent.Namlen));
			if (name === "." || name === "..") {
				continue;
			}
			max = max - (1) >> 0;
			count = count + (1) >> 0;
			names = $append(names, name);
		}
		_tmp = origlen - buf.$length >> 0;
		_tmp$1 = count;
		_tmp$2 = names;
		consumed = _tmp;
		count = _tmp$1;
		newnames = _tmp$2;
		return [consumed, count, newnames];
	};
	$pkg.ParseDirent = ParseDirent;
	mmapper.ptr.prototype.Mmap = function(fd, offset, length, prot, flags) {
		var $ptr, _key, _r, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tuple, addr, b, data, err, errno, fd, flags, length, m, offset, p, prot, sl, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _key = $f._key; _r = $f._r; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tmp$4 = $f._tmp$4; _tmp$5 = $f._tmp$5; _tuple = $f._tuple; addr = $f.addr; b = $f.b; data = $f.data; err = $f.err; errno = $f.errno; fd = $f.fd; flags = $f.flags; length = $f.length; m = $f.m; offset = $f.offset; p = $f.p; prot = $f.prot; sl = $f.sl; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		sl = [sl];
		data = sliceType.nil;
		err = $ifaceNil;
		m = this;
		if (length <= 0) {
			_tmp = sliceType.nil;
			_tmp$1 = new Errno(22);
			data = _tmp;
			err = _tmp$1;
			return [data, err];
		}
		_r = m.mmap(0, (length >>> 0), prot, flags, fd, offset); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		addr = _tuple[0];
		errno = _tuple[1];
		if (!($interfaceIsEqual(errno, $ifaceNil))) {
			_tmp$2 = sliceType.nil;
			_tmp$3 = errno;
			data = _tmp$2;
			err = _tmp$3;
			return [data, err];
		}
		sl[0] = new structType.ptr(addr, length, length);
		b = sl[0];
		p = $indexPtr(b.$array, b.$offset + (b.$capacity - 1 >> 0), ptrType$2);
		$r = m.Mutex.Lock(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$deferred.push([$methodVal(m.Mutex, "Unlock"), []]);
		_key = p; (m.active || $throwRuntimeError("assignment to entry in nil map"))[ptrType$2.keyFor(_key)] = { k: _key, v: b };
		_tmp$4 = b;
		_tmp$5 = $ifaceNil;
		data = _tmp$4;
		err = _tmp$5;
		return [data, err];
		/* */ } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if (!$curGoroutine.asleep) { return  [data, err]; } if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: mmapper.ptr.prototype.Mmap }; } $f.$ptr = $ptr; $f._key = _key; $f._r = _r; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tmp$4 = _tmp$4; $f._tmp$5 = _tmp$5; $f._tuple = _tuple; $f.addr = addr; $f.b = b; $f.data = data; $f.err = err; $f.errno = errno; $f.fd = fd; $f.flags = flags; $f.length = length; $f.m = m; $f.offset = offset; $f.p = p; $f.prot = prot; $f.sl = sl; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	mmapper.prototype.Mmap = function(fd, offset, length, prot, flags) { return this.$val.Mmap(fd, offset, length, prot, flags); };
	mmapper.ptr.prototype.Munmap = function(data) {
		var $ptr, _entry, _r, b, data, err, errno, m, p, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _entry = $f._entry; _r = $f._r; b = $f.b; data = $f.data; err = $f.err; errno = $f.errno; m = $f.m; p = $f.p; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		err = $ifaceNil;
		m = this;
		if ((data.$length === 0) || !((data.$length === data.$capacity))) {
			err = new Errno(22);
			return err;
		}
		p = $indexPtr(data.$array, data.$offset + (data.$capacity - 1 >> 0), ptrType$2);
		$r = m.Mutex.Lock(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$deferred.push([$methodVal(m.Mutex, "Unlock"), []]);
		b = (_entry = m.active[ptrType$2.keyFor(p)], _entry !== undefined ? _entry.v : sliceType.nil);
		if (b === sliceType.nil || !($indexPtr(b.$array, b.$offset + 0, ptrType$2) === $indexPtr(data.$array, data.$offset + 0, ptrType$2))) {
			err = new Errno(22);
			return err;
		}
		_r = m.munmap($sliceToArray(b), (b.$length >>> 0)); /* */ $s = 2; case 2: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		errno = _r;
		if (!($interfaceIsEqual(errno, $ifaceNil))) {
			err = errno;
			return err;
		}
		delete m.active[ptrType$2.keyFor(p)];
		err = $ifaceNil;
		return err;
		/* */ } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if (!$curGoroutine.asleep) { return  err; } if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: mmapper.ptr.prototype.Munmap }; } $f.$ptr = $ptr; $f._entry = _entry; $f._r = _r; $f.b = b; $f.data = data; $f.err = err; $f.errno = errno; $f.m = m; $f.p = p; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	mmapper.prototype.Munmap = function(data) { return this.$val.Munmap(data); };
	Errno.prototype.Error = function() {
		var $ptr, e, s;
		e = this.$val;
		if (0 <= (e >> 0) && (e >> 0) < 106) {
			s = ((e < 0 || e >= errors$1.length) ? $throwRuntimeError("index out of range") : errors$1[e]);
			if (!(s === "")) {
				return s;
			}
		}
		return "errno " + itoa((e >> 0));
	};
	$ptrType(Errno).prototype.Error = function() { return new Errno(this.$get()).Error(); };
	Errno.prototype.Temporary = function() {
		var $ptr, e;
		e = this.$val;
		return (e === 4) || (e === 24) || (e === 54) || (e === 53) || new Errno(e).Timeout();
	};
	$ptrType(Errno).prototype.Temporary = function() { return new Errno(this.$get()).Temporary(); };
	Errno.prototype.Timeout = function() {
		var $ptr, e;
		e = this.$val;
		return (e === 35) || (e === 35) || (e === 60);
	};
	$ptrType(Errno).prototype.Timeout = function() { return new Errno(this.$get()).Timeout(); };
	errnoErr = function(e) {
		var $ptr, _1, e;
		_1 = e;
		if (_1 === (0)) {
			return $ifaceNil;
		} else if (_1 === (35)) {
			return errEAGAIN;
		} else if (_1 === (22)) {
			return errEINVAL;
		} else if (_1 === (2)) {
			return errENOENT;
		}
		return new Errno(e);
	};
	Read = function(fd, p) {
		var $ptr, _tuple, err, fd, n, p;
		n = 0;
		err = $ifaceNil;
		_tuple = read(fd, p);
		n = _tuple[0];
		err = _tuple[1];
		if (false) {
			if (n > 0) {
				race.WriteRange($sliceToArray(p), n);
			}
			if ($interfaceIsEqual(err, $ifaceNil)) {
				race.Acquire((ioSync$24ptr || (ioSync$24ptr = new ptrType$20(function() { return ioSync; }, function($v) { ioSync = $v; }))));
			}
		}
		if (false && n > 0) {
			msanWrite($sliceToArray(p), n);
		}
		return [n, err];
	};
	$pkg.Read = Read;
	Write = function(fd, p) {
		var $ptr, _tuple, err, fd, n, p;
		n = 0;
		err = $ifaceNil;
		if (false) {
			race.ReleaseMerge((ioSync$24ptr || (ioSync$24ptr = new ptrType$20(function() { return ioSync; }, function($v) { ioSync = $v; }))));
		}
		_tuple = write(fd, p);
		n = _tuple[0];
		err = _tuple[1];
		if (false && n > 0) {
			race.ReadRange($sliceToArray(p), n);
		}
		if (false && n > 0) {
			msanRead($sliceToArray(p), n);
		}
		return [n, err];
	};
	$pkg.Write = Write;
	sysctl = function(mib, old, oldlen, new$1, newlen) {
		var $ptr, _p0, _tuple, e1, err, mib, new$1, newlen, old, oldlen;
		err = $ifaceNil;
		_p0 = 0;
		if (mib.$length > 0) {
			_p0 = $sliceToArray(mib);
		} else {
			_p0 = new Uint8Array(0);
		}
		_tuple = Syscall6(202, _p0, (mib.$length >>> 0), old, oldlen, new$1, newlen);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	Close = function(fd) {
		var $ptr, _tuple, e1, err, fd;
		err = $ifaceNil;
		_tuple = Syscall(6, (fd >>> 0), 0, 0);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Close = Close;
	Fchdir = function(fd) {
		var $ptr, _tuple, e1, err, fd;
		err = $ifaceNil;
		_tuple = Syscall(13, (fd >>> 0), 0, 0);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Fchdir = Fchdir;
	Fchmod = function(fd, mode) {
		var $ptr, _tuple, e1, err, fd, mode;
		err = $ifaceNil;
		_tuple = Syscall(124, (fd >>> 0), (mode >>> 0), 0);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Fchmod = Fchmod;
	Fchown = function(fd, uid, gid) {
		var $ptr, _tuple, e1, err, fd, gid, uid;
		err = $ifaceNil;
		_tuple = Syscall(123, (fd >>> 0), (uid >>> 0), (gid >>> 0));
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Fchown = Fchown;
	Fstat = function(fd, stat) {
		var $ptr, _array, _struct, _tuple, _view, e1, err, fd, stat;
		err = $ifaceNil;
		_array = new Uint8Array(144);
		_tuple = Syscall(339, (fd >>> 0), _array, 0);
		_struct = stat, _view = new DataView(_array.buffer, _array.byteOffset), _struct.Dev = _view.getInt32(0, true), _struct.Mode = _view.getUint16(4, true), _struct.Nlink = _view.getUint16(6, true), _struct.Ino = new $Uint64(_view.getUint32(12, true), _view.getUint32(8, true)), _struct.Uid = _view.getUint32(16, true), _struct.Gid = _view.getUint32(20, true), _struct.Rdev = _view.getInt32(24, true), _struct.Pad_cgo_0 = new ($nativeArray($kindUint8))(_array.buffer, $min(_array.byteOffset + 28, _array.buffer.byteLength)), _struct.Atimespec.Sec = new $Int64(_view.getUint32(36, true), _view.getUint32(32, true)), _struct.Atimespec.Nsec = new $Int64(_view.getUint32(44, true), _view.getUint32(40, true)), _struct.Mtimespec.Sec = new $Int64(_view.getUint32(52, true), _view.getUint32(48, true)), _struct.Mtimespec.Nsec = new $Int64(_view.getUint32(60, true), _view.getUint32(56, true)), _struct.Ctimespec.Sec = new $Int64(_view.getUint32(68, true), _view.getUint32(64, true)), _struct.Ctimespec.Nsec = new $Int64(_view.getUint32(76, true), _view.getUint32(72, true)), _struct.Birthtimespec.Sec = new $Int64(_view.getUint32(84, true), _view.getUint32(80, true)), _struct.Birthtimespec.Nsec = new $Int64(_view.getUint32(92, true), _view.getUint32(88, true)), _struct.Size = new $Int64(_view.getUint32(100, true), _view.getUint32(96, true)), _struct.Blocks = new $Int64(_view.getUint32(108, true), _view.getUint32(104, true)), _struct.Blksize = _view.getInt32(112, true), _struct.Flags = _view.getUint32(116, true), _struct.Gen = _view.getUint32(120, true), _struct.Lspare = _view.getInt32(124, true), _struct.Qspare = new ($nativeArray($kindInt64))(_array.buffer, $min(_array.byteOffset + 128, _array.buffer.byteLength));
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Fstat = Fstat;
	Fsync = function(fd) {
		var $ptr, _tuple, e1, err, fd;
		err = $ifaceNil;
		_tuple = Syscall(95, (fd >>> 0), 0, 0);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Fsync = Fsync;
	Ftruncate = function(fd, length) {
		var $ptr, _tuple, e1, err, fd, length;
		err = $ifaceNil;
		_tuple = Syscall(201, (fd >>> 0), (length.$low >>> 0), 0);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Ftruncate = Ftruncate;
	Getdirentries = function(fd, buf, basep) {
		var $ptr, _p0, _tuple, basep, buf, e1, err, fd, n, r0;
		n = 0;
		err = $ifaceNil;
		_p0 = 0;
		if (buf.$length > 0) {
			_p0 = $sliceToArray(buf);
		} else {
			_p0 = new Uint8Array(0);
		}
		_tuple = Syscall6(344, (fd >>> 0), _p0, (buf.$length >>> 0), basep, 0, 0);
		r0 = _tuple[0];
		e1 = _tuple[2];
		n = (r0 >> 0);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [n, err];
	};
	$pkg.Getdirentries = Getdirentries;
	Lstat = function(path, stat) {
		var $ptr, _array, _p0, _struct, _tuple, _tuple$1, _view, e1, err, path, stat;
		err = $ifaceNil;
		_p0 = ptrType$2.nil;
		_tuple = BytePtrFromString(path);
		_p0 = _tuple[0];
		err = _tuple[1];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return err;
		}
		_array = new Uint8Array(144);
		_tuple$1 = Syscall(340, _p0, _array, 0);
		_struct = stat, _view = new DataView(_array.buffer, _array.byteOffset), _struct.Dev = _view.getInt32(0, true), _struct.Mode = _view.getUint16(4, true), _struct.Nlink = _view.getUint16(6, true), _struct.Ino = new $Uint64(_view.getUint32(12, true), _view.getUint32(8, true)), _struct.Uid = _view.getUint32(16, true), _struct.Gid = _view.getUint32(20, true), _struct.Rdev = _view.getInt32(24, true), _struct.Pad_cgo_0 = new ($nativeArray($kindUint8))(_array.buffer, $min(_array.byteOffset + 28, _array.buffer.byteLength)), _struct.Atimespec.Sec = new $Int64(_view.getUint32(36, true), _view.getUint32(32, true)), _struct.Atimespec.Nsec = new $Int64(_view.getUint32(44, true), _view.getUint32(40, true)), _struct.Mtimespec.Sec = new $Int64(_view.getUint32(52, true), _view.getUint32(48, true)), _struct.Mtimespec.Nsec = new $Int64(_view.getUint32(60, true), _view.getUint32(56, true)), _struct.Ctimespec.Sec = new $Int64(_view.getUint32(68, true), _view.getUint32(64, true)), _struct.Ctimespec.Nsec = new $Int64(_view.getUint32(76, true), _view.getUint32(72, true)), _struct.Birthtimespec.Sec = new $Int64(_view.getUint32(84, true), _view.getUint32(80, true)), _struct.Birthtimespec.Nsec = new $Int64(_view.getUint32(92, true), _view.getUint32(88, true)), _struct.Size = new $Int64(_view.getUint32(100, true), _view.getUint32(96, true)), _struct.Blocks = new $Int64(_view.getUint32(108, true), _view.getUint32(104, true)), _struct.Blksize = _view.getInt32(112, true), _struct.Flags = _view.getUint32(116, true), _struct.Gen = _view.getUint32(120, true), _struct.Lspare = _view.getInt32(124, true), _struct.Qspare = new ($nativeArray($kindInt64))(_array.buffer, $min(_array.byteOffset + 128, _array.buffer.byteLength));
		e1 = _tuple$1[2];
		use(_p0);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	$pkg.Lstat = Lstat;
	Pread = function(fd, p, offset) {
		var $ptr, _p0, _tuple, e1, err, fd, n, offset, p, r0;
		n = 0;
		err = $ifaceNil;
		_p0 = 0;
		if (p.$length > 0) {
			_p0 = $sliceToArray(p);
		} else {
			_p0 = new Uint8Array(0);
		}
		_tuple = Syscall6(153, (fd >>> 0), _p0, (p.$length >>> 0), (offset.$low >>> 0), 0, 0);
		r0 = _tuple[0];
		e1 = _tuple[2];
		n = (r0 >> 0);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [n, err];
	};
	$pkg.Pread = Pread;
	Pwrite = function(fd, p, offset) {
		var $ptr, _p0, _tuple, e1, err, fd, n, offset, p, r0;
		n = 0;
		err = $ifaceNil;
		_p0 = 0;
		if (p.$length > 0) {
			_p0 = $sliceToArray(p);
		} else {
			_p0 = new Uint8Array(0);
		}
		_tuple = Syscall6(154, (fd >>> 0), _p0, (p.$length >>> 0), (offset.$low >>> 0), 0, 0);
		r0 = _tuple[0];
		e1 = _tuple[2];
		n = (r0 >> 0);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [n, err];
	};
	$pkg.Pwrite = Pwrite;
	read = function(fd, p) {
		var $ptr, _p0, _tuple, e1, err, fd, n, p, r0;
		n = 0;
		err = $ifaceNil;
		_p0 = 0;
		if (p.$length > 0) {
			_p0 = $sliceToArray(p);
		} else {
			_p0 = new Uint8Array(0);
		}
		_tuple = Syscall(3, (fd >>> 0), _p0, (p.$length >>> 0));
		r0 = _tuple[0];
		e1 = _tuple[2];
		n = (r0 >> 0);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [n, err];
	};
	Seek = function(fd, offset, whence) {
		var $ptr, _tuple, e1, err, fd, newoffset, offset, r0, whence;
		newoffset = new $Int64(0, 0);
		err = $ifaceNil;
		_tuple = Syscall(199, (fd >>> 0), (offset.$low >>> 0), (whence >>> 0));
		r0 = _tuple[0];
		e1 = _tuple[2];
		newoffset = new $Int64(0, r0.constructor === Number ? r0 : 1);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [newoffset, err];
	};
	$pkg.Seek = Seek;
	write = function(fd, p) {
		var $ptr, _p0, _tuple, e1, err, fd, n, p, r0;
		n = 0;
		err = $ifaceNil;
		_p0 = 0;
		if (p.$length > 0) {
			_p0 = $sliceToArray(p);
		} else {
			_p0 = new Uint8Array(0);
		}
		_tuple = Syscall(4, (fd >>> 0), _p0, (p.$length >>> 0));
		r0 = _tuple[0];
		e1 = _tuple[2];
		n = (r0 >> 0);
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [n, err];
	};
	mmap = function(addr, length, prot, flag, fd, pos) {
		var $ptr, _tuple, addr, e1, err, fd, flag, length, pos, prot, r0, ret;
		ret = 0;
		err = $ifaceNil;
		_tuple = Syscall6(197, addr, length, (prot >>> 0), (flag >>> 0), (fd >>> 0), (pos.$low >>> 0));
		r0 = _tuple[0];
		e1 = _tuple[2];
		ret = r0;
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return [ret, err];
	};
	munmap = function(addr, length) {
		var $ptr, _tuple, addr, e1, err, length;
		err = $ifaceNil;
		_tuple = Syscall(73, addr, length, 0);
		e1 = _tuple[2];
		if (!((e1 === 0))) {
			err = errnoErr(e1);
		}
		return err;
	};
	ptrType$27.methods = [{prop: "Mmap", name: "Mmap", pkg: "", typ: $funcType([$Int, $Int64, $Int, $Int, $Int], [sliceType, $error], false)}, {prop: "Munmap", name: "Munmap", pkg: "", typ: $funcType([sliceType], [$error], false)}];
	Errno.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Temporary", name: "Temporary", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Timeout", name: "Timeout", pkg: "", typ: $funcType([], [$Bool], false)}];
	ptrType$31.methods = [{prop: "Unix", name: "Unix", pkg: "", typ: $funcType([], [$Int64, $Int64], false)}, {prop: "Nano", name: "Nano", pkg: "", typ: $funcType([], [$Int64], false)}];
	mmapper.init([{prop: "Mutex", name: "", pkg: "", typ: sync.Mutex, tag: ""}, {prop: "active", name: "active", pkg: "syscall", typ: mapType, tag: ""}, {prop: "mmap", name: "mmap", pkg: "syscall", typ: funcType, tag: ""}, {prop: "munmap", name: "munmap", pkg: "syscall", typ: funcType$1, tag: ""}]);
	Timespec.init([{prop: "Sec", name: "Sec", pkg: "", typ: $Int64, tag: ""}, {prop: "Nsec", name: "Nsec", pkg: "", typ: $Int64, tag: ""}]);
	Stat_t.init([{prop: "Dev", name: "Dev", pkg: "", typ: $Int32, tag: ""}, {prop: "Mode", name: "Mode", pkg: "", typ: $Uint16, tag: ""}, {prop: "Nlink", name: "Nlink", pkg: "", typ: $Uint16, tag: ""}, {prop: "Ino", name: "Ino", pkg: "", typ: $Uint64, tag: ""}, {prop: "Uid", name: "Uid", pkg: "", typ: $Uint32, tag: ""}, {prop: "Gid", name: "Gid", pkg: "", typ: $Uint32, tag: ""}, {prop: "Rdev", name: "Rdev", pkg: "", typ: $Int32, tag: ""}, {prop: "Pad_cgo_0", name: "Pad_cgo_0", pkg: "", typ: arrayType$1, tag: ""}, {prop: "Atimespec", name: "Atimespec", pkg: "", typ: Timespec, tag: ""}, {prop: "Mtimespec", name: "Mtimespec", pkg: "", typ: Timespec, tag: ""}, {prop: "Ctimespec", name: "Ctimespec", pkg: "", typ: Timespec, tag: ""}, {prop: "Birthtimespec", name: "Birthtimespec", pkg: "", typ: Timespec, tag: ""}, {prop: "Size", name: "Size", pkg: "", typ: $Int64, tag: ""}, {prop: "Blocks", name: "Blocks", pkg: "", typ: $Int64, tag: ""}, {prop: "Blksize", name: "Blksize", pkg: "", typ: $Int32, tag: ""}, {prop: "Flags", name: "Flags", pkg: "", typ: $Uint32, tag: ""}, {prop: "Gen", name: "Gen", pkg: "", typ: $Uint32, tag: ""}, {prop: "Lspare", name: "Lspare", pkg: "", typ: $Int32, tag: ""}, {prop: "Qspare", name: "Qspare", pkg: "", typ: arrayType$17, tag: ""}]);
	Dirent.init([{prop: "Ino", name: "Ino", pkg: "", typ: $Uint64, tag: ""}, {prop: "Seekoff", name: "Seekoff", pkg: "", typ: $Uint64, tag: ""}, {prop: "Reclen", name: "Reclen", pkg: "", typ: $Uint16, tag: ""}, {prop: "Namlen", name: "Namlen", pkg: "", typ: $Uint16, tag: ""}, {prop: "Type", name: "Type", pkg: "", typ: $Uint8, tag: ""}, {prop: "Name", name: "Name", pkg: "", typ: arrayType$14, tag: ""}, {prop: "Pad_cgo_0", name: "Pad_cgo_0", pkg: "", typ: arrayType$15, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = bytes.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = errors.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = js.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = race.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = runtime.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sync.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		lineBuffer = sliceType.nil;
		syscallModule = null;
		envOnce = new sync.Once.ptr(new sync.Mutex.ptr(0, 0), 0);
		envLock = new sync.RWMutex.ptr(new sync.Mutex.ptr(0, 0), 0, 0, 0, 0);
		env = false;
		freebsdConfArch = "";
		ioSync = new $Int64(0, 0);
		warningPrinted = false;
		alreadyTriedToLoad = false;
		minusOne = -1;
		envs = runtime_envs();
		$pkg.Stdin = 0;
		$pkg.Stdout = 1;
		$pkg.Stderr = 2;
		errEAGAIN = new Errno(35);
		errEINVAL = new Errno(22);
		errENOENT = new Errno(2);
		errors$1 = $toNativeArray($kindString, ["", "operation not permitted", "no such file or directory", "no such process", "interrupted system call", "input/output error", "device not configured", "argument list too long", "exec format error", "bad file descriptor", "no child processes", "resource deadlock avoided", "cannot allocate memory", "permission denied", "bad address", "block device required", "resource busy", "file exists", "cross-device link", "operation not supported by device", "not a directory", "is a directory", "invalid argument", "too many open files in system", "too many open files", "inappropriate ioctl for device", "text file busy", "file too large", "no space left on device", "illegal seek", "read-only file system", "too many links", "broken pipe", "numerical argument out of domain", "result too large", "resource temporarily unavailable", "operation now in progress", "operation already in progress", "socket operation on non-socket", "destination address required", "message too long", "protocol wrong type for socket", "protocol not available", "protocol not supported", "socket type not supported", "operation not supported", "protocol family not supported", "address family not supported by protocol family", "address already in use", "can't assign requested address", "network is down", "network is unreachable", "network dropped connection on reset", "software caused connection abort", "connection reset by peer", "no buffer space available", "socket is already connected", "socket is not connected", "can't send after socket shutdown", "too many references: can't splice", "operation timed out", "connection refused", "too many levels of symbolic links", "file name too long", "host is down", "no route to host", "directory not empty", "too many processes", "too many users", "disc quota exceeded", "stale NFS file handle", "too many levels of remote in path", "RPC struct is bad", "RPC version wrong", "RPC prog. not avail", "program version wrong", "bad procedure for program", "no locks available", "function not implemented", "inappropriate file type or format", "authentication error", "need authenticator", "device power is off", "device error", "value too large to be stored in data type", "bad executable (or shared library)", "bad CPU type in executable", "shared library version mismatch", "malformed Mach-o file", "operation canceled", "identifier removed", "no message of desired type", "illegal byte sequence", "attribute not found", "bad message", "EMULTIHOP (Reserved)", "no message available on STREAM", "ENOLINK (Reserved)", "no STREAM resources", "not a STREAM", "protocol error", "STREAM ioctl timeout", "operation not supported on socket", "policy not found", "state not recoverable", "previous owner died"]);
		mapper = new mmapper.ptr(new sync.Mutex.ptr(0, 0), {}, mmap, munmap);
		minRoutingSockaddrLen = rsaAlignOf(0);
		init();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/gopherjs/gopherjs/nosync"] = (function() {
	var $pkg = {}, $init, Once, funcType, ptrType$3;
	Once = $pkg.Once = $newType(0, $kindStruct, "nosync.Once", "Once", "github.com/gopherjs/gopherjs/nosync", function(doing_, done_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.doing = false;
			this.done = false;
			return;
		}
		this.doing = doing_;
		this.done = done_;
	});
	funcType = $funcType([], [], false);
	ptrType$3 = $ptrType(Once);
	Once.ptr.prototype.Do = function(f) {
		var $ptr, f, o, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; f = $f.f; o = $f.o; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		o = [o];
		o[0] = this;
		if (o[0].done) {
			return;
		}
		if (o[0].doing) {
			$panic(new $String("nosync: Do called within f"));
		}
		o[0].doing = true;
		$deferred.push([(function(o) { return function() {
			var $ptr;
			o[0].doing = false;
			o[0].done = true;
		}; })(o), []]);
		$r = f(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ $s = -1; case -1: } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: Once.ptr.prototype.Do }; } $f.$ptr = $ptr; $f.f = f; $f.o = o; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	Once.prototype.Do = function(f) { return this.$val.Do(f); };
	ptrType$3.methods = [{prop: "Do", name: "Do", pkg: "", typ: $funcType([funcType], [], false)}];
	Once.init([{prop: "doing", name: "doing", pkg: "github.com/gopherjs/gopherjs/nosync", typ: $Bool, tag: ""}, {prop: "done", name: "done", pkg: "github.com/gopherjs/gopherjs/nosync", typ: $Bool, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["strings"] = (function() {
	var $pkg = {}, $init, errors, js, io, unicode, utf8, sliceType, IndexByte, Join;
	errors = $packages["errors"];
	js = $packages["github.com/gopherjs/gopherjs/js"];
	io = $packages["io"];
	unicode = $packages["unicode"];
	utf8 = $packages["unicode/utf8"];
	sliceType = $sliceType($Uint8);
	IndexByte = function(s, c) {
		var $ptr, c, s;
		return $parseInt(s.indexOf($global.String.fromCharCode(c))) >> 0;
	};
	$pkg.IndexByte = IndexByte;
	Join = function(a, sep) {
		var $ptr, _i, _ref, a, b, bp, i, n, s, sep;
		if (a.$length === 0) {
			return "";
		}
		if (a.$length === 1) {
			return (0 >= a.$length ? $throwRuntimeError("index out of range") : a.$array[a.$offset + 0]);
		}
		n = $imul(sep.length, ((a.$length - 1 >> 0)));
		i = 0;
		while (true) {
			if (!(i < a.$length)) { break; }
			n = n + (((i < 0 || i >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + i]).length) >> 0;
			i = i + (1) >> 0;
		}
		b = $makeSlice(sliceType, n);
		bp = $copyString(b, (0 >= a.$length ? $throwRuntimeError("index out of range") : a.$array[a.$offset + 0]));
		_ref = $subslice(a, 1);
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			s = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			bp = bp + ($copyString($subslice(b, bp), sep)) >> 0;
			bp = bp + ($copyString($subslice(b, bp), s)) >> 0;
			_i++;
		}
		return $bytesToString(b);
	};
	$pkg.Join = Join;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = js.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = io.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = unicode.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = utf8.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["time"] = (function() {
	var $pkg = {}, $init, errors, js, nosync, runtime, strings, syscall, ParseError, Time, Month, Weekday, Duration, Location, zone, zoneTrans, sliceType, sliceType$1, ptrType, sliceType$2, arrayType, sliceType$3, arrayType$1, arrayType$2, ptrType$1, arrayType$4, ptrType$3, ptrType$6, std0x, longDayNames, shortDayNames, shortMonthNames, longMonthNames, atoiError, errBad, errLeadingInt, months, days, daysBefore, utcLoc, utcLoc$24ptr, localLoc, localLoc$24ptr, localOnce, zoneinfo, badData, _tuple, _r, init, initLocal, startsWithLowerCase, nextStdChunk, match, lookup, appendInt, atoi, formatNano, quote, isDigit, getnum, cutspace, skip, Parse, parse, parseTimeZone, parseGMT, parseNanoseconds, leadingInt, absWeekday, absClock, fmtFrac, fmtInt, absDate, daysIn, Unix, isLeap, norm, Date, div, FixedZone;
	errors = $packages["errors"];
	js = $packages["github.com/gopherjs/gopherjs/js"];
	nosync = $packages["github.com/gopherjs/gopherjs/nosync"];
	runtime = $packages["runtime"];
	strings = $packages["strings"];
	syscall = $packages["syscall"];
	ParseError = $pkg.ParseError = $newType(0, $kindStruct, "time.ParseError", "ParseError", "time", function(Layout_, Value_, LayoutElem_, ValueElem_, Message_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Layout = "";
			this.Value = "";
			this.LayoutElem = "";
			this.ValueElem = "";
			this.Message = "";
			return;
		}
		this.Layout = Layout_;
		this.Value = Value_;
		this.LayoutElem = LayoutElem_;
		this.ValueElem = ValueElem_;
		this.Message = Message_;
	});
	Time = $pkg.Time = $newType(0, $kindStruct, "time.Time", "Time", "time", function(sec_, nsec_, loc_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.sec = new $Int64(0, 0);
			this.nsec = 0;
			this.loc = ptrType$1.nil;
			return;
		}
		this.sec = sec_;
		this.nsec = nsec_;
		this.loc = loc_;
	});
	Month = $pkg.Month = $newType(4, $kindInt, "time.Month", "Month", "time", null);
	Weekday = $pkg.Weekday = $newType(4, $kindInt, "time.Weekday", "Weekday", "time", null);
	Duration = $pkg.Duration = $newType(8, $kindInt64, "time.Duration", "Duration", "time", null);
	Location = $pkg.Location = $newType(0, $kindStruct, "time.Location", "Location", "time", function(name_, zone_, tx_, cacheStart_, cacheEnd_, cacheZone_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = "";
			this.zone = sliceType.nil;
			this.tx = sliceType$1.nil;
			this.cacheStart = new $Int64(0, 0);
			this.cacheEnd = new $Int64(0, 0);
			this.cacheZone = ptrType.nil;
			return;
		}
		this.name = name_;
		this.zone = zone_;
		this.tx = tx_;
		this.cacheStart = cacheStart_;
		this.cacheEnd = cacheEnd_;
		this.cacheZone = cacheZone_;
	});
	zone = $pkg.zone = $newType(0, $kindStruct, "time.zone", "zone", "time", function(name_, offset_, isDST_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = "";
			this.offset = 0;
			this.isDST = false;
			return;
		}
		this.name = name_;
		this.offset = offset_;
		this.isDST = isDST_;
	});
	zoneTrans = $pkg.zoneTrans = $newType(0, $kindStruct, "time.zoneTrans", "zoneTrans", "time", function(when_, index_, isstd_, isutc_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.when = new $Int64(0, 0);
			this.index = 0;
			this.isstd = false;
			this.isutc = false;
			return;
		}
		this.when = when_;
		this.index = index_;
		this.isstd = isstd_;
		this.isutc = isutc_;
	});
	sliceType = $sliceType(zone);
	sliceType$1 = $sliceType(zoneTrans);
	ptrType = $ptrType(zone);
	sliceType$2 = $sliceType($String);
	arrayType = $arrayType($Uint8, 20);
	sliceType$3 = $sliceType($Uint8);
	arrayType$1 = $arrayType($Uint8, 9);
	arrayType$2 = $arrayType($Uint8, 64);
	ptrType$1 = $ptrType(Location);
	arrayType$4 = $arrayType($Uint8, 32);
	ptrType$3 = $ptrType(ParseError);
	ptrType$6 = $ptrType(Time);
	init = function() {
		var $ptr;
		Unix(new $Int64(0, 0), new $Int64(0, 0));
	};
	initLocal = function() {
		var $ptr, d, i, j, s;
		d = new ($global.Date)();
		s = $internalize(d, $String);
		i = strings.IndexByte(s, 40);
		j = strings.IndexByte(s, 41);
		if ((i === -1) || (j === -1)) {
			localLoc.name = "UTC";
			return;
		}
		localLoc.name = s.substring((i + 1 >> 0), j);
		localLoc.zone = new sliceType([new zone.ptr(localLoc.name, $imul(($parseInt(d.getTimezoneOffset()) >> 0), -60), false)]);
	};
	startsWithLowerCase = function(str) {
		var $ptr, c, str;
		if (str.length === 0) {
			return false;
		}
		c = str.charCodeAt(0);
		return 97 <= c && c <= 122;
	};
	nextStdChunk = function(layout) {
		var $ptr, _1, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$12, _tmp$13, _tmp$14, _tmp$15, _tmp$16, _tmp$17, _tmp$18, _tmp$19, _tmp$2, _tmp$20, _tmp$21, _tmp$22, _tmp$23, _tmp$24, _tmp$25, _tmp$26, _tmp$27, _tmp$28, _tmp$29, _tmp$3, _tmp$30, _tmp$31, _tmp$32, _tmp$33, _tmp$34, _tmp$35, _tmp$36, _tmp$37, _tmp$38, _tmp$39, _tmp$4, _tmp$40, _tmp$41, _tmp$42, _tmp$43, _tmp$44, _tmp$45, _tmp$46, _tmp$47, _tmp$48, _tmp$49, _tmp$5, _tmp$50, _tmp$51, _tmp$52, _tmp$53, _tmp$54, _tmp$55, _tmp$56, _tmp$57, _tmp$58, _tmp$59, _tmp$6, _tmp$60, _tmp$61, _tmp$62, _tmp$63, _tmp$64, _tmp$65, _tmp$66, _tmp$67, _tmp$68, _tmp$69, _tmp$7, _tmp$70, _tmp$71, _tmp$72, _tmp$73, _tmp$74, _tmp$75, _tmp$76, _tmp$77, _tmp$78, _tmp$79, _tmp$8, _tmp$80, _tmp$81, _tmp$82, _tmp$83, _tmp$84, _tmp$85, _tmp$86, _tmp$9, c, ch, i, j, layout, prefix, std, std$1, suffix, x;
		prefix = "";
		std = 0;
		suffix = "";
		i = 0;
		while (true) {
			if (!(i < layout.length)) { break; }
			c = (layout.charCodeAt(i) >> 0);
			_1 = c;
			if (_1 === (74)) {
				if (layout.length >= (i + 3 >> 0) && layout.substring(i, (i + 3 >> 0)) === "Jan") {
					if (layout.length >= (i + 7 >> 0) && layout.substring(i, (i + 7 >> 0)) === "January") {
						_tmp = layout.substring(0, i);
						_tmp$1 = 257;
						_tmp$2 = layout.substring((i + 7 >> 0));
						prefix = _tmp;
						std = _tmp$1;
						suffix = _tmp$2;
						return [prefix, std, suffix];
					}
					if (!startsWithLowerCase(layout.substring((i + 3 >> 0)))) {
						_tmp$3 = layout.substring(0, i);
						_tmp$4 = 258;
						_tmp$5 = layout.substring((i + 3 >> 0));
						prefix = _tmp$3;
						std = _tmp$4;
						suffix = _tmp$5;
						return [prefix, std, suffix];
					}
				}
			} else if (_1 === (77)) {
				if (layout.length >= (i + 3 >> 0)) {
					if (layout.substring(i, (i + 3 >> 0)) === "Mon") {
						if (layout.length >= (i + 6 >> 0) && layout.substring(i, (i + 6 >> 0)) === "Monday") {
							_tmp$6 = layout.substring(0, i);
							_tmp$7 = 261;
							_tmp$8 = layout.substring((i + 6 >> 0));
							prefix = _tmp$6;
							std = _tmp$7;
							suffix = _tmp$8;
							return [prefix, std, suffix];
						}
						if (!startsWithLowerCase(layout.substring((i + 3 >> 0)))) {
							_tmp$9 = layout.substring(0, i);
							_tmp$10 = 262;
							_tmp$11 = layout.substring((i + 3 >> 0));
							prefix = _tmp$9;
							std = _tmp$10;
							suffix = _tmp$11;
							return [prefix, std, suffix];
						}
					}
					if (layout.substring(i, (i + 3 >> 0)) === "MST") {
						_tmp$12 = layout.substring(0, i);
						_tmp$13 = 21;
						_tmp$14 = layout.substring((i + 3 >> 0));
						prefix = _tmp$12;
						std = _tmp$13;
						suffix = _tmp$14;
						return [prefix, std, suffix];
					}
				}
			} else if (_1 === (48)) {
				if (layout.length >= (i + 2 >> 0) && 49 <= layout.charCodeAt((i + 1 >> 0)) && layout.charCodeAt((i + 1 >> 0)) <= 54) {
					_tmp$15 = layout.substring(0, i);
					_tmp$16 = (x = layout.charCodeAt((i + 1 >> 0)) - 49 << 24 >>> 24, ((x < 0 || x >= std0x.length) ? $throwRuntimeError("index out of range") : std0x[x]));
					_tmp$17 = layout.substring((i + 2 >> 0));
					prefix = _tmp$15;
					std = _tmp$16;
					suffix = _tmp$17;
					return [prefix, std, suffix];
				}
			} else if (_1 === (49)) {
				if (layout.length >= (i + 2 >> 0) && (layout.charCodeAt((i + 1 >> 0)) === 53)) {
					_tmp$18 = layout.substring(0, i);
					_tmp$19 = 522;
					_tmp$20 = layout.substring((i + 2 >> 0));
					prefix = _tmp$18;
					std = _tmp$19;
					suffix = _tmp$20;
					return [prefix, std, suffix];
				}
				_tmp$21 = layout.substring(0, i);
				_tmp$22 = 259;
				_tmp$23 = layout.substring((i + 1 >> 0));
				prefix = _tmp$21;
				std = _tmp$22;
				suffix = _tmp$23;
				return [prefix, std, suffix];
			} else if (_1 === (50)) {
				if (layout.length >= (i + 4 >> 0) && layout.substring(i, (i + 4 >> 0)) === "2006") {
					_tmp$24 = layout.substring(0, i);
					_tmp$25 = 273;
					_tmp$26 = layout.substring((i + 4 >> 0));
					prefix = _tmp$24;
					std = _tmp$25;
					suffix = _tmp$26;
					return [prefix, std, suffix];
				}
				_tmp$27 = layout.substring(0, i);
				_tmp$28 = 263;
				_tmp$29 = layout.substring((i + 1 >> 0));
				prefix = _tmp$27;
				std = _tmp$28;
				suffix = _tmp$29;
				return [prefix, std, suffix];
			} else if (_1 === (95)) {
				if (layout.length >= (i + 2 >> 0) && (layout.charCodeAt((i + 1 >> 0)) === 50)) {
					if (layout.length >= (i + 5 >> 0) && layout.substring((i + 1 >> 0), (i + 5 >> 0)) === "2006") {
						_tmp$30 = layout.substring(0, (i + 1 >> 0));
						_tmp$31 = 273;
						_tmp$32 = layout.substring((i + 5 >> 0));
						prefix = _tmp$30;
						std = _tmp$31;
						suffix = _tmp$32;
						return [prefix, std, suffix];
					}
					_tmp$33 = layout.substring(0, i);
					_tmp$34 = 264;
					_tmp$35 = layout.substring((i + 2 >> 0));
					prefix = _tmp$33;
					std = _tmp$34;
					suffix = _tmp$35;
					return [prefix, std, suffix];
				}
			} else if (_1 === (51)) {
				_tmp$36 = layout.substring(0, i);
				_tmp$37 = 523;
				_tmp$38 = layout.substring((i + 1 >> 0));
				prefix = _tmp$36;
				std = _tmp$37;
				suffix = _tmp$38;
				return [prefix, std, suffix];
			} else if (_1 === (52)) {
				_tmp$39 = layout.substring(0, i);
				_tmp$40 = 525;
				_tmp$41 = layout.substring((i + 1 >> 0));
				prefix = _tmp$39;
				std = _tmp$40;
				suffix = _tmp$41;
				return [prefix, std, suffix];
			} else if (_1 === (53)) {
				_tmp$42 = layout.substring(0, i);
				_tmp$43 = 527;
				_tmp$44 = layout.substring((i + 1 >> 0));
				prefix = _tmp$42;
				std = _tmp$43;
				suffix = _tmp$44;
				return [prefix, std, suffix];
			} else if (_1 === (80)) {
				if (layout.length >= (i + 2 >> 0) && (layout.charCodeAt((i + 1 >> 0)) === 77)) {
					_tmp$45 = layout.substring(0, i);
					_tmp$46 = 531;
					_tmp$47 = layout.substring((i + 2 >> 0));
					prefix = _tmp$45;
					std = _tmp$46;
					suffix = _tmp$47;
					return [prefix, std, suffix];
				}
			} else if (_1 === (112)) {
				if (layout.length >= (i + 2 >> 0) && (layout.charCodeAt((i + 1 >> 0)) === 109)) {
					_tmp$48 = layout.substring(0, i);
					_tmp$49 = 532;
					_tmp$50 = layout.substring((i + 2 >> 0));
					prefix = _tmp$48;
					std = _tmp$49;
					suffix = _tmp$50;
					return [prefix, std, suffix];
				}
			} else if (_1 === (45)) {
				if (layout.length >= (i + 7 >> 0) && layout.substring(i, (i + 7 >> 0)) === "-070000") {
					_tmp$51 = layout.substring(0, i);
					_tmp$52 = 28;
					_tmp$53 = layout.substring((i + 7 >> 0));
					prefix = _tmp$51;
					std = _tmp$52;
					suffix = _tmp$53;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 9 >> 0) && layout.substring(i, (i + 9 >> 0)) === "-07:00:00") {
					_tmp$54 = layout.substring(0, i);
					_tmp$55 = 31;
					_tmp$56 = layout.substring((i + 9 >> 0));
					prefix = _tmp$54;
					std = _tmp$55;
					suffix = _tmp$56;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 5 >> 0) && layout.substring(i, (i + 5 >> 0)) === "-0700") {
					_tmp$57 = layout.substring(0, i);
					_tmp$58 = 27;
					_tmp$59 = layout.substring((i + 5 >> 0));
					prefix = _tmp$57;
					std = _tmp$58;
					suffix = _tmp$59;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 6 >> 0) && layout.substring(i, (i + 6 >> 0)) === "-07:00") {
					_tmp$60 = layout.substring(0, i);
					_tmp$61 = 30;
					_tmp$62 = layout.substring((i + 6 >> 0));
					prefix = _tmp$60;
					std = _tmp$61;
					suffix = _tmp$62;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 3 >> 0) && layout.substring(i, (i + 3 >> 0)) === "-07") {
					_tmp$63 = layout.substring(0, i);
					_tmp$64 = 29;
					_tmp$65 = layout.substring((i + 3 >> 0));
					prefix = _tmp$63;
					std = _tmp$64;
					suffix = _tmp$65;
					return [prefix, std, suffix];
				}
			} else if (_1 === (90)) {
				if (layout.length >= (i + 7 >> 0) && layout.substring(i, (i + 7 >> 0)) === "Z070000") {
					_tmp$66 = layout.substring(0, i);
					_tmp$67 = 23;
					_tmp$68 = layout.substring((i + 7 >> 0));
					prefix = _tmp$66;
					std = _tmp$67;
					suffix = _tmp$68;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 9 >> 0) && layout.substring(i, (i + 9 >> 0)) === "Z07:00:00") {
					_tmp$69 = layout.substring(0, i);
					_tmp$70 = 26;
					_tmp$71 = layout.substring((i + 9 >> 0));
					prefix = _tmp$69;
					std = _tmp$70;
					suffix = _tmp$71;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 5 >> 0) && layout.substring(i, (i + 5 >> 0)) === "Z0700") {
					_tmp$72 = layout.substring(0, i);
					_tmp$73 = 22;
					_tmp$74 = layout.substring((i + 5 >> 0));
					prefix = _tmp$72;
					std = _tmp$73;
					suffix = _tmp$74;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 6 >> 0) && layout.substring(i, (i + 6 >> 0)) === "Z07:00") {
					_tmp$75 = layout.substring(0, i);
					_tmp$76 = 25;
					_tmp$77 = layout.substring((i + 6 >> 0));
					prefix = _tmp$75;
					std = _tmp$76;
					suffix = _tmp$77;
					return [prefix, std, suffix];
				}
				if (layout.length >= (i + 3 >> 0) && layout.substring(i, (i + 3 >> 0)) === "Z07") {
					_tmp$78 = layout.substring(0, i);
					_tmp$79 = 24;
					_tmp$80 = layout.substring((i + 3 >> 0));
					prefix = _tmp$78;
					std = _tmp$79;
					suffix = _tmp$80;
					return [prefix, std, suffix];
				}
			} else if (_1 === (46)) {
				if ((i + 1 >> 0) < layout.length && ((layout.charCodeAt((i + 1 >> 0)) === 48) || (layout.charCodeAt((i + 1 >> 0)) === 57))) {
					ch = layout.charCodeAt((i + 1 >> 0));
					j = i + 1 >> 0;
					while (true) {
						if (!(j < layout.length && (layout.charCodeAt(j) === ch))) { break; }
						j = j + (1) >> 0;
					}
					if (!isDigit(layout, j)) {
						std$1 = 32;
						if (layout.charCodeAt((i + 1 >> 0)) === 57) {
							std$1 = 33;
						}
						std$1 = std$1 | ((((j - ((i + 1 >> 0)) >> 0)) << 16 >> 0));
						_tmp$81 = layout.substring(0, i);
						_tmp$82 = std$1;
						_tmp$83 = layout.substring(j);
						prefix = _tmp$81;
						std = _tmp$82;
						suffix = _tmp$83;
						return [prefix, std, suffix];
					}
				}
			}
			i = i + (1) >> 0;
		}
		_tmp$84 = layout;
		_tmp$85 = 0;
		_tmp$86 = "";
		prefix = _tmp$84;
		std = _tmp$85;
		suffix = _tmp$86;
		return [prefix, std, suffix];
	};
	match = function(s1, s2) {
		var $ptr, c1, c2, i, s1, s2;
		i = 0;
		while (true) {
			if (!(i < s1.length)) { break; }
			c1 = s1.charCodeAt(i);
			c2 = s2.charCodeAt(i);
			if (!((c1 === c2))) {
				c1 = (c1 | (32)) >>> 0;
				c2 = (c2 | (32)) >>> 0;
				if (!((c1 === c2)) || c1 < 97 || c1 > 122) {
					return false;
				}
			}
			i = i + (1) >> 0;
		}
		return true;
	};
	lookup = function(tab, val) {
		var $ptr, _i, _ref, i, tab, v, val;
		_ref = tab;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			v = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			if (val.length >= v.length && match(val.substring(0, v.length), v)) {
				return [i, val.substring(v.length), $ifaceNil];
			}
			_i++;
		}
		return [-1, val, errBad];
	};
	appendInt = function(b, x, width) {
		var $ptr, _q, b, buf, i, q, u, w, width, x;
		u = (x >>> 0);
		if (x < 0) {
			b = $append(b, 45);
			u = (-x >>> 0);
		}
		buf = arrayType.zero();
		i = 20;
		while (true) {
			if (!(u >= 10)) { break; }
			i = i - (1) >> 0;
			q = (_q = u / 10, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
			((i < 0 || i >= buf.length) ? $throwRuntimeError("index out of range") : buf[i] = (((48 + u >>> 0) - (q * 10 >>> 0) >>> 0) << 24 >>> 24));
			u = q;
		}
		i = i - (1) >> 0;
		((i < 0 || i >= buf.length) ? $throwRuntimeError("index out of range") : buf[i] = ((48 + u >>> 0) << 24 >>> 24));
		w = 20 - i >> 0;
		while (true) {
			if (!(w < width)) { break; }
			b = $append(b, 48);
			w = w + (1) >> 0;
		}
		return $appendSlice(b, $subslice(new sliceType$3(buf), i));
	};
	atoi = function(s) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tuple$1, err, neg, q, rem, s, x;
		x = 0;
		err = $ifaceNil;
		neg = false;
		if (!(s === "") && ((s.charCodeAt(0) === 45) || (s.charCodeAt(0) === 43))) {
			neg = s.charCodeAt(0) === 45;
			s = s.substring(1);
		}
		_tuple$1 = leadingInt(s);
		q = _tuple$1[0];
		rem = _tuple$1[1];
		err = _tuple$1[2];
		x = ((q.$low + ((q.$high >> 31) * 4294967296)) >> 0);
		if (!($interfaceIsEqual(err, $ifaceNil)) || !(rem === "")) {
			_tmp = 0;
			_tmp$1 = atoiError;
			x = _tmp;
			err = _tmp$1;
			return [x, err];
		}
		if (neg) {
			x = -x;
		}
		_tmp$2 = x;
		_tmp$3 = $ifaceNil;
		x = _tmp$2;
		err = _tmp$3;
		return [x, err];
	};
	formatNano = function(b, nanosec, n, trim) {
		var $ptr, _q, _r$1, b, buf, n, nanosec, start, trim, u, x;
		u = nanosec;
		buf = arrayType$1.zero();
		start = 9;
		while (true) {
			if (!(start > 0)) { break; }
			start = start - (1) >> 0;
			((start < 0 || start >= buf.length) ? $throwRuntimeError("index out of range") : buf[start] = (((_r$1 = u % 10, _r$1 === _r$1 ? _r$1 : $throwRuntimeError("integer divide by zero")) + 48 >>> 0) << 24 >>> 24));
			u = (_q = u / (10), (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
		}
		if (n > 9) {
			n = 9;
		}
		if (trim) {
			while (true) {
				if (!(n > 0 && ((x = n - 1 >> 0, ((x < 0 || x >= buf.length) ? $throwRuntimeError("index out of range") : buf[x])) === 48))) { break; }
				n = n - (1) >> 0;
			}
			if (n === 0) {
				return b;
			}
		}
		b = $append(b, 46);
		return $appendSlice(b, $subslice(new sliceType$3(buf), 0, n));
	};
	Time.ptr.prototype.String = function() {
		var $ptr, _r$1, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.Format("2006-01-02 15:04:05.999999999 -0700 MST"); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.String }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.String = function() { return this.$val.String(); };
	Time.ptr.prototype.Format = function(layout) {
		var $ptr, _r$1, b, buf, layout, max, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; b = $f.b; buf = $f.buf; layout = $f.layout; max = $f.max; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		b = sliceType$3.nil;
		max = layout.length + 10 >> 0;
		if (max < 64) {
			buf = arrayType$2.zero();
			b = $subslice(new sliceType$3(buf), 0, 0);
		} else {
			b = $makeSlice(sliceType$3, 0, max);
		}
		_r$1 = t.AppendFormat(b, layout); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		b = _r$1;
		return $bytesToString(b);
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Format }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.b = b; $f.buf = buf; $f.layout = layout; $f.max = max; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Format = function(layout) { return this.$val.Format(layout); };
	Time.ptr.prototype.AppendFormat = function(b, layout) {
		var $ptr, _2, _q, _q$1, _q$2, _q$3, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, _tuple$1, _tuple$2, _tuple$3, _tuple$4, abs, absoffset, b, day, hour, hr, hr$1, layout, m, min, month, name, offset, prefix, s, sec, std, suffix, t, y, year, zone$1, zone$2, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _2 = $f._2; _q = $f._q; _q$1 = $f._q$1; _q$2 = $f._q$2; _q$3 = $f._q$3; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _tuple$1 = $f._tuple$1; _tuple$2 = $f._tuple$2; _tuple$3 = $f._tuple$3; _tuple$4 = $f._tuple$4; abs = $f.abs; absoffset = $f.absoffset; b = $f.b; day = $f.day; hour = $f.hour; hr = $f.hr; hr$1 = $f.hr$1; layout = $f.layout; m = $f.m; min = $f.min; month = $f.month; name = $f.name; offset = $f.offset; prefix = $f.prefix; s = $f.s; sec = $f.sec; std = $f.std; suffix = $f.suffix; t = $f.t; y = $f.y; year = $f.year; zone$1 = $f.zone$1; zone$2 = $f.zone$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.locabs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		name = _tuple$1[0];
		offset = _tuple$1[1];
		abs = _tuple$1[2];
		year = -1;
		month = 0;
		day = 0;
		hour = -1;
		min = 0;
		sec = 0;
		while (true) {
			if (!(!(layout === ""))) { break; }
			_tuple$2 = nextStdChunk(layout);
			prefix = _tuple$2[0];
			std = _tuple$2[1];
			suffix = _tuple$2[2];
			if (!(prefix === "")) {
				b = $appendSlice(b, prefix);
			}
			if (std === 0) {
				break;
			}
			layout = suffix;
			if (year < 0 && !(((std & 256) === 0))) {
				_tuple$3 = absDate(abs, true);
				year = _tuple$3[0];
				month = _tuple$3[1];
				day = _tuple$3[2];
			}
			if (hour < 0 && !(((std & 512) === 0))) {
				_tuple$4 = absClock(abs);
				hour = _tuple$4[0];
				min = _tuple$4[1];
				sec = _tuple$4[2];
			}
			switch (0) { default:
				_2 = std & 65535;
				if (_2 === (274)) {
					y = year;
					if (y < 0) {
						y = -y;
					}
					b = appendInt(b, (_r$2 = y % 100, _r$2 === _r$2 ? _r$2 : $throwRuntimeError("integer divide by zero")), 2);
				} else if (_2 === (273)) {
					b = appendInt(b, year, 4);
				} else if (_2 === (258)) {
					b = $appendSlice(b, new Month(month).String().substring(0, 3));
				} else if (_2 === (257)) {
					m = new Month(month).String();
					b = $appendSlice(b, m);
				} else if (_2 === (259)) {
					b = appendInt(b, (month >> 0), 0);
				} else if (_2 === (260)) {
					b = appendInt(b, (month >> 0), 2);
				} else if (_2 === (262)) {
					b = $appendSlice(b, new Weekday(absWeekday(abs)).String().substring(0, 3));
				} else if (_2 === (261)) {
					s = new Weekday(absWeekday(abs)).String();
					b = $appendSlice(b, s);
				} else if (_2 === (263)) {
					b = appendInt(b, day, 0);
				} else if (_2 === (264)) {
					if (day < 10) {
						b = $append(b, 32);
					}
					b = appendInt(b, day, 0);
				} else if (_2 === (265)) {
					b = appendInt(b, day, 2);
				} else if (_2 === (522)) {
					b = appendInt(b, hour, 2);
				} else if (_2 === (523)) {
					hr = (_r$3 = hour % 12, _r$3 === _r$3 ? _r$3 : $throwRuntimeError("integer divide by zero"));
					if (hr === 0) {
						hr = 12;
					}
					b = appendInt(b, hr, 0);
				} else if (_2 === (524)) {
					hr$1 = (_r$4 = hour % 12, _r$4 === _r$4 ? _r$4 : $throwRuntimeError("integer divide by zero"));
					if (hr$1 === 0) {
						hr$1 = 12;
					}
					b = appendInt(b, hr$1, 2);
				} else if (_2 === (525)) {
					b = appendInt(b, min, 0);
				} else if (_2 === (526)) {
					b = appendInt(b, min, 2);
				} else if (_2 === (527)) {
					b = appendInt(b, sec, 0);
				} else if (_2 === (528)) {
					b = appendInt(b, sec, 2);
				} else if (_2 === (531)) {
					if (hour >= 12) {
						b = $appendSlice(b, "PM");
					} else {
						b = $appendSlice(b, "AM");
					}
				} else if (_2 === (532)) {
					if (hour >= 12) {
						b = $appendSlice(b, "pm");
					} else {
						b = $appendSlice(b, "am");
					}
				} else if ((_2 === (22)) || (_2 === (25)) || (_2 === (23)) || (_2 === (24)) || (_2 === (26)) || (_2 === (27)) || (_2 === (30)) || (_2 === (28)) || (_2 === (29)) || (_2 === (31))) {
					if ((offset === 0) && ((std === 22) || (std === 25) || (std === 23) || (std === 24) || (std === 26))) {
						b = $append(b, 90);
						break;
					}
					zone$1 = (_q = offset / 60, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
					absoffset = offset;
					if (zone$1 < 0) {
						b = $append(b, 45);
						zone$1 = -zone$1;
						absoffset = -absoffset;
					} else {
						b = $append(b, 43);
					}
					b = appendInt(b, (_q$1 = zone$1 / 60, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >> 0 : $throwRuntimeError("integer divide by zero")), 2);
					if ((std === 25) || (std === 30) || (std === 26) || (std === 31)) {
						b = $append(b, 58);
					}
					if (!((std === 29)) && !((std === 24))) {
						b = appendInt(b, (_r$5 = zone$1 % 60, _r$5 === _r$5 ? _r$5 : $throwRuntimeError("integer divide by zero")), 2);
					}
					if ((std === 23) || (std === 28) || (std === 31) || (std === 26)) {
						if ((std === 31) || (std === 26)) {
							b = $append(b, 58);
						}
						b = appendInt(b, (_r$6 = absoffset % 60, _r$6 === _r$6 ? _r$6 : $throwRuntimeError("integer divide by zero")), 2);
					}
				} else if (_2 === (21)) {
					if (!(name === "")) {
						b = $appendSlice(b, name);
						break;
					}
					zone$2 = (_q$2 = offset / 60, (_q$2 === _q$2 && _q$2 !== 1/0 && _q$2 !== -1/0) ? _q$2 >> 0 : $throwRuntimeError("integer divide by zero"));
					if (zone$2 < 0) {
						b = $append(b, 45);
						zone$2 = -zone$2;
					} else {
						b = $append(b, 43);
					}
					b = appendInt(b, (_q$3 = zone$2 / 60, (_q$3 === _q$3 && _q$3 !== 1/0 && _q$3 !== -1/0) ? _q$3 >> 0 : $throwRuntimeError("integer divide by zero")), 2);
					b = appendInt(b, (_r$7 = zone$2 % 60, _r$7 === _r$7 ? _r$7 : $throwRuntimeError("integer divide by zero")), 2);
				} else if ((_2 === (32)) || (_2 === (33))) {
					b = formatNano(b, (t.Nanosecond() >>> 0), std >> 16 >> 0, (std & 65535) === 33);
				}
			}
		}
		return b;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.AppendFormat }; } $f.$ptr = $ptr; $f._2 = _2; $f._q = _q; $f._q$1 = _q$1; $f._q$2 = _q$2; $f._q$3 = _q$3; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._tuple$1 = _tuple$1; $f._tuple$2 = _tuple$2; $f._tuple$3 = _tuple$3; $f._tuple$4 = _tuple$4; $f.abs = abs; $f.absoffset = absoffset; $f.b = b; $f.day = day; $f.hour = hour; $f.hr = hr; $f.hr$1 = hr$1; $f.layout = layout; $f.m = m; $f.min = min; $f.month = month; $f.name = name; $f.offset = offset; $f.prefix = prefix; $f.s = s; $f.sec = sec; $f.std = std; $f.suffix = suffix; $f.t = t; $f.y = y; $f.year = year; $f.zone$1 = zone$1; $f.zone$2 = zone$2; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.AppendFormat = function(b, layout) { return this.$val.AppendFormat(b, layout); };
	quote = function(s) {
		var $ptr, s;
		return "\"" + s + "\"";
	};
	ParseError.ptr.prototype.Error = function() {
		var $ptr, e;
		e = this;
		if (e.Message === "") {
			return "parsing time " + quote(e.Value) + " as " + quote(e.Layout) + ": cannot parse " + quote(e.ValueElem) + " as " + quote(e.LayoutElem);
		}
		return "parsing time " + quote(e.Value) + e.Message;
	};
	ParseError.prototype.Error = function() { return this.$val.Error(); };
	isDigit = function(s, i) {
		var $ptr, c, i, s;
		if (s.length <= i) {
			return false;
		}
		c = s.charCodeAt(i);
		return 48 <= c && c <= 57;
	};
	getnum = function(s, fixed) {
		var $ptr, fixed, s;
		if (!isDigit(s, 0)) {
			return [0, s, errBad];
		}
		if (!isDigit(s, 1)) {
			if (fixed) {
				return [0, s, errBad];
			}
			return [((s.charCodeAt(0) - 48 << 24 >>> 24) >> 0), s.substring(1), $ifaceNil];
		}
		return [($imul(((s.charCodeAt(0) - 48 << 24 >>> 24) >> 0), 10)) + ((s.charCodeAt(1) - 48 << 24 >>> 24) >> 0) >> 0, s.substring(2), $ifaceNil];
	};
	cutspace = function(s) {
		var $ptr, s;
		while (true) {
			if (!(s.length > 0 && (s.charCodeAt(0) === 32))) { break; }
			s = s.substring(1);
		}
		return s;
	};
	skip = function(value, prefix) {
		var $ptr, prefix, value;
		while (true) {
			if (!(prefix.length > 0)) { break; }
			if (prefix.charCodeAt(0) === 32) {
				if (value.length > 0 && !((value.charCodeAt(0) === 32))) {
					return [value, errBad];
				}
				prefix = cutspace(prefix);
				value = cutspace(value);
				continue;
			}
			if ((value.length === 0) || !((value.charCodeAt(0) === prefix.charCodeAt(0)))) {
				return [value, errBad];
			}
			prefix = prefix.substring(1);
			value = value.substring(1);
		}
		return [value, $ifaceNil];
	};
	Parse = function(layout, value) {
		var $ptr, _r$1, layout, value, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; layout = $f.layout; value = $f.value; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r$1 = parse(layout, value, $pkg.UTC, $pkg.Local); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Parse }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.layout = layout; $f.value = value; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Parse = Parse;
	parse = function(layout, value, defaultLocation, local) {
		var $ptr, _3, _4, _5, _6, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$12, _tmp$13, _tmp$14, _tmp$15, _tmp$16, _tmp$17, _tmp$18, _tmp$19, _tmp$2, _tmp$20, _tmp$21, _tmp$22, _tmp$23, _tmp$24, _tmp$25, _tmp$26, _tmp$27, _tmp$28, _tmp$29, _tmp$3, _tmp$30, _tmp$31, _tmp$32, _tmp$33, _tmp$34, _tmp$35, _tmp$36, _tmp$37, _tmp$38, _tmp$39, _tmp$4, _tmp$40, _tmp$41, _tmp$42, _tmp$43, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, _tuple$1, _tuple$10, _tuple$11, _tuple$12, _tuple$13, _tuple$14, _tuple$15, _tuple$16, _tuple$17, _tuple$18, _tuple$19, _tuple$2, _tuple$20, _tuple$21, _tuple$22, _tuple$23, _tuple$24, _tuple$25, _tuple$3, _tuple$4, _tuple$5, _tuple$6, _tuple$7, _tuple$8, _tuple$9, alayout, amSet, avalue, day, defaultLocation, err, hour, hour$1, hr, i, layout, local, min, min$1, mm, month, n, n$1, name, ndigit, nsec, offset, offset$1, ok, ok$1, p, pmSet, prefix, rangeErrString, sec, seconds, sign, ss, std, stdstr, suffix, t, t$1, value, x, x$1, x$2, x$3, x$4, x$5, year, z, zoneName, zoneOffset, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _3 = $f._3; _4 = $f._4; _5 = $f._5; _6 = $f._6; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$10 = $f._tmp$10; _tmp$11 = $f._tmp$11; _tmp$12 = $f._tmp$12; _tmp$13 = $f._tmp$13; _tmp$14 = $f._tmp$14; _tmp$15 = $f._tmp$15; _tmp$16 = $f._tmp$16; _tmp$17 = $f._tmp$17; _tmp$18 = $f._tmp$18; _tmp$19 = $f._tmp$19; _tmp$2 = $f._tmp$2; _tmp$20 = $f._tmp$20; _tmp$21 = $f._tmp$21; _tmp$22 = $f._tmp$22; _tmp$23 = $f._tmp$23; _tmp$24 = $f._tmp$24; _tmp$25 = $f._tmp$25; _tmp$26 = $f._tmp$26; _tmp$27 = $f._tmp$27; _tmp$28 = $f._tmp$28; _tmp$29 = $f._tmp$29; _tmp$3 = $f._tmp$3; _tmp$30 = $f._tmp$30; _tmp$31 = $f._tmp$31; _tmp$32 = $f._tmp$32; _tmp$33 = $f._tmp$33; _tmp$34 = $f._tmp$34; _tmp$35 = $f._tmp$35; _tmp$36 = $f._tmp$36; _tmp$37 = $f._tmp$37; _tmp$38 = $f._tmp$38; _tmp$39 = $f._tmp$39; _tmp$4 = $f._tmp$4; _tmp$40 = $f._tmp$40; _tmp$41 = $f._tmp$41; _tmp$42 = $f._tmp$42; _tmp$43 = $f._tmp$43; _tmp$5 = $f._tmp$5; _tmp$6 = $f._tmp$6; _tmp$7 = $f._tmp$7; _tmp$8 = $f._tmp$8; _tmp$9 = $f._tmp$9; _tuple$1 = $f._tuple$1; _tuple$10 = $f._tuple$10; _tuple$11 = $f._tuple$11; _tuple$12 = $f._tuple$12; _tuple$13 = $f._tuple$13; _tuple$14 = $f._tuple$14; _tuple$15 = $f._tuple$15; _tuple$16 = $f._tuple$16; _tuple$17 = $f._tuple$17; _tuple$18 = $f._tuple$18; _tuple$19 = $f._tuple$19; _tuple$2 = $f._tuple$2; _tuple$20 = $f._tuple$20; _tuple$21 = $f._tuple$21; _tuple$22 = $f._tuple$22; _tuple$23 = $f._tuple$23; _tuple$24 = $f._tuple$24; _tuple$25 = $f._tuple$25; _tuple$3 = $f._tuple$3; _tuple$4 = $f._tuple$4; _tuple$5 = $f._tuple$5; _tuple$6 = $f._tuple$6; _tuple$7 = $f._tuple$7; _tuple$8 = $f._tuple$8; _tuple$9 = $f._tuple$9; alayout = $f.alayout; amSet = $f.amSet; avalue = $f.avalue; day = $f.day; defaultLocation = $f.defaultLocation; err = $f.err; hour = $f.hour; hour$1 = $f.hour$1; hr = $f.hr; i = $f.i; layout = $f.layout; local = $f.local; min = $f.min; min$1 = $f.min$1; mm = $f.mm; month = $f.month; n = $f.n; n$1 = $f.n$1; name = $f.name; ndigit = $f.ndigit; nsec = $f.nsec; offset = $f.offset; offset$1 = $f.offset$1; ok = $f.ok; ok$1 = $f.ok$1; p = $f.p; pmSet = $f.pmSet; prefix = $f.prefix; rangeErrString = $f.rangeErrString; sec = $f.sec; seconds = $f.seconds; sign = $f.sign; ss = $f.ss; std = $f.std; stdstr = $f.stdstr; suffix = $f.suffix; t = $f.t; t$1 = $f.t$1; value = $f.value; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; year = $f.year; z = $f.z; zoneName = $f.zoneName; zoneOffset = $f.zoneOffset; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_tmp = layout;
		_tmp$1 = value;
		alayout = _tmp;
		avalue = _tmp$1;
		rangeErrString = "";
		amSet = false;
		pmSet = false;
		year = 0;
		month = 1;
		day = 1;
		hour = 0;
		min = 0;
		sec = 0;
		nsec = 0;
		z = ptrType$1.nil;
		zoneOffset = -1;
		zoneName = "";
		while (true) {
			err = $ifaceNil;
			_tuple$1 = nextStdChunk(layout);
			prefix = _tuple$1[0];
			std = _tuple$1[1];
			suffix = _tuple$1[2];
			stdstr = layout.substring(prefix.length, (layout.length - suffix.length >> 0));
			_tuple$2 = skip(value, prefix);
			value = _tuple$2[0];
			err = _tuple$2[1];
			if (!($interfaceIsEqual(err, $ifaceNil))) {
				return [new Time.ptr(new $Int64(0, 0), 0, ptrType$1.nil), new ParseError.ptr(alayout, avalue, prefix, value, "")];
			}
			if (std === 0) {
				if (!((value.length === 0))) {
					return [new Time.ptr(new $Int64(0, 0), 0, ptrType$1.nil), new ParseError.ptr(alayout, avalue, "", value, ": extra text: " + value)];
				}
				break;
			}
			layout = suffix;
			p = "";
			switch (0) { default:
				_3 = std & 65535;
				if (_3 === (274)) {
					if (value.length < 2) {
						err = errBad;
						break;
					}
					_tmp$2 = value.substring(0, 2);
					_tmp$3 = value.substring(2);
					p = _tmp$2;
					value = _tmp$3;
					_tuple$3 = atoi(p);
					year = _tuple$3[0];
					err = _tuple$3[1];
					if (year >= 69) {
						year = year + (1900) >> 0;
					} else {
						year = year + (2000) >> 0;
					}
				} else if (_3 === (273)) {
					if (value.length < 4 || !isDigit(value, 0)) {
						err = errBad;
						break;
					}
					_tmp$4 = value.substring(0, 4);
					_tmp$5 = value.substring(4);
					p = _tmp$4;
					value = _tmp$5;
					_tuple$4 = atoi(p);
					year = _tuple$4[0];
					err = _tuple$4[1];
				} else if (_3 === (258)) {
					_tuple$5 = lookup(shortMonthNames, value);
					month = _tuple$5[0];
					value = _tuple$5[1];
					err = _tuple$5[2];
				} else if (_3 === (257)) {
					_tuple$6 = lookup(longMonthNames, value);
					month = _tuple$6[0];
					value = _tuple$6[1];
					err = _tuple$6[2];
				} else if ((_3 === (259)) || (_3 === (260))) {
					_tuple$7 = getnum(value, std === 260);
					month = _tuple$7[0];
					value = _tuple$7[1];
					err = _tuple$7[2];
					if (month <= 0 || 12 < month) {
						rangeErrString = "month";
					}
				} else if (_3 === (262)) {
					_tuple$8 = lookup(shortDayNames, value);
					value = _tuple$8[1];
					err = _tuple$8[2];
				} else if (_3 === (261)) {
					_tuple$9 = lookup(longDayNames, value);
					value = _tuple$9[1];
					err = _tuple$9[2];
				} else if ((_3 === (263)) || (_3 === (264)) || (_3 === (265))) {
					if ((std === 264) && value.length > 0 && (value.charCodeAt(0) === 32)) {
						value = value.substring(1);
					}
					_tuple$10 = getnum(value, std === 265);
					day = _tuple$10[0];
					value = _tuple$10[1];
					err = _tuple$10[2];
					if (day < 0) {
						rangeErrString = "day";
					}
				} else if (_3 === (522)) {
					_tuple$11 = getnum(value, false);
					hour = _tuple$11[0];
					value = _tuple$11[1];
					err = _tuple$11[2];
					if (hour < 0 || 24 <= hour) {
						rangeErrString = "hour";
					}
				} else if ((_3 === (523)) || (_3 === (524))) {
					_tuple$12 = getnum(value, std === 524);
					hour = _tuple$12[0];
					value = _tuple$12[1];
					err = _tuple$12[2];
					if (hour < 0 || 12 < hour) {
						rangeErrString = "hour";
					}
				} else if ((_3 === (525)) || (_3 === (526))) {
					_tuple$13 = getnum(value, std === 526);
					min = _tuple$13[0];
					value = _tuple$13[1];
					err = _tuple$13[2];
					if (min < 0 || 60 <= min) {
						rangeErrString = "minute";
					}
				} else if ((_3 === (527)) || (_3 === (528))) {
					_tuple$14 = getnum(value, std === 528);
					sec = _tuple$14[0];
					value = _tuple$14[1];
					err = _tuple$14[2];
					if (sec < 0 || 60 <= sec) {
						rangeErrString = "second";
					}
					if (value.length >= 2 && (value.charCodeAt(0) === 46) && isDigit(value, 1)) {
						_tuple$15 = nextStdChunk(layout);
						std = _tuple$15[1];
						std = std & (65535);
						if ((std === 32) || (std === 33)) {
							break;
						}
						n = 2;
						while (true) {
							if (!(n < value.length && isDigit(value, n))) { break; }
							n = n + (1) >> 0;
						}
						_tuple$16 = parseNanoseconds(value, n);
						nsec = _tuple$16[0];
						rangeErrString = _tuple$16[1];
						err = _tuple$16[2];
						value = value.substring(n);
					}
				} else if (_3 === (531)) {
					if (value.length < 2) {
						err = errBad;
						break;
					}
					_tmp$6 = value.substring(0, 2);
					_tmp$7 = value.substring(2);
					p = _tmp$6;
					value = _tmp$7;
					_4 = p;
					if (_4 === ("PM")) {
						pmSet = true;
					} else if (_4 === ("AM")) {
						amSet = true;
					} else {
						err = errBad;
					}
				} else if (_3 === (532)) {
					if (value.length < 2) {
						err = errBad;
						break;
					}
					_tmp$8 = value.substring(0, 2);
					_tmp$9 = value.substring(2);
					p = _tmp$8;
					value = _tmp$9;
					_5 = p;
					if (_5 === ("pm")) {
						pmSet = true;
					} else if (_5 === ("am")) {
						amSet = true;
					} else {
						err = errBad;
					}
				} else if ((_3 === (22)) || (_3 === (25)) || (_3 === (23)) || (_3 === (24)) || (_3 === (26)) || (_3 === (27)) || (_3 === (29)) || (_3 === (30)) || (_3 === (28)) || (_3 === (31))) {
					if (((std === 22) || (std === 24) || (std === 25)) && value.length >= 1 && (value.charCodeAt(0) === 90)) {
						value = value.substring(1);
						z = $pkg.UTC;
						break;
					}
					_tmp$10 = "";
					_tmp$11 = "";
					_tmp$12 = "";
					_tmp$13 = "";
					sign = _tmp$10;
					hour$1 = _tmp$11;
					min$1 = _tmp$12;
					seconds = _tmp$13;
					if ((std === 25) || (std === 30)) {
						if (value.length < 6) {
							err = errBad;
							break;
						}
						if (!((value.charCodeAt(3) === 58))) {
							err = errBad;
							break;
						}
						_tmp$14 = value.substring(0, 1);
						_tmp$15 = value.substring(1, 3);
						_tmp$16 = value.substring(4, 6);
						_tmp$17 = "00";
						_tmp$18 = value.substring(6);
						sign = _tmp$14;
						hour$1 = _tmp$15;
						min$1 = _tmp$16;
						seconds = _tmp$17;
						value = _tmp$18;
					} else if ((std === 29) || (std === 24)) {
						if (value.length < 3) {
							err = errBad;
							break;
						}
						_tmp$19 = value.substring(0, 1);
						_tmp$20 = value.substring(1, 3);
						_tmp$21 = "00";
						_tmp$22 = "00";
						_tmp$23 = value.substring(3);
						sign = _tmp$19;
						hour$1 = _tmp$20;
						min$1 = _tmp$21;
						seconds = _tmp$22;
						value = _tmp$23;
					} else if ((std === 26) || (std === 31)) {
						if (value.length < 9) {
							err = errBad;
							break;
						}
						if (!((value.charCodeAt(3) === 58)) || !((value.charCodeAt(6) === 58))) {
							err = errBad;
							break;
						}
						_tmp$24 = value.substring(0, 1);
						_tmp$25 = value.substring(1, 3);
						_tmp$26 = value.substring(4, 6);
						_tmp$27 = value.substring(7, 9);
						_tmp$28 = value.substring(9);
						sign = _tmp$24;
						hour$1 = _tmp$25;
						min$1 = _tmp$26;
						seconds = _tmp$27;
						value = _tmp$28;
					} else if ((std === 23) || (std === 28)) {
						if (value.length < 7) {
							err = errBad;
							break;
						}
						_tmp$29 = value.substring(0, 1);
						_tmp$30 = value.substring(1, 3);
						_tmp$31 = value.substring(3, 5);
						_tmp$32 = value.substring(5, 7);
						_tmp$33 = value.substring(7);
						sign = _tmp$29;
						hour$1 = _tmp$30;
						min$1 = _tmp$31;
						seconds = _tmp$32;
						value = _tmp$33;
					} else {
						if (value.length < 5) {
							err = errBad;
							break;
						}
						_tmp$34 = value.substring(0, 1);
						_tmp$35 = value.substring(1, 3);
						_tmp$36 = value.substring(3, 5);
						_tmp$37 = "00";
						_tmp$38 = value.substring(5);
						sign = _tmp$34;
						hour$1 = _tmp$35;
						min$1 = _tmp$36;
						seconds = _tmp$37;
						value = _tmp$38;
					}
					_tmp$39 = 0;
					_tmp$40 = 0;
					_tmp$41 = 0;
					hr = _tmp$39;
					mm = _tmp$40;
					ss = _tmp$41;
					_tuple$17 = atoi(hour$1);
					hr = _tuple$17[0];
					err = _tuple$17[1];
					if ($interfaceIsEqual(err, $ifaceNil)) {
						_tuple$18 = atoi(min$1);
						mm = _tuple$18[0];
						err = _tuple$18[1];
					}
					if ($interfaceIsEqual(err, $ifaceNil)) {
						_tuple$19 = atoi(seconds);
						ss = _tuple$19[0];
						err = _tuple$19[1];
					}
					zoneOffset = ($imul(((($imul(hr, 60)) + mm >> 0)), 60)) + ss >> 0;
					_6 = sign.charCodeAt(0);
					if (_6 === (43)) {
					} else if (_6 === (45)) {
						zoneOffset = -zoneOffset;
					} else {
						err = errBad;
					}
				} else if (_3 === (21)) {
					if (value.length >= 3 && value.substring(0, 3) === "UTC") {
						z = $pkg.UTC;
						value = value.substring(3);
						break;
					}
					_tuple$20 = parseTimeZone(value);
					n$1 = _tuple$20[0];
					ok = _tuple$20[1];
					if (!ok) {
						err = errBad;
						break;
					}
					_tmp$42 = value.substring(0, n$1);
					_tmp$43 = value.substring(n$1);
					zoneName = _tmp$42;
					value = _tmp$43;
				} else if (_3 === (32)) {
					ndigit = 1 + ((std >> 16 >> 0)) >> 0;
					if (value.length < ndigit) {
						err = errBad;
						break;
					}
					_tuple$21 = parseNanoseconds(value, ndigit);
					nsec = _tuple$21[0];
					rangeErrString = _tuple$21[1];
					err = _tuple$21[2];
					value = value.substring(ndigit);
				} else if (_3 === (33)) {
					if (value.length < 2 || !((value.charCodeAt(0) === 46)) || value.charCodeAt(1) < 48 || 57 < value.charCodeAt(1)) {
						break;
					}
					i = 0;
					while (true) {
						if (!(i < 9 && (i + 1 >> 0) < value.length && 48 <= value.charCodeAt((i + 1 >> 0)) && value.charCodeAt((i + 1 >> 0)) <= 57)) { break; }
						i = i + (1) >> 0;
					}
					_tuple$22 = parseNanoseconds(value, 1 + i >> 0);
					nsec = _tuple$22[0];
					rangeErrString = _tuple$22[1];
					err = _tuple$22[2];
					value = value.substring((1 + i >> 0));
				}
			}
			if (!(rangeErrString === "")) {
				return [new Time.ptr(new $Int64(0, 0), 0, ptrType$1.nil), new ParseError.ptr(alayout, avalue, stdstr, value, ": " + rangeErrString + " out of range")];
			}
			if (!($interfaceIsEqual(err, $ifaceNil))) {
				return [new Time.ptr(new $Int64(0, 0), 0, ptrType$1.nil), new ParseError.ptr(alayout, avalue, stdstr, value, "")];
			}
		}
		if (pmSet && hour < 12) {
			hour = hour + (12) >> 0;
		} else if (amSet && (hour === 12)) {
			hour = 0;
		}
		if (day > daysIn((month >> 0), year)) {
			return [new Time.ptr(new $Int64(0, 0), 0, ptrType$1.nil), new ParseError.ptr(alayout, avalue, "", value, ": day out of range")];
		}
		/* */ if (!(z === ptrType$1.nil)) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!(z === ptrType$1.nil)) { */ case 1:
			_r$1 = Date(year, (month >> 0), day, hour, min, sec, nsec, z); /* */ $s = 3; case 3: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			/* */ $s = 4; case 4:
			return [_r$1, $ifaceNil];
		/* } */ case 2:
		/* */ if (!((zoneOffset === -1))) { $s = 5; continue; }
		/* */ $s = 6; continue;
		/* if (!((zoneOffset === -1))) { */ case 5:
			_r$2 = Date(year, (month >> 0), day, hour, min, sec, nsec, $pkg.UTC); /* */ $s = 7; case 7: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			t = $clone(_r$2, Time);
			t.sec = (x = t.sec, x$1 = new $Int64(0, zoneOffset), new $Int64(x.$high - x$1.$high, x.$low - x$1.$low));
			_r$3 = local.lookup((x$2 = t.sec, new $Int64(x$2.$high + -15, x$2.$low + 2288912640))); /* */ $s = 8; case 8: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_tuple$23 = _r$3;
			name = _tuple$23[0];
			offset = _tuple$23[1];
			if ((offset === zoneOffset) && (zoneName === "" || name === zoneName)) {
				t.loc = local;
				return [t, $ifaceNil];
			}
			t.loc = FixedZone(zoneName, zoneOffset);
			return [t, $ifaceNil];
		/* } */ case 6:
		/* */ if (!(zoneName === "")) { $s = 9; continue; }
		/* */ $s = 10; continue;
		/* if (!(zoneName === "")) { */ case 9:
			_r$4 = Date(year, (month >> 0), day, hour, min, sec, nsec, $pkg.UTC); /* */ $s = 11; case 11: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			t$1 = $clone(_r$4, Time);
			_r$5 = local.lookupName(zoneName, (x$3 = t$1.sec, new $Int64(x$3.$high + -15, x$3.$low + 2288912640))); /* */ $s = 12; case 12: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
			_tuple$24 = _r$5;
			offset$1 = _tuple$24[0];
			ok$1 = _tuple$24[2];
			if (ok$1) {
				t$1.sec = (x$4 = t$1.sec, x$5 = new $Int64(0, offset$1), new $Int64(x$4.$high - x$5.$high, x$4.$low - x$5.$low));
				t$1.loc = local;
				return [t$1, $ifaceNil];
			}
			if (zoneName.length > 3 && zoneName.substring(0, 3) === "GMT") {
				_tuple$25 = atoi(zoneName.substring(3));
				offset$1 = _tuple$25[0];
				offset$1 = $imul(offset$1, (3600));
			}
			t$1.loc = FixedZone(zoneName, offset$1);
			return [t$1, $ifaceNil];
		/* } */ case 10:
		_r$6 = Date(year, (month >> 0), day, hour, min, sec, nsec, defaultLocation); /* */ $s = 13; case 13: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
		/* */ $s = 14; case 14:
		return [_r$6, $ifaceNil];
		/* */ } return; } if ($f === undefined) { $f = { $blk: parse }; } $f.$ptr = $ptr; $f._3 = _3; $f._4 = _4; $f._5 = _5; $f._6 = _6; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$10 = _tmp$10; $f._tmp$11 = _tmp$11; $f._tmp$12 = _tmp$12; $f._tmp$13 = _tmp$13; $f._tmp$14 = _tmp$14; $f._tmp$15 = _tmp$15; $f._tmp$16 = _tmp$16; $f._tmp$17 = _tmp$17; $f._tmp$18 = _tmp$18; $f._tmp$19 = _tmp$19; $f._tmp$2 = _tmp$2; $f._tmp$20 = _tmp$20; $f._tmp$21 = _tmp$21; $f._tmp$22 = _tmp$22; $f._tmp$23 = _tmp$23; $f._tmp$24 = _tmp$24; $f._tmp$25 = _tmp$25; $f._tmp$26 = _tmp$26; $f._tmp$27 = _tmp$27; $f._tmp$28 = _tmp$28; $f._tmp$29 = _tmp$29; $f._tmp$3 = _tmp$3; $f._tmp$30 = _tmp$30; $f._tmp$31 = _tmp$31; $f._tmp$32 = _tmp$32; $f._tmp$33 = _tmp$33; $f._tmp$34 = _tmp$34; $f._tmp$35 = _tmp$35; $f._tmp$36 = _tmp$36; $f._tmp$37 = _tmp$37; $f._tmp$38 = _tmp$38; $f._tmp$39 = _tmp$39; $f._tmp$4 = _tmp$4; $f._tmp$40 = _tmp$40; $f._tmp$41 = _tmp$41; $f._tmp$42 = _tmp$42; $f._tmp$43 = _tmp$43; $f._tmp$5 = _tmp$5; $f._tmp$6 = _tmp$6; $f._tmp$7 = _tmp$7; $f._tmp$8 = _tmp$8; $f._tmp$9 = _tmp$9; $f._tuple$1 = _tuple$1; $f._tuple$10 = _tuple$10; $f._tuple$11 = _tuple$11; $f._tuple$12 = _tuple$12; $f._tuple$13 = _tuple$13; $f._tuple$14 = _tuple$14; $f._tuple$15 = _tuple$15; $f._tuple$16 = _tuple$16; $f._tuple$17 = _tuple$17; $f._tuple$18 = _tuple$18; $f._tuple$19 = _tuple$19; $f._tuple$2 = _tuple$2; $f._tuple$20 = _tuple$20; $f._tuple$21 = _tuple$21; $f._tuple$22 = _tuple$22; $f._tuple$23 = _tuple$23; $f._tuple$24 = _tuple$24; $f._tuple$25 = _tuple$25; $f._tuple$3 = _tuple$3; $f._tuple$4 = _tuple$4; $f._tuple$5 = _tuple$5; $f._tuple$6 = _tuple$6; $f._tuple$7 = _tuple$7; $f._tuple$8 = _tuple$8; $f._tuple$9 = _tuple$9; $f.alayout = alayout; $f.amSet = amSet; $f.avalue = avalue; $f.day = day; $f.defaultLocation = defaultLocation; $f.err = err; $f.hour = hour; $f.hour$1 = hour$1; $f.hr = hr; $f.i = i; $f.layout = layout; $f.local = local; $f.min = min; $f.min$1 = min$1; $f.mm = mm; $f.month = month; $f.n = n; $f.n$1 = n$1; $f.name = name; $f.ndigit = ndigit; $f.nsec = nsec; $f.offset = offset; $f.offset$1 = offset$1; $f.ok = ok; $f.ok$1 = ok$1; $f.p = p; $f.pmSet = pmSet; $f.prefix = prefix; $f.rangeErrString = rangeErrString; $f.sec = sec; $f.seconds = seconds; $f.sign = sign; $f.ss = ss; $f.std = std; $f.stdstr = stdstr; $f.suffix = suffix; $f.t = t; $f.t$1 = t$1; $f.value = value; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.year = year; $f.z = z; $f.zoneName = zoneName; $f.zoneOffset = zoneOffset; $f.$s = $s; $f.$r = $r; return $f;
	};
	parseTimeZone = function(value) {
		var $ptr, _7, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$12, _tmp$13, _tmp$14, _tmp$15, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, c, length, nUpper, ok, value;
		length = 0;
		ok = false;
		if (value.length < 3) {
			_tmp = 0;
			_tmp$1 = false;
			length = _tmp;
			ok = _tmp$1;
			return [length, ok];
		}
		if (value.length >= 4 && (value.substring(0, 4) === "ChST" || value.substring(0, 4) === "MeST")) {
			_tmp$2 = 4;
			_tmp$3 = true;
			length = _tmp$2;
			ok = _tmp$3;
			return [length, ok];
		}
		if (value.substring(0, 3) === "GMT") {
			length = parseGMT(value);
			_tmp$4 = length;
			_tmp$5 = true;
			length = _tmp$4;
			ok = _tmp$5;
			return [length, ok];
		}
		nUpper = 0;
		nUpper = 0;
		while (true) {
			if (!(nUpper < 6)) { break; }
			if (nUpper >= value.length) {
				break;
			}
			c = value.charCodeAt(nUpper);
			if (c < 65 || 90 < c) {
				break;
			}
			nUpper = nUpper + (1) >> 0;
		}
		_7 = nUpper;
		if ((_7 === (0)) || (_7 === (1)) || (_7 === (2)) || (_7 === (6))) {
			_tmp$6 = 0;
			_tmp$7 = false;
			length = _tmp$6;
			ok = _tmp$7;
			return [length, ok];
		} else if (_7 === (5)) {
			if (value.charCodeAt(4) === 84) {
				_tmp$8 = 5;
				_tmp$9 = true;
				length = _tmp$8;
				ok = _tmp$9;
				return [length, ok];
			}
		} else if (_7 === (4)) {
			if (value.charCodeAt(3) === 84) {
				_tmp$10 = 4;
				_tmp$11 = true;
				length = _tmp$10;
				ok = _tmp$11;
				return [length, ok];
			}
		} else if (_7 === (3)) {
			_tmp$12 = 3;
			_tmp$13 = true;
			length = _tmp$12;
			ok = _tmp$13;
			return [length, ok];
		}
		_tmp$14 = 0;
		_tmp$15 = false;
		length = _tmp$14;
		ok = _tmp$15;
		return [length, ok];
	};
	parseGMT = function(value) {
		var $ptr, _tuple$1, err, rem, sign, value, x;
		value = value.substring(3);
		if (value.length === 0) {
			return 3;
		}
		sign = value.charCodeAt(0);
		if (!((sign === 45)) && !((sign === 43))) {
			return 3;
		}
		_tuple$1 = leadingInt(value.substring(1));
		x = _tuple$1[0];
		rem = _tuple$1[1];
		err = _tuple$1[2];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return 3;
		}
		if (sign === 45) {
			x = new $Int64(-x.$high, -x.$low);
		}
		if ((x.$high === 0 && x.$low === 0) || (x.$high < -1 || (x.$high === -1 && x.$low < 4294967282)) || (0 < x.$high || (0 === x.$high && 12 < x.$low))) {
			return 3;
		}
		return (3 + value.length >> 0) - rem.length >> 0;
	};
	parseNanoseconds = function(value, nbytes) {
		var $ptr, _tuple$1, err, i, nbytes, ns, rangeErrString, scaleDigits, value;
		ns = 0;
		rangeErrString = "";
		err = $ifaceNil;
		if (!((value.charCodeAt(0) === 46))) {
			err = errBad;
			return [ns, rangeErrString, err];
		}
		_tuple$1 = atoi(value.substring(1, nbytes));
		ns = _tuple$1[0];
		err = _tuple$1[1];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return [ns, rangeErrString, err];
		}
		if (ns < 0 || 1000000000 <= ns) {
			rangeErrString = "fractional second";
			return [ns, rangeErrString, err];
		}
		scaleDigits = 10 - nbytes >> 0;
		i = 0;
		while (true) {
			if (!(i < scaleDigits)) { break; }
			ns = $imul(ns, (10));
			i = i + (1) >> 0;
		}
		return [ns, rangeErrString, err];
	};
	leadingInt = function(s) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, c, err, i, rem, s, x, x$1, x$2, x$3;
		x = new $Int64(0, 0);
		rem = "";
		err = $ifaceNil;
		i = 0;
		while (true) {
			if (!(i < s.length)) { break; }
			c = s.charCodeAt(i);
			if (c < 48 || c > 57) {
				break;
			}
			if ((x.$high > 214748364 || (x.$high === 214748364 && x.$low > 3435973836))) {
				_tmp = new $Int64(0, 0);
				_tmp$1 = "";
				_tmp$2 = errLeadingInt;
				x = _tmp;
				rem = _tmp$1;
				err = _tmp$2;
				return [x, rem, err];
			}
			x = (x$1 = (x$2 = $mul64(x, new $Int64(0, 10)), x$3 = new $Int64(0, c), new $Int64(x$2.$high + x$3.$high, x$2.$low + x$3.$low)), new $Int64(x$1.$high - 0, x$1.$low - 48));
			if ((x.$high < 0 || (x.$high === 0 && x.$low < 0))) {
				_tmp$3 = new $Int64(0, 0);
				_tmp$4 = "";
				_tmp$5 = errLeadingInt;
				x = _tmp$3;
				rem = _tmp$4;
				err = _tmp$5;
				return [x, rem, err];
			}
			i = i + (1) >> 0;
		}
		_tmp$6 = x;
		_tmp$7 = s.substring(i);
		_tmp$8 = $ifaceNil;
		x = _tmp$6;
		rem = _tmp$7;
		err = _tmp$8;
		return [x, rem, err];
	};
	Time.ptr.prototype.After = function(u) {
		var $ptr, t, u, x, x$1, x$2, x$3;
		u = $clone(u, Time);
		t = $clone(this, Time);
		return (x = t.sec, x$1 = u.sec, (x.$high > x$1.$high || (x.$high === x$1.$high && x.$low > x$1.$low))) || (x$2 = t.sec, x$3 = u.sec, (x$2.$high === x$3.$high && x$2.$low === x$3.$low)) && t.nsec > u.nsec;
	};
	Time.prototype.After = function(u) { return this.$val.After(u); };
	Time.ptr.prototype.Before = function(u) {
		var $ptr, t, u, x, x$1, x$2, x$3;
		u = $clone(u, Time);
		t = $clone(this, Time);
		return (x = t.sec, x$1 = u.sec, (x.$high < x$1.$high || (x.$high === x$1.$high && x.$low < x$1.$low))) || (x$2 = t.sec, x$3 = u.sec, (x$2.$high === x$3.$high && x$2.$low === x$3.$low)) && t.nsec < u.nsec;
	};
	Time.prototype.Before = function(u) { return this.$val.Before(u); };
	Time.ptr.prototype.Equal = function(u) {
		var $ptr, t, u, x, x$1;
		u = $clone(u, Time);
		t = $clone(this, Time);
		return (x = t.sec, x$1 = u.sec, (x.$high === x$1.$high && x.$low === x$1.$low)) && (t.nsec === u.nsec);
	};
	Time.prototype.Equal = function(u) { return this.$val.Equal(u); };
	Month.prototype.String = function() {
		var $ptr, m, x;
		m = this.$val;
		return (x = m - 1 >> 0, ((x < 0 || x >= months.length) ? $throwRuntimeError("index out of range") : months[x]));
	};
	$ptrType(Month).prototype.String = function() { return new Month(this.$get()).String(); };
	Weekday.prototype.String = function() {
		var $ptr, d;
		d = this.$val;
		return ((d < 0 || d >= days.length) ? $throwRuntimeError("index out of range") : days[d]);
	};
	$ptrType(Weekday).prototype.String = function() { return new Weekday(this.$get()).String(); };
	Time.ptr.prototype.IsZero = function() {
		var $ptr, t, x;
		t = $clone(this, Time);
		return (x = t.sec, (x.$high === 0 && x.$low === 0)) && (t.nsec === 0);
	};
	Time.prototype.IsZero = function() { return this.$val.IsZero(); };
	Time.ptr.prototype.abs = function() {
		var $ptr, _r$1, _r$2, _tuple$1, l, offset, sec, t, x, x$1, x$2, x$3, x$4, x$5, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; _tuple$1 = $f._tuple$1; l = $f.l; offset = $f.offset; sec = $f.sec; t = $f.t; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		l = t.loc;
		/* */ if (l === ptrType$1.nil || l === localLoc) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (l === ptrType$1.nil || l === localLoc) { */ case 1:
			_r$1 = l.get(); /* */ $s = 3; case 3: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			l = _r$1;
		/* } */ case 2:
		sec = (x = t.sec, new $Int64(x.$high + -15, x.$low + 2288912640));
		/* */ if (!(l === utcLoc)) { $s = 4; continue; }
		/* */ $s = 5; continue;
		/* if (!(l === utcLoc)) { */ case 4:
			/* */ if (!(l.cacheZone === ptrType.nil) && (x$1 = l.cacheStart, (x$1.$high < sec.$high || (x$1.$high === sec.$high && x$1.$low <= sec.$low))) && (x$2 = l.cacheEnd, (sec.$high < x$2.$high || (sec.$high === x$2.$high && sec.$low < x$2.$low)))) { $s = 6; continue; }
			/* */ $s = 7; continue;
			/* if (!(l.cacheZone === ptrType.nil) && (x$1 = l.cacheStart, (x$1.$high < sec.$high || (x$1.$high === sec.$high && x$1.$low <= sec.$low))) && (x$2 = l.cacheEnd, (sec.$high < x$2.$high || (sec.$high === x$2.$high && sec.$low < x$2.$low)))) { */ case 6:
				sec = (x$3 = new $Int64(0, l.cacheZone.offset), new $Int64(sec.$high + x$3.$high, sec.$low + x$3.$low));
				$s = 8; continue;
			/* } else { */ case 7:
				_r$2 = l.lookup(sec); /* */ $s = 9; case 9: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_tuple$1 = _r$2;
				offset = _tuple$1[1];
				sec = (x$4 = new $Int64(0, offset), new $Int64(sec.$high + x$4.$high, sec.$low + x$4.$low));
			/* } */ case 8:
		/* } */ case 5:
		return (x$5 = new $Int64(sec.$high + 2147483646, sec.$low + 450480384), new $Uint64(x$5.$high, x$5.$low));
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.abs }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._tuple$1 = _tuple$1; $f.l = l; $f.offset = offset; $f.sec = sec; $f.t = t; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.abs = function() { return this.$val.abs(); };
	Time.ptr.prototype.locabs = function() {
		var $ptr, _r$1, _r$2, _tuple$1, abs, l, name, offset, sec, t, x, x$1, x$2, x$3, x$4, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; _tuple$1 = $f._tuple$1; abs = $f.abs; l = $f.l; name = $f.name; offset = $f.offset; sec = $f.sec; t = $f.t; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		name = "";
		offset = 0;
		abs = new $Uint64(0, 0);
		t = $clone(this, Time);
		l = t.loc;
		/* */ if (l === ptrType$1.nil || l === localLoc) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (l === ptrType$1.nil || l === localLoc) { */ case 1:
			_r$1 = l.get(); /* */ $s = 3; case 3: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			l = _r$1;
		/* } */ case 2:
		sec = (x = t.sec, new $Int64(x.$high + -15, x.$low + 2288912640));
		/* */ if (!(l === utcLoc)) { $s = 4; continue; }
		/* */ $s = 5; continue;
		/* if (!(l === utcLoc)) { */ case 4:
			/* */ if (!(l.cacheZone === ptrType.nil) && (x$1 = l.cacheStart, (x$1.$high < sec.$high || (x$1.$high === sec.$high && x$1.$low <= sec.$low))) && (x$2 = l.cacheEnd, (sec.$high < x$2.$high || (sec.$high === x$2.$high && sec.$low < x$2.$low)))) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (!(l.cacheZone === ptrType.nil) && (x$1 = l.cacheStart, (x$1.$high < sec.$high || (x$1.$high === sec.$high && x$1.$low <= sec.$low))) && (x$2 = l.cacheEnd, (sec.$high < x$2.$high || (sec.$high === x$2.$high && sec.$low < x$2.$low)))) { */ case 7:
				name = l.cacheZone.name;
				offset = l.cacheZone.offset;
				$s = 9; continue;
			/* } else { */ case 8:
				_r$2 = l.lookup(sec); /* */ $s = 10; case 10: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_tuple$1 = _r$2;
				name = _tuple$1[0];
				offset = _tuple$1[1];
			/* } */ case 9:
			sec = (x$3 = new $Int64(0, offset), new $Int64(sec.$high + x$3.$high, sec.$low + x$3.$low));
			$s = 6; continue;
		/* } else { */ case 5:
			name = "UTC";
		/* } */ case 6:
		abs = (x$4 = new $Int64(sec.$high + 2147483646, sec.$low + 450480384), new $Uint64(x$4.$high, x$4.$low));
		return [name, offset, abs];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.locabs }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._tuple$1 = _tuple$1; $f.abs = abs; $f.l = l; $f.name = name; $f.offset = offset; $f.sec = sec; $f.t = t; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.locabs = function() { return this.$val.locabs(); };
	Time.ptr.prototype.Date = function() {
		var $ptr, _r$1, _tuple$1, day, month, t, year, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; day = $f.day; month = $f.month; t = $f.t; year = $f.year; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		year = 0;
		month = 0;
		day = 0;
		t = $clone(this, Time);
		_r$1 = t.date(true); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		year = _tuple$1[0];
		month = _tuple$1[1];
		day = _tuple$1[2];
		return [year, month, day];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Date }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.day = day; $f.month = month; $f.t = t; $f.year = year; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Date = function() { return this.$val.Date(); };
	Time.ptr.prototype.Year = function() {
		var $ptr, _r$1, _tuple$1, t, year, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; t = $f.t; year = $f.year; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.date(false); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		year = _tuple$1[0];
		return year;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Year }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.t = t; $f.year = year; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Year = function() { return this.$val.Year(); };
	Time.ptr.prototype.Month = function() {
		var $ptr, _r$1, _tuple$1, month, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; month = $f.month; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.date(true); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		month = _tuple$1[1];
		return month;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Month }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.month = month; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Month = function() { return this.$val.Month(); };
	Time.ptr.prototype.Day = function() {
		var $ptr, _r$1, _tuple$1, day, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; day = $f.day; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.date(true); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		day = _tuple$1[2];
		return day;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Day }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.day = day; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Day = function() { return this.$val.Day(); };
	Time.ptr.prototype.Weekday = function() {
		var $ptr, _r$1, _r$2, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.abs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_r$2 = absWeekday(_r$1); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return _r$2;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Weekday }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Weekday = function() { return this.$val.Weekday(); };
	absWeekday = function(abs) {
		var $ptr, _q, abs, sec;
		sec = $div64((new $Uint64(abs.$high + 0, abs.$low + 86400)), new $Uint64(0, 604800), true);
		return ((_q = (sec.$low >> 0) / 86400, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) >> 0);
	};
	Time.ptr.prototype.ISOWeek = function() {
		var $ptr, _q, _r$1, _r$2, _r$3, _r$4, _r$5, _tuple$1, day, dec31wday, jan1wday, month, t, wday, week, yday, year, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _q = $f._q; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _tuple$1 = $f._tuple$1; day = $f.day; dec31wday = $f.dec31wday; jan1wday = $f.jan1wday; month = $f.month; t = $f.t; wday = $f.wday; week = $f.week; yday = $f.yday; year = $f.year; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		year = 0;
		week = 0;
		t = $clone(this, Time);
		_r$1 = t.date(true); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		year = _tuple$1[0];
		month = _tuple$1[1];
		day = _tuple$1[2];
		yday = _tuple$1[3];
		_r$3 = t.Weekday(); /* */ $s = 2; case 2: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		wday = (_r$2 = ((_r$3 + 6 >> 0) >> 0) % 7, _r$2 === _r$2 ? _r$2 : $throwRuntimeError("integer divide by zero"));
		week = (_q = (((yday - wday >> 0) + 7 >> 0)) / 7, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
		jan1wday = (_r$4 = (((wday - yday >> 0) + 371 >> 0)) % 7, _r$4 === _r$4 ? _r$4 : $throwRuntimeError("integer divide by zero"));
		if (1 <= jan1wday && jan1wday <= 3) {
			week = week + (1) >> 0;
		}
		if (week === 0) {
			year = year - (1) >> 0;
			week = 52;
			if ((jan1wday === 4) || ((jan1wday === 5) && isLeap(year))) {
				week = week + (1) >> 0;
			}
		}
		if ((month === 12) && day >= 29 && wday < 3) {
			dec31wday = (_r$5 = (((wday + 31 >> 0) - day >> 0)) % 7, _r$5 === _r$5 ? _r$5 : $throwRuntimeError("integer divide by zero"));
			if (0 <= dec31wday && dec31wday <= 2) {
				year = year + (1) >> 0;
				week = 1;
			}
		}
		return [year, week];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.ISOWeek }; } $f.$ptr = $ptr; $f._q = _q; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._tuple$1 = _tuple$1; $f.day = day; $f.dec31wday = dec31wday; $f.jan1wday = jan1wday; $f.month = month; $f.t = t; $f.wday = wday; $f.week = week; $f.yday = yday; $f.year = year; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.ISOWeek = function() { return this.$val.ISOWeek(); };
	Time.ptr.prototype.Clock = function() {
		var $ptr, _r$1, _r$2, _tuple$1, hour, min, sec, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; _tuple$1 = $f._tuple$1; hour = $f.hour; min = $f.min; sec = $f.sec; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		hour = 0;
		min = 0;
		sec = 0;
		t = $clone(this, Time);
		_r$1 = t.abs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_r$2 = absClock(_r$1); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_tuple$1 = _r$2;
		hour = _tuple$1[0];
		min = _tuple$1[1];
		sec = _tuple$1[2];
		/* */ $s = 3; case 3:
		return [hour, min, sec];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Clock }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._tuple$1 = _tuple$1; $f.hour = hour; $f.min = min; $f.sec = sec; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Clock = function() { return this.$val.Clock(); };
	absClock = function(abs) {
		var $ptr, _q, _q$1, abs, hour, min, sec;
		hour = 0;
		min = 0;
		sec = 0;
		sec = ($div64(abs, new $Uint64(0, 86400), true).$low >> 0);
		hour = (_q = sec / 3600, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
		sec = sec - (($imul(hour, 3600))) >> 0;
		min = (_q$1 = sec / 60, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >> 0 : $throwRuntimeError("integer divide by zero"));
		sec = sec - (($imul(min, 60))) >> 0;
		return [hour, min, sec];
	};
	Time.ptr.prototype.Hour = function() {
		var $ptr, _q, _r$1, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _q = $f._q; _r$1 = $f._r$1; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.abs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return (_q = ($div64(_r$1, new $Uint64(0, 86400), true).$low >> 0) / 3600, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Hour }; } $f.$ptr = $ptr; $f._q = _q; $f._r$1 = _r$1; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Hour = function() { return this.$val.Hour(); };
	Time.ptr.prototype.Minute = function() {
		var $ptr, _q, _r$1, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _q = $f._q; _r$1 = $f._r$1; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.abs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return (_q = ($div64(_r$1, new $Uint64(0, 3600), true).$low >> 0) / 60, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Minute }; } $f.$ptr = $ptr; $f._q = _q; $f._r$1 = _r$1; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Minute = function() { return this.$val.Minute(); };
	Time.ptr.prototype.Second = function() {
		var $ptr, _r$1, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.abs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return ($div64(_r$1, new $Uint64(0, 60), true).$low >> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Second }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Second = function() { return this.$val.Second(); };
	Time.ptr.prototype.Nanosecond = function() {
		var $ptr, t;
		t = $clone(this, Time);
		return (t.nsec >> 0);
	};
	Time.prototype.Nanosecond = function() { return this.$val.Nanosecond(); };
	Time.ptr.prototype.YearDay = function() {
		var $ptr, _r$1, _tuple$1, t, yday, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; t = $f.t; yday = $f.yday; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.date(false); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		yday = _tuple$1[3];
		return yday + 1 >> 0;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.YearDay }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.t = t; $f.yday = yday; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.YearDay = function() { return this.$val.YearDay(); };
	Duration.prototype.String = function() {
		var $ptr, _tuple$1, _tuple$2, buf, d, neg, prec, u, w;
		d = this;
		buf = arrayType$4.zero();
		w = 32;
		u = new $Uint64(d.$high, d.$low);
		neg = (d.$high < 0 || (d.$high === 0 && d.$low < 0));
		if (neg) {
			u = new $Uint64(-u.$high, -u.$low);
		}
		if ((u.$high < 0 || (u.$high === 0 && u.$low < 1000000000))) {
			prec = 0;
			w = w - (1) >> 0;
			((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 115);
			w = w - (1) >> 0;
			if ((u.$high === 0 && u.$low === 0)) {
				return "0";
			} else if ((u.$high < 0 || (u.$high === 0 && u.$low < 1000))) {
				prec = 0;
				((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 110);
			} else if ((u.$high < 0 || (u.$high === 0 && u.$low < 1000000))) {
				prec = 3;
				w = w - (1) >> 0;
				$copyString($subslice(new sliceType$3(buf), w), "\xC2\xB5");
			} else {
				prec = 6;
				((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 109);
			}
			_tuple$1 = fmtFrac($subslice(new sliceType$3(buf), 0, w), u, prec);
			w = _tuple$1[0];
			u = _tuple$1[1];
			w = fmtInt($subslice(new sliceType$3(buf), 0, w), u);
		} else {
			w = w - (1) >> 0;
			((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 115);
			_tuple$2 = fmtFrac($subslice(new sliceType$3(buf), 0, w), u, 9);
			w = _tuple$2[0];
			u = _tuple$2[1];
			w = fmtInt($subslice(new sliceType$3(buf), 0, w), $div64(u, new $Uint64(0, 60), true));
			u = $div64(u, (new $Uint64(0, 60)), false);
			if ((u.$high > 0 || (u.$high === 0 && u.$low > 0))) {
				w = w - (1) >> 0;
				((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 109);
				w = fmtInt($subslice(new sliceType$3(buf), 0, w), $div64(u, new $Uint64(0, 60), true));
				u = $div64(u, (new $Uint64(0, 60)), false);
				if ((u.$high > 0 || (u.$high === 0 && u.$low > 0))) {
					w = w - (1) >> 0;
					((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 104);
					w = fmtInt($subslice(new sliceType$3(buf), 0, w), u);
				}
			}
		}
		if (neg) {
			w = w - (1) >> 0;
			((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 45);
		}
		return $bytesToString($subslice(new sliceType$3(buf), w));
	};
	$ptrType(Duration).prototype.String = function() { return this.$get().String(); };
	fmtFrac = function(buf, v, prec) {
		var $ptr, _tmp, _tmp$1, buf, digit, i, nv, nw, prec, print, v, w;
		nw = 0;
		nv = new $Uint64(0, 0);
		w = buf.$length;
		print = false;
		i = 0;
		while (true) {
			if (!(i < prec)) { break; }
			digit = $div64(v, new $Uint64(0, 10), true);
			print = print || !((digit.$high === 0 && digit.$low === 0));
			if (print) {
				w = w - (1) >> 0;
				((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = ((digit.$low << 24 >>> 24) + 48 << 24 >>> 24));
			}
			v = $div64(v, (new $Uint64(0, 10)), false);
			i = i + (1) >> 0;
		}
		if (print) {
			w = w - (1) >> 0;
			((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = 46);
		}
		_tmp = w;
		_tmp$1 = v;
		nw = _tmp;
		nv = _tmp$1;
		return [nw, nv];
	};
	fmtInt = function(buf, v) {
		var $ptr, buf, v, w;
		w = buf.$length;
		if ((v.$high === 0 && v.$low === 0)) {
			w = w - (1) >> 0;
			((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = 48);
		} else {
			while (true) {
				if (!((v.$high > 0 || (v.$high === 0 && v.$low > 0)))) { break; }
				w = w - (1) >> 0;
				((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = (($div64(v, new $Uint64(0, 10), true).$low << 24 >>> 24) + 48 << 24 >>> 24));
				v = $div64(v, (new $Uint64(0, 10)), false);
			}
		}
		return w;
	};
	Duration.prototype.Nanoseconds = function() {
		var $ptr, d;
		d = this;
		return new $Int64(d.$high, d.$low);
	};
	$ptrType(Duration).prototype.Nanoseconds = function() { return this.$get().Nanoseconds(); };
	Duration.prototype.Seconds = function() {
		var $ptr, d, nsec, sec;
		d = this;
		sec = $div64(d, new Duration(0, 1000000000), false);
		nsec = $div64(d, new Duration(0, 1000000000), true);
		return $flatten64(sec) + $flatten64(nsec) * 1e-09;
	};
	$ptrType(Duration).prototype.Seconds = function() { return this.$get().Seconds(); };
	Duration.prototype.Minutes = function() {
		var $ptr, d, min, nsec;
		d = this;
		min = $div64(d, new Duration(13, 4165425152), false);
		nsec = $div64(d, new Duration(13, 4165425152), true);
		return $flatten64(min) + $flatten64(nsec) * 1.6666666666666667e-11;
	};
	$ptrType(Duration).prototype.Minutes = function() { return this.$get().Minutes(); };
	Duration.prototype.Hours = function() {
		var $ptr, d, hour, nsec;
		d = this;
		hour = $div64(d, new Duration(838, 817405952), false);
		nsec = $div64(d, new Duration(838, 817405952), true);
		return $flatten64(hour) + $flatten64(nsec) * 2.777777777777778e-13;
	};
	$ptrType(Duration).prototype.Hours = function() { return this.$get().Hours(); };
	Time.ptr.prototype.Add = function(d) {
		var $ptr, d, nsec, t, x, x$1, x$2, x$3, x$4, x$5, x$6, x$7;
		t = $clone(this, Time);
		t.sec = (x = t.sec, x$1 = (x$2 = $div64(d, new Duration(0, 1000000000), false), new $Int64(x$2.$high, x$2.$low)), new $Int64(x.$high + x$1.$high, x.$low + x$1.$low));
		nsec = t.nsec + ((x$3 = $div64(d, new Duration(0, 1000000000), true), x$3.$low + ((x$3.$high >> 31) * 4294967296)) >> 0) >> 0;
		if (nsec >= 1000000000) {
			t.sec = (x$4 = t.sec, x$5 = new $Int64(0, 1), new $Int64(x$4.$high + x$5.$high, x$4.$low + x$5.$low));
			nsec = nsec - (1000000000) >> 0;
		} else if (nsec < 0) {
			t.sec = (x$6 = t.sec, x$7 = new $Int64(0, 1), new $Int64(x$6.$high - x$7.$high, x$6.$low - x$7.$low));
			nsec = nsec + (1000000000) >> 0;
		}
		t.nsec = nsec;
		return t;
	};
	Time.prototype.Add = function(d) { return this.$val.Add(d); };
	Time.ptr.prototype.Sub = function(u) {
		var $ptr, d, t, u, x, x$1, x$2, x$3, x$4;
		u = $clone(u, Time);
		t = $clone(this, Time);
		d = (x = $mul64((x$1 = (x$2 = t.sec, x$3 = u.sec, new $Int64(x$2.$high - x$3.$high, x$2.$low - x$3.$low)), new Duration(x$1.$high, x$1.$low)), new Duration(0, 1000000000)), x$4 = new Duration(0, (t.nsec - u.nsec >> 0)), new Duration(x.$high + x$4.$high, x.$low + x$4.$low));
		if (u.Add(d).Equal(t)) {
			return d;
		} else if (t.Before(u)) {
			return new Duration(-2147483648, 0);
		} else {
			return new Duration(2147483647, 4294967295);
		}
	};
	Time.prototype.Sub = function(u) { return this.$val.Sub(u); };
	Time.ptr.prototype.AddDate = function(years, months$1, days$1) {
		var $ptr, _r$1, _r$2, _r$3, _tuple$1, _tuple$2, day, days$1, hour, min, month, months$1, sec, t, year, years, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _tuple$1 = $f._tuple$1; _tuple$2 = $f._tuple$2; day = $f.day; days$1 = $f.days$1; hour = $f.hour; min = $f.min; month = $f.month; months$1 = $f.months$1; sec = $f.sec; t = $f.t; year = $f.year; years = $f.years; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.Date(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		year = _tuple$1[0];
		month = _tuple$1[1];
		day = _tuple$1[2];
		_r$2 = t.Clock(); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_tuple$2 = _r$2;
		hour = _tuple$2[0];
		min = _tuple$2[1];
		sec = _tuple$2[2];
		_r$3 = Date(year + years >> 0, month + (months$1 >> 0) >> 0, day + days$1 >> 0, hour, min, sec, (t.nsec >> 0), t.loc); /* */ $s = 3; case 3: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		/* */ $s = 4; case 4:
		return _r$3;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.AddDate }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._tuple$1 = _tuple$1; $f._tuple$2 = _tuple$2; $f.day = day; $f.days$1 = days$1; $f.hour = hour; $f.min = min; $f.month = month; $f.months$1 = months$1; $f.sec = sec; $f.t = t; $f.year = year; $f.years = years; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.AddDate = function(years, months$1, days$1) { return this.$val.AddDate(years, months$1, days$1); };
	Time.ptr.prototype.date = function(full) {
		var $ptr, _r$1, _r$2, _tuple$1, day, full, month, t, yday, year, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; _tuple$1 = $f._tuple$1; day = $f.day; full = $f.full; month = $f.month; t = $f.t; yday = $f.yday; year = $f.year; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		year = 0;
		month = 0;
		day = 0;
		yday = 0;
		t = $clone(this, Time);
		_r$1 = t.abs(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_r$2 = absDate(_r$1, full); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_tuple$1 = _r$2;
		year = _tuple$1[0];
		month = _tuple$1[1];
		day = _tuple$1[2];
		yday = _tuple$1[3];
		/* */ $s = 3; case 3:
		return [year, month, day, yday];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.date }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._tuple$1 = _tuple$1; $f.day = day; $f.full = full; $f.month = month; $f.t = t; $f.yday = yday; $f.year = year; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.date = function(full) { return this.$val.date(full); };
	absDate = function(abs, full) {
		var $ptr, _q, abs, begin, d, day, end, full, month, n, x, x$1, x$10, x$11, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, y, yday, year;
		year = 0;
		month = 0;
		day = 0;
		yday = 0;
		d = $div64(abs, new $Uint64(0, 86400), false);
		n = $div64(d, new $Uint64(0, 146097), false);
		y = $mul64(new $Uint64(0, 400), n);
		d = (x = $mul64(new $Uint64(0, 146097), n), new $Uint64(d.$high - x.$high, d.$low - x.$low));
		n = $div64(d, new $Uint64(0, 36524), false);
		n = (x$1 = $shiftRightUint64(n, 2), new $Uint64(n.$high - x$1.$high, n.$low - x$1.$low));
		y = (x$2 = $mul64(new $Uint64(0, 100), n), new $Uint64(y.$high + x$2.$high, y.$low + x$2.$low));
		d = (x$3 = $mul64(new $Uint64(0, 36524), n), new $Uint64(d.$high - x$3.$high, d.$low - x$3.$low));
		n = $div64(d, new $Uint64(0, 1461), false);
		y = (x$4 = $mul64(new $Uint64(0, 4), n), new $Uint64(y.$high + x$4.$high, y.$low + x$4.$low));
		d = (x$5 = $mul64(new $Uint64(0, 1461), n), new $Uint64(d.$high - x$5.$high, d.$low - x$5.$low));
		n = $div64(d, new $Uint64(0, 365), false);
		n = (x$6 = $shiftRightUint64(n, 2), new $Uint64(n.$high - x$6.$high, n.$low - x$6.$low));
		y = (x$7 = n, new $Uint64(y.$high + x$7.$high, y.$low + x$7.$low));
		d = (x$8 = $mul64(new $Uint64(0, 365), n), new $Uint64(d.$high - x$8.$high, d.$low - x$8.$low));
		year = ((x$9 = (x$10 = new $Int64(y.$high, y.$low), new $Int64(x$10.$high + -69, x$10.$low + 4075721025)), x$9.$low + ((x$9.$high >> 31) * 4294967296)) >> 0);
		yday = (d.$low >> 0);
		if (!full) {
			return [year, month, day, yday];
		}
		day = yday;
		if (isLeap(year)) {
			if (day > 59) {
				day = day - (1) >> 0;
			} else if ((day === 59)) {
				month = 2;
				day = 29;
				return [year, month, day, yday];
			}
		}
		month = ((_q = day / 31, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) >> 0);
		end = ((x$11 = month + 1 >> 0, ((x$11 < 0 || x$11 >= daysBefore.length) ? $throwRuntimeError("index out of range") : daysBefore[x$11])) >> 0);
		begin = 0;
		if (day >= end) {
			month = month + (1) >> 0;
			begin = end;
		} else {
			begin = (((month < 0 || month >= daysBefore.length) ? $throwRuntimeError("index out of range") : daysBefore[month]) >> 0);
		}
		month = month + (1) >> 0;
		day = (day - begin >> 0) + 1 >> 0;
		return [year, month, day, yday];
	};
	daysIn = function(m, year) {
		var $ptr, m, x, year;
		if ((m === 2) && isLeap(year)) {
			return 29;
		}
		return ((((m < 0 || m >= daysBefore.length) ? $throwRuntimeError("index out of range") : daysBefore[m]) - (x = m - 1 >> 0, ((x < 0 || x >= daysBefore.length) ? $throwRuntimeError("index out of range") : daysBefore[x])) >> 0) >> 0);
	};
	Time.ptr.prototype.UTC = function() {
		var $ptr, t;
		t = $clone(this, Time);
		t.loc = $pkg.UTC;
		return t;
	};
	Time.prototype.UTC = function() { return this.$val.UTC(); };
	Time.ptr.prototype.Local = function() {
		var $ptr, t;
		t = $clone(this, Time);
		t.loc = $pkg.Local;
		return t;
	};
	Time.prototype.Local = function() { return this.$val.Local(); };
	Time.ptr.prototype.In = function(loc) {
		var $ptr, loc, t;
		t = $clone(this, Time);
		if (loc === ptrType$1.nil) {
			$panic(new $String("time: missing Location in call to Time.In"));
		}
		t.loc = loc;
		return t;
	};
	Time.prototype.In = function(loc) { return this.$val.In(loc); };
	Time.ptr.prototype.Location = function() {
		var $ptr, l, t;
		t = $clone(this, Time);
		l = t.loc;
		if (l === ptrType$1.nil) {
			l = $pkg.UTC;
		}
		return l;
	};
	Time.prototype.Location = function() { return this.$val.Location(); };
	Time.ptr.prototype.Zone = function() {
		var $ptr, _r$1, _tuple$1, name, offset, t, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; name = $f.name; offset = $f.offset; t = $f.t; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		name = "";
		offset = 0;
		t = $clone(this, Time);
		_r$1 = t.loc.lookup((x = t.sec, new $Int64(x.$high + -15, x.$low + 2288912640))); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		name = _tuple$1[0];
		offset = _tuple$1[1];
		return [name, offset];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.Zone }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.name = name; $f.offset = offset; $f.t = t; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.Zone = function() { return this.$val.Zone(); };
	Time.ptr.prototype.Unix = function() {
		var $ptr, t, x;
		t = $clone(this, Time);
		return (x = t.sec, new $Int64(x.$high + -15, x.$low + 2288912640));
	};
	Time.prototype.Unix = function() { return this.$val.Unix(); };
	Time.ptr.prototype.UnixNano = function() {
		var $ptr, t, x, x$1, x$2;
		t = $clone(this, Time);
		return (x = $mul64(((x$1 = t.sec, new $Int64(x$1.$high + -15, x$1.$low + 2288912640))), new $Int64(0, 1000000000)), x$2 = new $Int64(0, t.nsec), new $Int64(x.$high + x$2.$high, x.$low + x$2.$low));
	};
	Time.prototype.UnixNano = function() { return this.$val.UnixNano(); };
	Time.ptr.prototype.MarshalBinary = function() {
		var $ptr, _q, _r$1, _r$2, _tuple$1, enc, offset, offsetMin, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _q = $f._q; _r$1 = $f._r$1; _r$2 = $f._r$2; _tuple$1 = $f._tuple$1; enc = $f.enc; offset = $f.offset; offsetMin = $f.offsetMin; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		offsetMin = 0;
		/* */ if (t.Location() === utcLoc) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (t.Location() === utcLoc) { */ case 1:
			offsetMin = -1;
			$s = 3; continue;
		/* } else { */ case 2:
			_r$1 = t.Zone(); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			_tuple$1 = _r$1;
			offset = _tuple$1[1];
			if (!(((_r$2 = offset % 60, _r$2 === _r$2 ? _r$2 : $throwRuntimeError("integer divide by zero")) === 0))) {
				return [sliceType$3.nil, errors.New("Time.MarshalBinary: zone offset has fractional minute")];
			}
			offset = (_q = offset / (60), (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
			if (offset < -32768 || (offset === -1) || offset > 32767) {
				return [sliceType$3.nil, errors.New("Time.MarshalBinary: unexpected zone offset")];
			}
			offsetMin = (offset << 16 >> 16);
		/* } */ case 3:
		enc = new sliceType$3([1, ($shiftRightInt64(t.sec, 56).$low << 24 >>> 24), ($shiftRightInt64(t.sec, 48).$low << 24 >>> 24), ($shiftRightInt64(t.sec, 40).$low << 24 >>> 24), ($shiftRightInt64(t.sec, 32).$low << 24 >>> 24), ($shiftRightInt64(t.sec, 24).$low << 24 >>> 24), ($shiftRightInt64(t.sec, 16).$low << 24 >>> 24), ($shiftRightInt64(t.sec, 8).$low << 24 >>> 24), (t.sec.$low << 24 >>> 24), ((t.nsec >> 24 >> 0) << 24 >>> 24), ((t.nsec >> 16 >> 0) << 24 >>> 24), ((t.nsec >> 8 >> 0) << 24 >>> 24), (t.nsec << 24 >>> 24), ((offsetMin >> 8 << 16 >> 16) << 24 >>> 24), (offsetMin << 24 >>> 24)]);
		return [enc, $ifaceNil];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.MarshalBinary }; } $f.$ptr = $ptr; $f._q = _q; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._tuple$1 = _tuple$1; $f.enc = enc; $f.offset = offset; $f.offsetMin = offsetMin; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.MarshalBinary = function() { return this.$val.MarshalBinary(); };
	Time.ptr.prototype.UnmarshalBinary = function(data$1) {
		var $ptr, _r$1, _tuple$1, buf, data$1, localoff, offset, t, x, x$1, x$10, x$11, x$12, x$13, x$14, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; buf = $f.buf; data$1 = $f.data$1; localoff = $f.localoff; offset = $f.offset; t = $f.t; x = $f.x; x$1 = $f.x$1; x$10 = $f.x$10; x$11 = $f.x$11; x$12 = $f.x$12; x$13 = $f.x$13; x$14 = $f.x$14; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; x$6 = $f.x$6; x$7 = $f.x$7; x$8 = $f.x$8; x$9 = $f.x$9; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		buf = data$1;
		if (buf.$length === 0) {
			return errors.New("Time.UnmarshalBinary: no data");
		}
		if (!(((0 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 0]) === 1))) {
			return errors.New("Time.UnmarshalBinary: unsupported version");
		}
		if (!((buf.$length === 15))) {
			return errors.New("Time.UnmarshalBinary: invalid length");
		}
		buf = $subslice(buf, 1);
		t.sec = (x = (x$1 = (x$2 = (x$3 = (x$4 = (x$5 = (x$6 = new $Int64(0, (7 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 7])), x$7 = $shiftLeft64(new $Int64(0, (6 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 6])), 8), new $Int64(x$6.$high | x$7.$high, (x$6.$low | x$7.$low) >>> 0)), x$8 = $shiftLeft64(new $Int64(0, (5 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 5])), 16), new $Int64(x$5.$high | x$8.$high, (x$5.$low | x$8.$low) >>> 0)), x$9 = $shiftLeft64(new $Int64(0, (4 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 4])), 24), new $Int64(x$4.$high | x$9.$high, (x$4.$low | x$9.$low) >>> 0)), x$10 = $shiftLeft64(new $Int64(0, (3 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 3])), 32), new $Int64(x$3.$high | x$10.$high, (x$3.$low | x$10.$low) >>> 0)), x$11 = $shiftLeft64(new $Int64(0, (2 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 2])), 40), new $Int64(x$2.$high | x$11.$high, (x$2.$low | x$11.$low) >>> 0)), x$12 = $shiftLeft64(new $Int64(0, (1 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 1])), 48), new $Int64(x$1.$high | x$12.$high, (x$1.$low | x$12.$low) >>> 0)), x$13 = $shiftLeft64(new $Int64(0, (0 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 0])), 56), new $Int64(x.$high | x$13.$high, (x.$low | x$13.$low) >>> 0));
		buf = $subslice(buf, 8);
		t.nsec = ((((3 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 3]) >> 0) | (((2 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 2]) >> 0) << 8 >> 0)) | (((1 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 1]) >> 0) << 16 >> 0)) | (((0 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 0]) >> 0) << 24 >> 0);
		buf = $subslice(buf, 4);
		offset = $imul(((((1 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 1]) << 16 >> 16) | (((0 >= buf.$length ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + 0]) << 16 >> 16) << 8 << 16 >> 16)) >> 0), 60);
		/* */ if (offset === -60) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (offset === -60) { */ case 1:
			t.loc = utcLoc;
			$s = 3; continue;
		/* } else { */ case 2:
			_r$1 = $pkg.Local.lookup((x$14 = t.sec, new $Int64(x$14.$high + -15, x$14.$low + 2288912640))); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			_tuple$1 = _r$1;
			localoff = _tuple$1[1];
			if (offset === localoff) {
				t.loc = $pkg.Local;
			} else {
				t.loc = FixedZone("", offset);
			}
		/* } */ case 3:
		return $ifaceNil;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.UnmarshalBinary }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.buf = buf; $f.data$1 = data$1; $f.localoff = localoff; $f.offset = offset; $f.t = t; $f.x = x; $f.x$1 = x$1; $f.x$10 = x$10; $f.x$11 = x$11; $f.x$12 = x$12; $f.x$13 = x$13; $f.x$14 = x$14; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.x$6 = x$6; $f.x$7 = x$7; $f.x$8 = x$8; $f.x$9 = x$9; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.UnmarshalBinary = function(data$1) { return this.$val.UnmarshalBinary(data$1); };
	Time.ptr.prototype.GobEncode = function() {
		var $ptr, _r$1, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.MarshalBinary(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.GobEncode }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.GobEncode = function() { return this.$val.GobEncode(); };
	Time.ptr.prototype.GobDecode = function(data$1) {
		var $ptr, _r$1, data$1, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; data$1 = $f.data$1; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		_r$1 = t.UnmarshalBinary(data$1); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.GobDecode }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.data$1 = data$1; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.GobDecode = function(data$1) { return this.$val.GobDecode(data$1); };
	Time.ptr.prototype.MarshalJSON = function() {
		var $ptr, _r$1, _r$2, b, t, y, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; b = $f.b; t = $f.t; y = $f.y; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.Year(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		y = _r$1;
		if (y < 0 || y >= 10000) {
			return [sliceType$3.nil, errors.New("Time.MarshalJSON: year outside of range [0,9999]")];
		}
		b = $makeSlice(sliceType$3, 0, 37);
		b = $append(b, 34);
		_r$2 = t.AppendFormat(b, "2006-01-02T15:04:05.999999999Z07:00"); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		b = _r$2;
		b = $append(b, 34);
		return [b, $ifaceNil];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.MarshalJSON }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.b = b; $f.t = t; $f.y = y; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.MarshalJSON = function() { return this.$val.MarshalJSON(); };
	Time.ptr.prototype.UnmarshalJSON = function(data$1) {
		var $ptr, _r$1, _tuple$1, data$1, err, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; data$1 = $f.data$1; err = $f.err; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		err = $ifaceNil;
		t = this;
		_r$1 = Parse("\"2006-01-02T15:04:05Z07:00\"", $bytesToString(data$1)); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		Time.copy(t, _tuple$1[0]);
		err = _tuple$1[1];
		return err;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.UnmarshalJSON }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.data$1 = data$1; $f.err = err; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.UnmarshalJSON = function(data$1) { return this.$val.UnmarshalJSON(data$1); };
	Time.ptr.prototype.MarshalText = function() {
		var $ptr, _r$1, _r$2, b, t, y, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; b = $f.b; t = $f.t; y = $f.y; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = $clone(this, Time);
		_r$1 = t.Year(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		y = _r$1;
		if (y < 0 || y >= 10000) {
			return [sliceType$3.nil, errors.New("Time.MarshalText: year outside of range [0,9999]")];
		}
		b = $makeSlice(sliceType$3, 0, 35);
		_r$2 = t.AppendFormat(b, "2006-01-02T15:04:05.999999999Z07:00"); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return [_r$2, $ifaceNil];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.MarshalText }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.b = b; $f.t = t; $f.y = y; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.MarshalText = function() { return this.$val.MarshalText(); };
	Time.ptr.prototype.UnmarshalText = function(data$1) {
		var $ptr, _r$1, _tuple$1, data$1, err, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _tuple$1 = $f._tuple$1; data$1 = $f.data$1; err = $f.err; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		err = $ifaceNil;
		t = this;
		_r$1 = Parse("2006-01-02T15:04:05Z07:00", $bytesToString(data$1)); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$1 = _r$1;
		Time.copy(t, _tuple$1[0]);
		err = _tuple$1[1];
		return err;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Time.ptr.prototype.UnmarshalText }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._tuple$1 = _tuple$1; $f.data$1 = data$1; $f.err = err; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	Time.prototype.UnmarshalText = function(data$1) { return this.$val.UnmarshalText(data$1); };
	Unix = function(sec, nsec) {
		var $ptr, n, nsec, sec, x, x$1, x$2, x$3;
		if ((nsec.$high < 0 || (nsec.$high === 0 && nsec.$low < 0)) || (nsec.$high > 0 || (nsec.$high === 0 && nsec.$low >= 1000000000))) {
			n = $div64(nsec, new $Int64(0, 1000000000), false);
			sec = (x = n, new $Int64(sec.$high + x.$high, sec.$low + x.$low));
			nsec = (x$1 = $mul64(n, new $Int64(0, 1000000000)), new $Int64(nsec.$high - x$1.$high, nsec.$low - x$1.$low));
			if ((nsec.$high < 0 || (nsec.$high === 0 && nsec.$low < 0))) {
				nsec = (x$2 = new $Int64(0, 1000000000), new $Int64(nsec.$high + x$2.$high, nsec.$low + x$2.$low));
				sec = (x$3 = new $Int64(0, 1), new $Int64(sec.$high - x$3.$high, sec.$low - x$3.$low));
			}
		}
		return new Time.ptr(new $Int64(sec.$high + 14, sec.$low + 2006054656), ((nsec.$low + ((nsec.$high >> 31) * 4294967296)) >> 0), $pkg.Local);
	};
	$pkg.Unix = Unix;
	isLeap = function(year) {
		var $ptr, _r$1, _r$2, _r$3, year;
		return ((_r$1 = year % 4, _r$1 === _r$1 ? _r$1 : $throwRuntimeError("integer divide by zero")) === 0) && (!(((_r$2 = year % 100, _r$2 === _r$2 ? _r$2 : $throwRuntimeError("integer divide by zero")) === 0)) || ((_r$3 = year % 400, _r$3 === _r$3 ? _r$3 : $throwRuntimeError("integer divide by zero")) === 0));
	};
	norm = function(hi, lo, base) {
		var $ptr, _q, _q$1, _tmp, _tmp$1, base, hi, lo, n, n$1, nhi, nlo;
		nhi = 0;
		nlo = 0;
		if (lo < 0) {
			n = (_q = ((-lo - 1 >> 0)) / base, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) + 1 >> 0;
			hi = hi - (n) >> 0;
			lo = lo + (($imul(n, base))) >> 0;
		}
		if (lo >= base) {
			n$1 = (_q$1 = lo / base, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >> 0 : $throwRuntimeError("integer divide by zero"));
			hi = hi + (n$1) >> 0;
			lo = lo - (($imul(n$1, base))) >> 0;
		}
		_tmp = hi;
		_tmp$1 = lo;
		nhi = _tmp;
		nlo = _tmp$1;
		return [nhi, nlo];
	};
	Date = function(year, month, day, hour, min, sec, nsec, loc) {
		var $ptr, _r$1, _r$2, _r$3, _tuple$1, _tuple$2, _tuple$3, _tuple$4, _tuple$5, _tuple$6, _tuple$7, _tuple$8, abs, d, day, end, hour, loc, m, min, month, n, nsec, offset, sec, start, unix, utc, x, x$1, x$10, x$11, x$12, x$13, x$14, x$15, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, y, year, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _tuple$1 = $f._tuple$1; _tuple$2 = $f._tuple$2; _tuple$3 = $f._tuple$3; _tuple$4 = $f._tuple$4; _tuple$5 = $f._tuple$5; _tuple$6 = $f._tuple$6; _tuple$7 = $f._tuple$7; _tuple$8 = $f._tuple$8; abs = $f.abs; d = $f.d; day = $f.day; end = $f.end; hour = $f.hour; loc = $f.loc; m = $f.m; min = $f.min; month = $f.month; n = $f.n; nsec = $f.nsec; offset = $f.offset; sec = $f.sec; start = $f.start; unix = $f.unix; utc = $f.utc; x = $f.x; x$1 = $f.x$1; x$10 = $f.x$10; x$11 = $f.x$11; x$12 = $f.x$12; x$13 = $f.x$13; x$14 = $f.x$14; x$15 = $f.x$15; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; x$6 = $f.x$6; x$7 = $f.x$7; x$8 = $f.x$8; x$9 = $f.x$9; y = $f.y; year = $f.year; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		if (loc === ptrType$1.nil) {
			$panic(new $String("time: missing Location in call to Date"));
		}
		m = (month >> 0) - 1 >> 0;
		_tuple$1 = norm(year, m, 12);
		year = _tuple$1[0];
		m = _tuple$1[1];
		month = (m >> 0) + 1 >> 0;
		_tuple$2 = norm(sec, nsec, 1000000000);
		sec = _tuple$2[0];
		nsec = _tuple$2[1];
		_tuple$3 = norm(min, sec, 60);
		min = _tuple$3[0];
		sec = _tuple$3[1];
		_tuple$4 = norm(hour, min, 60);
		hour = _tuple$4[0];
		min = _tuple$4[1];
		_tuple$5 = norm(day, hour, 24);
		day = _tuple$5[0];
		hour = _tuple$5[1];
		y = (x = (x$1 = new $Int64(0, year), new $Int64(x$1.$high - -69, x$1.$low - 4075721025)), new $Uint64(x.$high, x.$low));
		n = $div64(y, new $Uint64(0, 400), false);
		y = (x$2 = $mul64(new $Uint64(0, 400), n), new $Uint64(y.$high - x$2.$high, y.$low - x$2.$low));
		d = $mul64(new $Uint64(0, 146097), n);
		n = $div64(y, new $Uint64(0, 100), false);
		y = (x$3 = $mul64(new $Uint64(0, 100), n), new $Uint64(y.$high - x$3.$high, y.$low - x$3.$low));
		d = (x$4 = $mul64(new $Uint64(0, 36524), n), new $Uint64(d.$high + x$4.$high, d.$low + x$4.$low));
		n = $div64(y, new $Uint64(0, 4), false);
		y = (x$5 = $mul64(new $Uint64(0, 4), n), new $Uint64(y.$high - x$5.$high, y.$low - x$5.$low));
		d = (x$6 = $mul64(new $Uint64(0, 1461), n), new $Uint64(d.$high + x$6.$high, d.$low + x$6.$low));
		n = y;
		d = (x$7 = $mul64(new $Uint64(0, 365), n), new $Uint64(d.$high + x$7.$high, d.$low + x$7.$low));
		d = (x$8 = new $Uint64(0, (x$9 = month - 1 >> 0, ((x$9 < 0 || x$9 >= daysBefore.length) ? $throwRuntimeError("index out of range") : daysBefore[x$9]))), new $Uint64(d.$high + x$8.$high, d.$low + x$8.$low));
		if (isLeap(year) && month >= 3) {
			d = (x$10 = new $Uint64(0, 1), new $Uint64(d.$high + x$10.$high, d.$low + x$10.$low));
		}
		d = (x$11 = new $Uint64(0, (day - 1 >> 0)), new $Uint64(d.$high + x$11.$high, d.$low + x$11.$low));
		abs = $mul64(d, new $Uint64(0, 86400));
		abs = (x$12 = new $Uint64(0, ((($imul(hour, 3600)) + ($imul(min, 60)) >> 0) + sec >> 0)), new $Uint64(abs.$high + x$12.$high, abs.$low + x$12.$low));
		unix = (x$13 = new $Int64(abs.$high, abs.$low), new $Int64(x$13.$high + -2147483647, x$13.$low + 3844486912));
		_r$1 = loc.lookup(unix); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple$6 = _r$1;
		offset = _tuple$6[1];
		start = _tuple$6[3];
		end = _tuple$6[4];
		/* */ if (!((offset === 0))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!((offset === 0))) { */ case 2:
				utc = (x$14 = new $Int64(0, offset), new $Int64(unix.$high - x$14.$high, unix.$low - x$14.$low));
				/* */ if ((utc.$high < start.$high || (utc.$high === start.$high && utc.$low < start.$low))) { $s = 5; continue; }
				/* */ if ((utc.$high > end.$high || (utc.$high === end.$high && utc.$low >= end.$low))) { $s = 6; continue; }
				/* */ $s = 7; continue;
				/* if ((utc.$high < start.$high || (utc.$high === start.$high && utc.$low < start.$low))) { */ case 5:
					_r$2 = loc.lookup(new $Int64(start.$high - 0, start.$low - 1)); /* */ $s = 8; case 8: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					_tuple$7 = _r$2;
					offset = _tuple$7[1];
					$s = 7; continue;
				/* } else if ((utc.$high > end.$high || (utc.$high === end.$high && utc.$low >= end.$low))) { */ case 6:
					_r$3 = loc.lookup(end); /* */ $s = 9; case 9: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
					_tuple$8 = _r$3;
					offset = _tuple$8[1];
				/* } */ case 7:
			case 4:
			unix = (x$15 = new $Int64(0, offset), new $Int64(unix.$high - x$15.$high, unix.$low - x$15.$low));
		/* } */ case 3:
		return new Time.ptr(new $Int64(unix.$high + 14, unix.$low + 2006054656), (nsec >> 0), loc);
		/* */ } return; } if ($f === undefined) { $f = { $blk: Date }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._tuple$1 = _tuple$1; $f._tuple$2 = _tuple$2; $f._tuple$3 = _tuple$3; $f._tuple$4 = _tuple$4; $f._tuple$5 = _tuple$5; $f._tuple$6 = _tuple$6; $f._tuple$7 = _tuple$7; $f._tuple$8 = _tuple$8; $f.abs = abs; $f.d = d; $f.day = day; $f.end = end; $f.hour = hour; $f.loc = loc; $f.m = m; $f.min = min; $f.month = month; $f.n = n; $f.nsec = nsec; $f.offset = offset; $f.sec = sec; $f.start = start; $f.unix = unix; $f.utc = utc; $f.x = x; $f.x$1 = x$1; $f.x$10 = x$10; $f.x$11 = x$11; $f.x$12 = x$12; $f.x$13 = x$13; $f.x$14 = x$14; $f.x$15 = x$15; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.x$6 = x$6; $f.x$7 = x$7; $f.x$8 = x$8; $f.x$9 = x$9; $f.y = y; $f.year = year; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Date = Date;
	Time.ptr.prototype.Truncate = function(d) {
		var $ptr, _tuple$1, d, r, t;
		t = $clone(this, Time);
		if ((d.$high < 0 || (d.$high === 0 && d.$low <= 0))) {
			return t;
		}
		_tuple$1 = div(t, d);
		r = _tuple$1[1];
		return t.Add(new Duration(-r.$high, -r.$low));
	};
	Time.prototype.Truncate = function(d) { return this.$val.Truncate(d); };
	Time.ptr.prototype.Round = function(d) {
		var $ptr, _tuple$1, d, r, t, x;
		t = $clone(this, Time);
		if ((d.$high < 0 || (d.$high === 0 && d.$low <= 0))) {
			return t;
		}
		_tuple$1 = div(t, d);
		r = _tuple$1[1];
		if ((x = new Duration(r.$high + r.$high, r.$low + r.$low), (x.$high < d.$high || (x.$high === d.$high && x.$low < d.$low)))) {
			return t.Add(new Duration(-r.$high, -r.$low));
		}
		return t.Add(new Duration(d.$high - r.$high, d.$low - r.$low));
	};
	Time.prototype.Round = function(d) { return this.$val.Round(d); };
	div = function(t, d) {
		var $ptr, _q, _r$1, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, d, d0, d1, d1$1, neg, nsec, qmod2, r, sec, t, tmp, u0, u0x, u1, x, x$1, x$10, x$11, x$12, x$13, x$14, x$15, x$16, x$17, x$18, x$19, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		qmod2 = 0;
		r = new Duration(0, 0);
		t = $clone(t, Time);
		neg = false;
		nsec = t.nsec;
		if ((x = t.sec, (x.$high < 0 || (x.$high === 0 && x.$low < 0)))) {
			neg = true;
			t.sec = (x$1 = t.sec, new $Int64(-x$1.$high, -x$1.$low));
			nsec = -nsec;
			if (nsec < 0) {
				nsec = nsec + (1000000000) >> 0;
				t.sec = (x$2 = t.sec, x$3 = new $Int64(0, 1), new $Int64(x$2.$high - x$3.$high, x$2.$low - x$3.$low));
			}
		}
		if ((d.$high < 0 || (d.$high === 0 && d.$low < 1000000000)) && (x$4 = $div64(new Duration(0, 1000000000), (new Duration(d.$high + d.$high, d.$low + d.$low)), true), (x$4.$high === 0 && x$4.$low === 0))) {
			qmod2 = ((_q = nsec / ((d.$low + ((d.$high >> 31) * 4294967296)) >> 0), (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) >> 0) & 1;
			r = new Duration(0, (_r$1 = nsec % ((d.$low + ((d.$high >> 31) * 4294967296)) >> 0), _r$1 === _r$1 ? _r$1 : $throwRuntimeError("integer divide by zero")));
		} else if ((x$5 = $div64(d, new Duration(0, 1000000000), true), (x$5.$high === 0 && x$5.$low === 0))) {
			d1 = (x$6 = $div64(d, new Duration(0, 1000000000), false), new $Int64(x$6.$high, x$6.$low));
			qmod2 = ((x$7 = $div64(t.sec, d1, false), x$7.$low + ((x$7.$high >> 31) * 4294967296)) >> 0) & 1;
			r = (x$8 = $mul64((x$9 = $div64(t.sec, d1, true), new Duration(x$9.$high, x$9.$low)), new Duration(0, 1000000000)), x$10 = new Duration(0, nsec), new Duration(x$8.$high + x$10.$high, x$8.$low + x$10.$low));
		} else {
			sec = (x$11 = t.sec, new $Uint64(x$11.$high, x$11.$low));
			tmp = $mul64(($shiftRightUint64(sec, 32)), new $Uint64(0, 1000000000));
			u1 = $shiftRightUint64(tmp, 32);
			u0 = $shiftLeft64(tmp, 32);
			tmp = $mul64(new $Uint64(sec.$high & 0, (sec.$low & 4294967295) >>> 0), new $Uint64(0, 1000000000));
			_tmp = u0;
			_tmp$1 = new $Uint64(u0.$high + tmp.$high, u0.$low + tmp.$low);
			u0x = _tmp;
			u0 = _tmp$1;
			if ((u0.$high < u0x.$high || (u0.$high === u0x.$high && u0.$low < u0x.$low))) {
				u1 = (x$12 = new $Uint64(0, 1), new $Uint64(u1.$high + x$12.$high, u1.$low + x$12.$low));
			}
			_tmp$2 = u0;
			_tmp$3 = (x$13 = new $Uint64(0, nsec), new $Uint64(u0.$high + x$13.$high, u0.$low + x$13.$low));
			u0x = _tmp$2;
			u0 = _tmp$3;
			if ((u0.$high < u0x.$high || (u0.$high === u0x.$high && u0.$low < u0x.$low))) {
				u1 = (x$14 = new $Uint64(0, 1), new $Uint64(u1.$high + x$14.$high, u1.$low + x$14.$low));
			}
			d1$1 = new $Uint64(d.$high, d.$low);
			while (true) {
				if (!(!((x$15 = $shiftRightUint64(d1$1, 63), (x$15.$high === 0 && x$15.$low === 1))))) { break; }
				d1$1 = $shiftLeft64(d1$1, (1));
			}
			d0 = new $Uint64(0, 0);
			while (true) {
				qmod2 = 0;
				if ((u1.$high > d1$1.$high || (u1.$high === d1$1.$high && u1.$low > d1$1.$low)) || (u1.$high === d1$1.$high && u1.$low === d1$1.$low) && (u0.$high > d0.$high || (u0.$high === d0.$high && u0.$low >= d0.$low))) {
					qmod2 = 1;
					_tmp$4 = u0;
					_tmp$5 = new $Uint64(u0.$high - d0.$high, u0.$low - d0.$low);
					u0x = _tmp$4;
					u0 = _tmp$5;
					if ((u0.$high > u0x.$high || (u0.$high === u0x.$high && u0.$low > u0x.$low))) {
						u1 = (x$16 = new $Uint64(0, 1), new $Uint64(u1.$high - x$16.$high, u1.$low - x$16.$low));
					}
					u1 = (x$17 = d1$1, new $Uint64(u1.$high - x$17.$high, u1.$low - x$17.$low));
				}
				if ((d1$1.$high === 0 && d1$1.$low === 0) && (x$18 = new $Uint64(d.$high, d.$low), (d0.$high === x$18.$high && d0.$low === x$18.$low))) {
					break;
				}
				d0 = $shiftRightUint64(d0, (1));
				d0 = (x$19 = $shiftLeft64((new $Uint64(d1$1.$high & 0, (d1$1.$low & 1) >>> 0)), 63), new $Uint64(d0.$high | x$19.$high, (d0.$low | x$19.$low) >>> 0));
				d1$1 = $shiftRightUint64(d1$1, (1));
			}
			r = new Duration(u0.$high, u0.$low);
		}
		if (neg && !((r.$high === 0 && r.$low === 0))) {
			qmod2 = (qmod2 ^ (1)) >> 0;
			r = new Duration(d.$high - r.$high, d.$low - r.$low);
		}
		return [qmod2, r];
	};
	Location.ptr.prototype.get = function() {
		var $ptr, l, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; l = $f.l; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		l = this;
		if (l === ptrType$1.nil) {
			return utcLoc;
		}
		/* */ if (l === localLoc) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (l === localLoc) { */ case 1:
			$r = localOnce.Do(initLocal); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* } */ case 2:
		return l;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Location.ptr.prototype.get }; } $f.$ptr = $ptr; $f.l = l; $f.$s = $s; $f.$r = $r; return $f;
	};
	Location.prototype.get = function() { return this.$val.get(); };
	Location.ptr.prototype.String = function() {
		var $ptr, _r$1, l, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; l = $f.l; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		l = this;
		_r$1 = l.get(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r$1.name;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Location.ptr.prototype.String }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f.l = l; $f.$s = $s; $f.$r = $r; return $f;
	};
	Location.prototype.String = function() { return this.$val.String(); };
	FixedZone = function(name, offset) {
		var $ptr, l, name, offset, x;
		l = new Location.ptr(name, new sliceType([new zone.ptr(name, offset, false)]), new sliceType$1([new zoneTrans.ptr(new $Int64(-2147483648, 0), 0, false, false)]), new $Int64(-2147483648, 0), new $Int64(2147483647, 4294967295), ptrType.nil);
		l.cacheZone = (x = l.zone, (0 >= x.$length ? $throwRuntimeError("index out of range") : x.$array[x.$offset + 0]));
		return l;
	};
	$pkg.FixedZone = FixedZone;
	Location.ptr.prototype.lookup = function(sec) {
		var $ptr, _q, _r$1, end, hi, isDST, l, lim, lo, m, name, offset, sec, start, tx, x, x$1, x$2, x$3, x$4, x$5, x$6, x$7, x$8, zone$1, zone$2, zone$3, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _q = $f._q; _r$1 = $f._r$1; end = $f.end; hi = $f.hi; isDST = $f.isDST; l = $f.l; lim = $f.lim; lo = $f.lo; m = $f.m; name = $f.name; offset = $f.offset; sec = $f.sec; start = $f.start; tx = $f.tx; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; x$6 = $f.x$6; x$7 = $f.x$7; x$8 = $f.x$8; zone$1 = $f.zone$1; zone$2 = $f.zone$2; zone$3 = $f.zone$3; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		name = "";
		offset = 0;
		isDST = false;
		start = new $Int64(0, 0);
		end = new $Int64(0, 0);
		l = this;
		_r$1 = l.get(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		l = _r$1;
		if (l.zone.$length === 0) {
			name = "UTC";
			offset = 0;
			isDST = false;
			start = new $Int64(-2147483648, 0);
			end = new $Int64(2147483647, 4294967295);
			return [name, offset, isDST, start, end];
		}
		zone$1 = l.cacheZone;
		if (!(zone$1 === ptrType.nil) && (x = l.cacheStart, (x.$high < sec.$high || (x.$high === sec.$high && x.$low <= sec.$low))) && (x$1 = l.cacheEnd, (sec.$high < x$1.$high || (sec.$high === x$1.$high && sec.$low < x$1.$low)))) {
			name = zone$1.name;
			offset = zone$1.offset;
			isDST = zone$1.isDST;
			start = l.cacheStart;
			end = l.cacheEnd;
			return [name, offset, isDST, start, end];
		}
		if ((l.tx.$length === 0) || (x$2 = (x$3 = l.tx, (0 >= x$3.$length ? $throwRuntimeError("index out of range") : x$3.$array[x$3.$offset + 0])).when, (sec.$high < x$2.$high || (sec.$high === x$2.$high && sec.$low < x$2.$low)))) {
			zone$2 = (x$4 = l.zone, x$5 = l.lookupFirstZone(), ((x$5 < 0 || x$5 >= x$4.$length) ? $throwRuntimeError("index out of range") : x$4.$array[x$4.$offset + x$5]));
			name = zone$2.name;
			offset = zone$2.offset;
			isDST = zone$2.isDST;
			start = new $Int64(-2147483648, 0);
			if (l.tx.$length > 0) {
				end = (x$6 = l.tx, (0 >= x$6.$length ? $throwRuntimeError("index out of range") : x$6.$array[x$6.$offset + 0])).when;
			} else {
				end = new $Int64(2147483647, 4294967295);
			}
			return [name, offset, isDST, start, end];
		}
		tx = l.tx;
		end = new $Int64(2147483647, 4294967295);
		lo = 0;
		hi = tx.$length;
		while (true) {
			if (!((hi - lo >> 0) > 1)) { break; }
			m = lo + (_q = ((hi - lo >> 0)) / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) >> 0;
			lim = ((m < 0 || m >= tx.$length) ? $throwRuntimeError("index out of range") : tx.$array[tx.$offset + m]).when;
			if ((sec.$high < lim.$high || (sec.$high === lim.$high && sec.$low < lim.$low))) {
				end = lim;
				hi = m;
			} else {
				lo = m;
			}
		}
		zone$3 = (x$7 = l.zone, x$8 = ((lo < 0 || lo >= tx.$length) ? $throwRuntimeError("index out of range") : tx.$array[tx.$offset + lo]).index, ((x$8 < 0 || x$8 >= x$7.$length) ? $throwRuntimeError("index out of range") : x$7.$array[x$7.$offset + x$8]));
		name = zone$3.name;
		offset = zone$3.offset;
		isDST = zone$3.isDST;
		start = ((lo < 0 || lo >= tx.$length) ? $throwRuntimeError("index out of range") : tx.$array[tx.$offset + lo]).when;
		return [name, offset, isDST, start, end];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Location.ptr.prototype.lookup }; } $f.$ptr = $ptr; $f._q = _q; $f._r$1 = _r$1; $f.end = end; $f.hi = hi; $f.isDST = isDST; $f.l = l; $f.lim = lim; $f.lo = lo; $f.m = m; $f.name = name; $f.offset = offset; $f.sec = sec; $f.start = start; $f.tx = tx; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.x$6 = x$6; $f.x$7 = x$7; $f.x$8 = x$8; $f.zone$1 = zone$1; $f.zone$2 = zone$2; $f.zone$3 = zone$3; $f.$s = $s; $f.$r = $r; return $f;
	};
	Location.prototype.lookup = function(sec) { return this.$val.lookup(sec); };
	Location.ptr.prototype.lookupFirstZone = function() {
		var $ptr, _i, _ref, l, x, x$1, x$2, x$3, x$4, x$5, zi, zi$1;
		l = this;
		if (!l.firstZoneUsed()) {
			return 0;
		}
		if (l.tx.$length > 0 && (x = l.zone, x$1 = (x$2 = l.tx, (0 >= x$2.$length ? $throwRuntimeError("index out of range") : x$2.$array[x$2.$offset + 0])).index, ((x$1 < 0 || x$1 >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + x$1])).isDST) {
			zi = ((x$3 = l.tx, (0 >= x$3.$length ? $throwRuntimeError("index out of range") : x$3.$array[x$3.$offset + 0])).index >> 0) - 1 >> 0;
			while (true) {
				if (!(zi >= 0)) { break; }
				if (!(x$4 = l.zone, ((zi < 0 || zi >= x$4.$length) ? $throwRuntimeError("index out of range") : x$4.$array[x$4.$offset + zi])).isDST) {
					return zi;
				}
				zi = zi - (1) >> 0;
			}
		}
		_ref = l.zone;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			zi$1 = _i;
			if (!(x$5 = l.zone, ((zi$1 < 0 || zi$1 >= x$5.$length) ? $throwRuntimeError("index out of range") : x$5.$array[x$5.$offset + zi$1])).isDST) {
				return zi$1;
			}
			_i++;
		}
		return 0;
	};
	Location.prototype.lookupFirstZone = function() { return this.$val.lookupFirstZone(); };
	Location.ptr.prototype.firstZoneUsed = function() {
		var $ptr, _i, _ref, l, tx;
		l = this;
		_ref = l.tx;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			tx = $clone(((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]), zoneTrans);
			if (tx.index === 0) {
				return true;
			}
			_i++;
		}
		return false;
	};
	Location.prototype.firstZoneUsed = function() { return this.$val.firstZoneUsed(); };
	Location.ptr.prototype.lookupName = function(name, unix) {
		var $ptr, _i, _i$1, _r$1, _r$2, _ref, _ref$1, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tuple$1, i, i$1, isDST, isDST$1, l, nam, name, offset, offset$1, ok, unix, x, x$1, x$2, zone$1, zone$2, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _i = $f._i; _i$1 = $f._i$1; _r$1 = $f._r$1; _r$2 = $f._r$2; _ref = $f._ref; _ref$1 = $f._ref$1; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tmp$4 = $f._tmp$4; _tmp$5 = $f._tmp$5; _tuple$1 = $f._tuple$1; i = $f.i; i$1 = $f.i$1; isDST = $f.isDST; isDST$1 = $f.isDST$1; l = $f.l; nam = $f.nam; name = $f.name; offset = $f.offset; offset$1 = $f.offset$1; ok = $f.ok; unix = $f.unix; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; zone$1 = $f.zone$1; zone$2 = $f.zone$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		offset = 0;
		isDST = false;
		ok = false;
		l = this;
		_r$1 = l.get(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		l = _r$1;
		_ref = l.zone;
		_i = 0;
		/* while (true) { */ case 2:
			/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 3; continue; }
			i = _i;
			zone$1 = (x = l.zone, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
			/* */ if (zone$1.name === name) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if (zone$1.name === name) { */ case 4:
				_r$2 = l.lookup((x$1 = new $Int64(0, zone$1.offset), new $Int64(unix.$high - x$1.$high, unix.$low - x$1.$low))); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_tuple$1 = _r$2;
				nam = _tuple$1[0];
				offset$1 = _tuple$1[1];
				isDST$1 = _tuple$1[2];
				if (nam === zone$1.name) {
					_tmp = offset$1;
					_tmp$1 = isDST$1;
					_tmp$2 = true;
					offset = _tmp;
					isDST = _tmp$1;
					ok = _tmp$2;
					return [offset, isDST, ok];
				}
			/* } */ case 5:
			_i++;
		/* } */ $s = 2; continue; case 3:
		_ref$1 = l.zone;
		_i$1 = 0;
		while (true) {
			if (!(_i$1 < _ref$1.$length)) { break; }
			i$1 = _i$1;
			zone$2 = (x$2 = l.zone, ((i$1 < 0 || i$1 >= x$2.$length) ? $throwRuntimeError("index out of range") : x$2.$array[x$2.$offset + i$1]));
			if (zone$2.name === name) {
				_tmp$3 = zone$2.offset;
				_tmp$4 = zone$2.isDST;
				_tmp$5 = true;
				offset = _tmp$3;
				isDST = _tmp$4;
				ok = _tmp$5;
				return [offset, isDST, ok];
			}
			_i$1++;
		}
		return [offset, isDST, ok];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Location.ptr.prototype.lookupName }; } $f.$ptr = $ptr; $f._i = _i; $f._i$1 = _i$1; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._ref = _ref; $f._ref$1 = _ref$1; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tmp$4 = _tmp$4; $f._tmp$5 = _tmp$5; $f._tuple$1 = _tuple$1; $f.i = i; $f.i$1 = i$1; $f.isDST = isDST; $f.isDST$1 = isDST$1; $f.l = l; $f.nam = nam; $f.name = name; $f.offset = offset; $f.offset$1 = offset$1; $f.ok = ok; $f.unix = unix; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.zone$1 = zone$1; $f.zone$2 = zone$2; $f.$s = $s; $f.$r = $r; return $f;
	};
	Location.prototype.lookupName = function(name, unix) { return this.$val.lookupName(name, unix); };
	ptrType$3.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	Time.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Format", name: "Format", pkg: "", typ: $funcType([$String], [$String], false)}, {prop: "AppendFormat", name: "AppendFormat", pkg: "", typ: $funcType([sliceType$3, $String], [sliceType$3], false)}, {prop: "After", name: "After", pkg: "", typ: $funcType([Time], [$Bool], false)}, {prop: "Before", name: "Before", pkg: "", typ: $funcType([Time], [$Bool], false)}, {prop: "Equal", name: "Equal", pkg: "", typ: $funcType([Time], [$Bool], false)}, {prop: "IsZero", name: "IsZero", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "abs", name: "abs", pkg: "time", typ: $funcType([], [$Uint64], false)}, {prop: "locabs", name: "locabs", pkg: "time", typ: $funcType([], [$String, $Int, $Uint64], false)}, {prop: "Date", name: "Date", pkg: "", typ: $funcType([], [$Int, Month, $Int], false)}, {prop: "Year", name: "Year", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Month", name: "Month", pkg: "", typ: $funcType([], [Month], false)}, {prop: "Day", name: "Day", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Weekday", name: "Weekday", pkg: "", typ: $funcType([], [Weekday], false)}, {prop: "ISOWeek", name: "ISOWeek", pkg: "", typ: $funcType([], [$Int, $Int], false)}, {prop: "Clock", name: "Clock", pkg: "", typ: $funcType([], [$Int, $Int, $Int], false)}, {prop: "Hour", name: "Hour", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Minute", name: "Minute", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Second", name: "Second", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Nanosecond", name: "Nanosecond", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "YearDay", name: "YearDay", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Add", name: "Add", pkg: "", typ: $funcType([Duration], [Time], false)}, {prop: "Sub", name: "Sub", pkg: "", typ: $funcType([Time], [Duration], false)}, {prop: "AddDate", name: "AddDate", pkg: "", typ: $funcType([$Int, $Int, $Int], [Time], false)}, {prop: "date", name: "date", pkg: "time", typ: $funcType([$Bool], [$Int, Month, $Int, $Int], false)}, {prop: "UTC", name: "UTC", pkg: "", typ: $funcType([], [Time], false)}, {prop: "Local", name: "Local", pkg: "", typ: $funcType([], [Time], false)}, {prop: "In", name: "In", pkg: "", typ: $funcType([ptrType$1], [Time], false)}, {prop: "Location", name: "Location", pkg: "", typ: $funcType([], [ptrType$1], false)}, {prop: "Zone", name: "Zone", pkg: "", typ: $funcType([], [$String, $Int], false)}, {prop: "Unix", name: "Unix", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "UnixNano", name: "UnixNano", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "MarshalBinary", name: "MarshalBinary", pkg: "", typ: $funcType([], [sliceType$3, $error], false)}, {prop: "GobEncode", name: "GobEncode", pkg: "", typ: $funcType([], [sliceType$3, $error], false)}, {prop: "MarshalJSON", name: "MarshalJSON", pkg: "", typ: $funcType([], [sliceType$3, $error], false)}, {prop: "MarshalText", name: "MarshalText", pkg: "", typ: $funcType([], [sliceType$3, $error], false)}, {prop: "Truncate", name: "Truncate", pkg: "", typ: $funcType([Duration], [Time], false)}, {prop: "Round", name: "Round", pkg: "", typ: $funcType([Duration], [Time], false)}];
	ptrType$6.methods = [{prop: "UnmarshalBinary", name: "UnmarshalBinary", pkg: "", typ: $funcType([sliceType$3], [$error], false)}, {prop: "GobDecode", name: "GobDecode", pkg: "", typ: $funcType([sliceType$3], [$error], false)}, {prop: "UnmarshalJSON", name: "UnmarshalJSON", pkg: "", typ: $funcType([sliceType$3], [$error], false)}, {prop: "UnmarshalText", name: "UnmarshalText", pkg: "", typ: $funcType([sliceType$3], [$error], false)}];
	Month.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	Weekday.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	Duration.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Nanoseconds", name: "Nanoseconds", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "Seconds", name: "Seconds", pkg: "", typ: $funcType([], [$Float64], false)}, {prop: "Minutes", name: "Minutes", pkg: "", typ: $funcType([], [$Float64], false)}, {prop: "Hours", name: "Hours", pkg: "", typ: $funcType([], [$Float64], false)}];
	ptrType$1.methods = [{prop: "get", name: "get", pkg: "time", typ: $funcType([], [ptrType$1], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "lookup", name: "lookup", pkg: "time", typ: $funcType([$Int64], [$String, $Int, $Bool, $Int64, $Int64], false)}, {prop: "lookupFirstZone", name: "lookupFirstZone", pkg: "time", typ: $funcType([], [$Int], false)}, {prop: "firstZoneUsed", name: "firstZoneUsed", pkg: "time", typ: $funcType([], [$Bool], false)}, {prop: "lookupName", name: "lookupName", pkg: "time", typ: $funcType([$String, $Int64], [$Int, $Bool, $Bool], false)}];
	ParseError.init([{prop: "Layout", name: "Layout", pkg: "", typ: $String, tag: ""}, {prop: "Value", name: "Value", pkg: "", typ: $String, tag: ""}, {prop: "LayoutElem", name: "LayoutElem", pkg: "", typ: $String, tag: ""}, {prop: "ValueElem", name: "ValueElem", pkg: "", typ: $String, tag: ""}, {prop: "Message", name: "Message", pkg: "", typ: $String, tag: ""}]);
	Time.init([{prop: "sec", name: "sec", pkg: "time", typ: $Int64, tag: ""}, {prop: "nsec", name: "nsec", pkg: "time", typ: $Int32, tag: ""}, {prop: "loc", name: "loc", pkg: "time", typ: ptrType$1, tag: ""}]);
	Location.init([{prop: "name", name: "name", pkg: "time", typ: $String, tag: ""}, {prop: "zone", name: "zone", pkg: "time", typ: sliceType, tag: ""}, {prop: "tx", name: "tx", pkg: "time", typ: sliceType$1, tag: ""}, {prop: "cacheStart", name: "cacheStart", pkg: "time", typ: $Int64, tag: ""}, {prop: "cacheEnd", name: "cacheEnd", pkg: "time", typ: $Int64, tag: ""}, {prop: "cacheZone", name: "cacheZone", pkg: "time", typ: ptrType, tag: ""}]);
	zone.init([{prop: "name", name: "name", pkg: "time", typ: $String, tag: ""}, {prop: "offset", name: "offset", pkg: "time", typ: $Int, tag: ""}, {prop: "isDST", name: "isDST", pkg: "time", typ: $Bool, tag: ""}]);
	zoneTrans.init([{prop: "when", name: "when", pkg: "time", typ: $Int64, tag: ""}, {prop: "index", name: "index", pkg: "time", typ: $Uint8, tag: ""}, {prop: "isstd", name: "isstd", pkg: "time", typ: $Bool, tag: ""}, {prop: "isutc", name: "isutc", pkg: "time", typ: $Bool, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = js.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = nosync.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = runtime.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = strings.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = syscall.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		localLoc = new Location.ptr("", sliceType.nil, sliceType$1.nil, new $Int64(0, 0), new $Int64(0, 0), ptrType.nil);
		localOnce = new nosync.Once.ptr(false, false);
		std0x = $toNativeArray($kindInt, [260, 265, 524, 526, 528, 274]);
		longDayNames = new sliceType$2(["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]);
		shortDayNames = new sliceType$2(["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]);
		shortMonthNames = new sliceType$2(["---", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]);
		longMonthNames = new sliceType$2(["---", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]);
		atoiError = errors.New("time: invalid number");
		errBad = errors.New("bad value for field");
		errLeadingInt = errors.New("time: bad [0-9]*");
		months = $toNativeArray($kindString, ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]);
		days = $toNativeArray($kindString, ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]);
		daysBefore = $toNativeArray($kindInt32, [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]);
		utcLoc = new Location.ptr("UTC", sliceType.nil, sliceType$1.nil, new $Int64(0, 0), new $Int64(0, 0), ptrType.nil);
		$pkg.UTC = utcLoc;
		$pkg.Local = localLoc;
		_r = syscall.Getenv("ZONEINFO"); /* */ $s = 7; case 7: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		zoneinfo = _tuple[0];
		badData = errors.New("malformed time zone information");
		init();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["os"] = (function() {
	var $pkg = {}, $init, errors, js, io, runtime, sync, atomic, syscall, time, PathError, SyscallError, LinkError, File, file, dirInfo, FileInfo, FileMode, fileStat, sliceType, ptrType, sliceType$1, sliceType$2, ptrType$1, ptrType$2, ptrType$3, ptrType$4, arrayType, ptrType$12, funcType$1, ptrType$13, arrayType$2, arrayType$5, ptrType$15, errFinished, lstat, useSyscallwd, supportsCloseOnExec, runtime_args, init, NewSyscallError, IsNotExist, isNotExist, fixCount, sigpipe, syscallMode, NewFile, epipecheck, Lstat, basename, init$1, useSyscallwdDarwin, init$2, fillFileStatFromSys, timespecToTime, init$3;
	errors = $packages["errors"];
	js = $packages["github.com/gopherjs/gopherjs/js"];
	io = $packages["io"];
	runtime = $packages["runtime"];
	sync = $packages["sync"];
	atomic = $packages["sync/atomic"];
	syscall = $packages["syscall"];
	time = $packages["time"];
	PathError = $pkg.PathError = $newType(0, $kindStruct, "os.PathError", "PathError", "os", function(Op_, Path_, Err_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Op = "";
			this.Path = "";
			this.Err = $ifaceNil;
			return;
		}
		this.Op = Op_;
		this.Path = Path_;
		this.Err = Err_;
	});
	SyscallError = $pkg.SyscallError = $newType(0, $kindStruct, "os.SyscallError", "SyscallError", "os", function(Syscall_, Err_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Syscall = "";
			this.Err = $ifaceNil;
			return;
		}
		this.Syscall = Syscall_;
		this.Err = Err_;
	});
	LinkError = $pkg.LinkError = $newType(0, $kindStruct, "os.LinkError", "LinkError", "os", function(Op_, Old_, New_, Err_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Op = "";
			this.Old = "";
			this.New = "";
			this.Err = $ifaceNil;
			return;
		}
		this.Op = Op_;
		this.Old = Old_;
		this.New = New_;
		this.Err = Err_;
	});
	File = $pkg.File = $newType(0, $kindStruct, "os.File", "File", "os", function(file_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.file = ptrType$12.nil;
			return;
		}
		this.file = file_;
	});
	file = $pkg.file = $newType(0, $kindStruct, "os.file", "file", "os", function(fd_, name_, dirinfo_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.fd = 0;
			this.name = "";
			this.dirinfo = ptrType.nil;
			return;
		}
		this.fd = fd_;
		this.name = name_;
		this.dirinfo = dirinfo_;
	});
	dirInfo = $pkg.dirInfo = $newType(0, $kindStruct, "os.dirInfo", "dirInfo", "os", function(buf_, nbuf_, bufp_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.buf = sliceType$1.nil;
			this.nbuf = 0;
			this.bufp = 0;
			return;
		}
		this.buf = buf_;
		this.nbuf = nbuf_;
		this.bufp = bufp_;
	});
	FileInfo = $pkg.FileInfo = $newType(8, $kindInterface, "os.FileInfo", "FileInfo", "os", null);
	FileMode = $pkg.FileMode = $newType(4, $kindUint32, "os.FileMode", "FileMode", "os", null);
	fileStat = $pkg.fileStat = $newType(0, $kindStruct, "os.fileStat", "fileStat", "os", function(name_, size_, mode_, modTime_, sys_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = "";
			this.size = new $Int64(0, 0);
			this.mode = 0;
			this.modTime = new time.Time.ptr(new $Int64(0, 0), 0, ptrType$13.nil);
			this.sys = new syscall.Stat_t.ptr(0, 0, 0, new $Uint64(0, 0), 0, 0, 0, arrayType.zero(), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new $Int64(0, 0), new $Int64(0, 0), 0, 0, 0, 0, arrayType$2.zero());
			return;
		}
		this.name = name_;
		this.size = size_;
		this.mode = mode_;
		this.modTime = modTime_;
		this.sys = sys_;
	});
	sliceType = $sliceType($String);
	ptrType = $ptrType(dirInfo);
	sliceType$1 = $sliceType($Uint8);
	sliceType$2 = $sliceType(FileInfo);
	ptrType$1 = $ptrType(File);
	ptrType$2 = $ptrType(PathError);
	ptrType$3 = $ptrType(LinkError);
	ptrType$4 = $ptrType(SyscallError);
	arrayType = $arrayType($Uint8, 4);
	ptrType$12 = $ptrType(file);
	funcType$1 = $funcType([ptrType$12], [$error], false);
	ptrType$13 = $ptrType(time.Location);
	arrayType$2 = $arrayType($Int64, 2);
	arrayType$5 = $arrayType($Uint8, 32);
	ptrType$15 = $ptrType(fileStat);
	runtime_args = function() {
		var $ptr;
		return $pkg.Args;
	};
	init = function() {
		var $ptr, argv, i, process;
		process = $global.process;
		if (!(process === undefined)) {
			argv = process.argv;
			$pkg.Args = $makeSlice(sliceType, ($parseInt(argv.length) - 1 >> 0));
			i = 0;
			while (true) {
				if (!(i < ($parseInt(argv.length) - 1 >> 0))) { break; }
				((i < 0 || i >= $pkg.Args.$length) ? $throwRuntimeError("index out of range") : $pkg.Args.$array[$pkg.Args.$offset + i] = $internalize(argv[(i + 1 >> 0)], $String));
				i = i + (1) >> 0;
			}
		}
		if ($pkg.Args.$length === 0) {
			$pkg.Args = new sliceType(["?"]);
		}
	};
	File.ptr.prototype.readdirnames = function(n) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tuple, _tuple$1, _tuple$2, d, err, errno, f, n, names, nb, nc, size;
		names = sliceType.nil;
		err = $ifaceNil;
		f = this;
		if (f.file.dirinfo === ptrType.nil) {
			f.file.dirinfo = new dirInfo.ptr(sliceType$1.nil, 0, 0);
			f.file.dirinfo.buf = $makeSlice(sliceType$1, 4096);
		}
		d = f.file.dirinfo;
		size = n;
		if (size <= 0) {
			size = 100;
			n = -1;
		}
		names = $makeSlice(sliceType, 0, size);
		while (true) {
			if (!(!((n === 0)))) { break; }
			if (d.bufp >= d.nbuf) {
				d.bufp = 0;
				errno = $ifaceNil;
				_tuple$1 = syscall.ReadDirent(f.file.fd, d.buf);
				_tuple = fixCount(_tuple$1[0], _tuple$1[1]);
				d.nbuf = _tuple[0];
				errno = _tuple[1];
				if (!($interfaceIsEqual(errno, $ifaceNil))) {
					_tmp = names;
					_tmp$1 = NewSyscallError("readdirent", errno);
					names = _tmp;
					err = _tmp$1;
					return [names, err];
				}
				if (d.nbuf <= 0) {
					break;
				}
			}
			_tmp$2 = 0;
			_tmp$3 = 0;
			nb = _tmp$2;
			nc = _tmp$3;
			_tuple$2 = syscall.ParseDirent($subslice(d.buf, d.bufp, d.nbuf), n, names);
			nb = _tuple$2[0];
			nc = _tuple$2[1];
			names = _tuple$2[2];
			d.bufp = d.bufp + (nb) >> 0;
			n = n - (nc) >> 0;
		}
		if (n >= 0 && (names.$length === 0)) {
			_tmp$4 = names;
			_tmp$5 = io.EOF;
			names = _tmp$4;
			err = _tmp$5;
			return [names, err];
		}
		_tmp$6 = names;
		_tmp$7 = $ifaceNil;
		names = _tmp$6;
		err = _tmp$7;
		return [names, err];
	};
	File.prototype.readdirnames = function(n) { return this.$val.readdirnames(n); };
	File.ptr.prototype.Readdir = function(n) {
		var $ptr, _r, _tmp, _tmp$1, _tuple, err, f, fi, n, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tuple = $f._tuple; err = $f.err; f = $f.f; fi = $f.fi; n = $f.n; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		fi = sliceType$2.nil;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = sliceType$2.nil;
			_tmp$1 = $pkg.ErrInvalid;
			fi = _tmp;
			err = _tmp$1;
			return [fi, err];
		}
		_r = f.readdir(n); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		fi = _tuple[0];
		err = _tuple[1];
		/* */ $s = 2; case 2:
		return [fi, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: File.ptr.prototype.Readdir }; } $f.$ptr = $ptr; $f._r = _r; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tuple = _tuple; $f.err = err; $f.f = f; $f.fi = fi; $f.n = n; $f.$s = $s; $f.$r = $r; return $f;
	};
	File.prototype.Readdir = function(n) { return this.$val.Readdir(n); };
	File.ptr.prototype.Readdirnames = function(n) {
		var $ptr, _tmp, _tmp$1, _tuple, err, f, n, names;
		names = sliceType.nil;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = sliceType.nil;
			_tmp$1 = $pkg.ErrInvalid;
			names = _tmp;
			err = _tmp$1;
			return [names, err];
		}
		_tuple = f.readdirnames(n);
		names = _tuple[0];
		err = _tuple[1];
		return [names, err];
	};
	File.prototype.Readdirnames = function(n) { return this.$val.Readdirnames(n); };
	PathError.ptr.prototype.Error = function() {
		var $ptr, _r, e, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; e = $f.e; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		e = this;
		_r = e.Err.Error(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return e.Op + " " + e.Path + ": " + _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: PathError.ptr.prototype.Error }; } $f.$ptr = $ptr; $f._r = _r; $f.e = e; $f.$s = $s; $f.$r = $r; return $f;
	};
	PathError.prototype.Error = function() { return this.$val.Error(); };
	SyscallError.ptr.prototype.Error = function() {
		var $ptr, _r, e, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; e = $f.e; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		e = this;
		_r = e.Err.Error(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return e.Syscall + ": " + _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: SyscallError.ptr.prototype.Error }; } $f.$ptr = $ptr; $f._r = _r; $f.e = e; $f.$s = $s; $f.$r = $r; return $f;
	};
	SyscallError.prototype.Error = function() { return this.$val.Error(); };
	NewSyscallError = function(syscall$1, err) {
		var $ptr, err, syscall$1;
		if ($interfaceIsEqual(err, $ifaceNil)) {
			return $ifaceNil;
		}
		return new SyscallError.ptr(syscall$1, err);
	};
	$pkg.NewSyscallError = NewSyscallError;
	IsNotExist = function(err) {
		var $ptr, err;
		return isNotExist(err);
	};
	$pkg.IsNotExist = IsNotExist;
	isNotExist = function(err) {
		var $ptr, _ref, err, pe, pe$1, pe$2, pe$3;
		_ref = err;
		if (_ref === $ifaceNil) {
			pe = _ref;
			return false;
		} else if ($assertType(_ref, ptrType$2, true)[1]) {
			pe$1 = _ref.$val;
			err = pe$1.Err;
		} else if ($assertType(_ref, ptrType$3, true)[1]) {
			pe$2 = _ref.$val;
			err = pe$2.Err;
		} else if ($assertType(_ref, ptrType$4, true)[1]) {
			pe$3 = _ref.$val;
			err = pe$3.Err;
		}
		return $interfaceIsEqual(err, new syscall.Errno(2)) || $interfaceIsEqual(err, $pkg.ErrNotExist);
	};
	File.ptr.prototype.Name = function() {
		var $ptr, f;
		f = this;
		return f.file.name;
	};
	File.prototype.Name = function() { return this.$val.Name(); };
	LinkError.ptr.prototype.Error = function() {
		var $ptr, _r, e, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; e = $f.e; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		e = this;
		_r = e.Err.Error(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return e.Op + " " + e.Old + " " + e.New + ": " + _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: LinkError.ptr.prototype.Error }; } $f.$ptr = $ptr; $f._r = _r; $f.e = e; $f.$s = $s; $f.$r = $r; return $f;
	};
	LinkError.prototype.Error = function() { return this.$val.Error(); };
	File.ptr.prototype.Read = function(b) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tuple, b, e, err, f, n;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = 0;
			_tmp$1 = $pkg.ErrInvalid;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		_tuple = f.read(b);
		n = _tuple[0];
		e = _tuple[1];
		if ((n === 0) && b.$length > 0 && $interfaceIsEqual(e, $ifaceNil)) {
			_tmp$2 = 0;
			_tmp$3 = io.EOF;
			n = _tmp$2;
			err = _tmp$3;
			return [n, err];
		}
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			err = new PathError.ptr("read", f.file.name, e);
		}
		_tmp$4 = n;
		_tmp$5 = err;
		n = _tmp$4;
		err = _tmp$5;
		return [n, err];
	};
	File.prototype.Read = function(b) { return this.$val.Read(b); };
	File.ptr.prototype.ReadAt = function(b, off) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tuple, b, e, err, f, m, n, off, x;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = 0;
			_tmp$1 = $pkg.ErrInvalid;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		while (true) {
			if (!(b.$length > 0)) { break; }
			_tuple = f.pread(b, off);
			m = _tuple[0];
			e = _tuple[1];
			if ((m === 0) && $interfaceIsEqual(e, $ifaceNil)) {
				_tmp$2 = n;
				_tmp$3 = io.EOF;
				n = _tmp$2;
				err = _tmp$3;
				return [n, err];
			}
			if (!($interfaceIsEqual(e, $ifaceNil))) {
				err = new PathError.ptr("read", f.file.name, e);
				break;
			}
			n = n + (m) >> 0;
			b = $subslice(b, m);
			off = (x = new $Int64(0, m), new $Int64(off.$high + x.$high, off.$low + x.$low));
		}
		return [n, err];
	};
	File.prototype.ReadAt = function(b, off) { return this.$val.ReadAt(b, off); };
	File.ptr.prototype.Write = function(b) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tuple, b, e, err, f, n;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = 0;
			_tmp$1 = $pkg.ErrInvalid;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		_tuple = f.write(b);
		n = _tuple[0];
		e = _tuple[1];
		if (n < 0) {
			n = 0;
		}
		if (!((n === b.$length))) {
			err = io.ErrShortWrite;
		}
		epipecheck(f, e);
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			err = new PathError.ptr("write", f.file.name, e);
		}
		_tmp$2 = n;
		_tmp$3 = err;
		n = _tmp$2;
		err = _tmp$3;
		return [n, err];
	};
	File.prototype.Write = function(b) { return this.$val.Write(b); };
	File.ptr.prototype.WriteAt = function(b, off) {
		var $ptr, _tmp, _tmp$1, _tuple, b, e, err, f, m, n, off, x;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = 0;
			_tmp$1 = $pkg.ErrInvalid;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		while (true) {
			if (!(b.$length > 0)) { break; }
			_tuple = f.pwrite(b, off);
			m = _tuple[0];
			e = _tuple[1];
			if (!($interfaceIsEqual(e, $ifaceNil))) {
				err = new PathError.ptr("write", f.file.name, e);
				break;
			}
			n = n + (m) >> 0;
			b = $subslice(b, m);
			off = (x = new $Int64(0, m), new $Int64(off.$high + x.$high, off.$low + x.$low));
		}
		return [n, err];
	};
	File.prototype.WriteAt = function(b, off) { return this.$val.WriteAt(b, off); };
	File.ptr.prototype.Seek = function(offset, whence) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tuple, e, err, f, offset, r, ret, whence;
		ret = new $Int64(0, 0);
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = new $Int64(0, 0);
			_tmp$1 = $pkg.ErrInvalid;
			ret = _tmp;
			err = _tmp$1;
			return [ret, err];
		}
		_tuple = f.seek(offset, whence);
		r = _tuple[0];
		e = _tuple[1];
		if ($interfaceIsEqual(e, $ifaceNil) && !(f.file.dirinfo === ptrType.nil) && !((r.$high === 0 && r.$low === 0))) {
			e = new syscall.Errno(21);
		}
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			_tmp$2 = new $Int64(0, 0);
			_tmp$3 = new PathError.ptr("seek", f.file.name, e);
			ret = _tmp$2;
			err = _tmp$3;
			return [ret, err];
		}
		_tmp$4 = r;
		_tmp$5 = $ifaceNil;
		ret = _tmp$4;
		err = _tmp$5;
		return [ret, err];
	};
	File.prototype.Seek = function(offset, whence) { return this.$val.Seek(offset, whence); };
	File.ptr.prototype.WriteString = function(s) {
		var $ptr, _tmp, _tmp$1, _tuple, err, f, n, s;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (f === ptrType$1.nil) {
			_tmp = 0;
			_tmp$1 = $pkg.ErrInvalid;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
		_tuple = f.Write(new sliceType$1($stringToBytes(s)));
		n = _tuple[0];
		err = _tuple[1];
		return [n, err];
	};
	File.prototype.WriteString = function(s) { return this.$val.WriteString(s); };
	File.ptr.prototype.Chdir = function() {
		var $ptr, e, f;
		f = this;
		if (f === ptrType$1.nil) {
			return $pkg.ErrInvalid;
		}
		e = syscall.Fchdir(f.file.fd);
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			return new PathError.ptr("chdir", f.file.name, e);
		}
		return $ifaceNil;
	};
	File.prototype.Chdir = function() { return this.$val.Chdir(); };
	fixCount = function(n, err) {
		var $ptr, err, n;
		if (n < 0) {
			n = 0;
		}
		return [n, err];
	};
	sigpipe = function() {
		$throwRuntimeError("native function not implemented: os.sigpipe");
	};
	syscallMode = function(i) {
		var $ptr, i, o;
		o = 0;
		o = (o | ((new FileMode(i).Perm() >>> 0))) >>> 0;
		if (!((((i & 8388608) >>> 0) === 0))) {
			o = (o | (2048)) >>> 0;
		}
		if (!((((i & 4194304) >>> 0) === 0))) {
			o = (o | (1024)) >>> 0;
		}
		if (!((((i & 1048576) >>> 0) === 0))) {
			o = (o | (512)) >>> 0;
		}
		return o;
	};
	File.ptr.prototype.Chmod = function(mode) {
		var $ptr, e, f, mode;
		f = this;
		if (f === ptrType$1.nil) {
			return $pkg.ErrInvalid;
		}
		e = syscall.Fchmod(f.file.fd, syscallMode(mode));
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			return new PathError.ptr("chmod", f.file.name, e);
		}
		return $ifaceNil;
	};
	File.prototype.Chmod = function(mode) { return this.$val.Chmod(mode); };
	File.ptr.prototype.Chown = function(uid, gid) {
		var $ptr, e, f, gid, uid;
		f = this;
		if (f === ptrType$1.nil) {
			return $pkg.ErrInvalid;
		}
		e = syscall.Fchown(f.file.fd, uid, gid);
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			return new PathError.ptr("chown", f.file.name, e);
		}
		return $ifaceNil;
	};
	File.prototype.Chown = function(uid, gid) { return this.$val.Chown(uid, gid); };
	File.ptr.prototype.Truncate = function(size) {
		var $ptr, e, f, size;
		f = this;
		if (f === ptrType$1.nil) {
			return $pkg.ErrInvalid;
		}
		e = syscall.Ftruncate(f.file.fd, size);
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			return new PathError.ptr("truncate", f.file.name, e);
		}
		return $ifaceNil;
	};
	File.prototype.Truncate = function(size) { return this.$val.Truncate(size); };
	File.ptr.prototype.Sync = function() {
		var $ptr, e, f;
		f = this;
		if (f === ptrType$1.nil) {
			return $pkg.ErrInvalid;
		}
		e = syscall.Fsync(f.file.fd);
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			return NewSyscallError("fsync", e);
		}
		return $ifaceNil;
	};
	File.prototype.Sync = function() { return this.$val.Sync(); };
	File.ptr.prototype.Fd = function() {
		var $ptr, f;
		f = this;
		if (f === ptrType$1.nil) {
			return 4294967295;
		}
		return (f.file.fd >>> 0);
	};
	File.prototype.Fd = function() { return this.$val.Fd(); };
	NewFile = function(fd, name) {
		var $ptr, f, fd, fdi, name;
		fdi = (fd >> 0);
		if (fdi < 0) {
			return ptrType$1.nil;
		}
		f = new File.ptr(new file.ptr(fdi, name, ptrType.nil));
		runtime.SetFinalizer(f.file, new funcType$1($methodExpr(ptrType$12, "close")));
		return f;
	};
	$pkg.NewFile = NewFile;
	epipecheck = function(file$1, e) {
		var $ptr, e, file$1;
		if ($interfaceIsEqual(e, new syscall.Errno(32)) && ((file$1.file.fd === 1) || (file$1.file.fd === 2))) {
			sigpipe();
		}
	};
	File.ptr.prototype.Close = function() {
		var $ptr, f;
		f = this;
		if (f === ptrType$1.nil) {
			return $pkg.ErrInvalid;
		}
		return f.file.close();
	};
	File.prototype.Close = function() { return this.$val.Close(); };
	file.ptr.prototype.close = function() {
		var $ptr, e, err, file$1;
		file$1 = this;
		if (file$1 === ptrType$12.nil || file$1.fd < 0) {
			return new syscall.Errno(22);
		}
		err = $ifaceNil;
		e = syscall.Close(file$1.fd);
		if (!($interfaceIsEqual(e, $ifaceNil))) {
			err = new PathError.ptr("close", file$1.name, e);
		}
		file$1.fd = -1;
		runtime.SetFinalizer(file$1, $ifaceNil);
		return err;
	};
	file.prototype.close = function() { return this.$val.close(); };
	File.ptr.prototype.Stat = function() {
		var $ptr, err, f, fs;
		f = this;
		if (f === ptrType$1.nil) {
			return [$ifaceNil, $pkg.ErrInvalid];
		}
		fs = new fileStat.ptr("", new $Int64(0, 0), 0, new time.Time.ptr(new $Int64(0, 0), 0, ptrType$13.nil), new syscall.Stat_t.ptr(0, 0, 0, new $Uint64(0, 0), 0, 0, 0, arrayType.zero(), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new $Int64(0, 0), new $Int64(0, 0), 0, 0, 0, 0, arrayType$2.zero()));
		err = syscall.Fstat(f.file.fd, fs.sys);
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return [$ifaceNil, new PathError.ptr("stat", f.file.name, err)];
		}
		fillFileStatFromSys(fs, f.file.name);
		return [fs, $ifaceNil];
	};
	File.prototype.Stat = function() { return this.$val.Stat(); };
	Lstat = function(name) {
		var $ptr, err, fs, name;
		fs = new fileStat.ptr("", new $Int64(0, 0), 0, new time.Time.ptr(new $Int64(0, 0), 0, ptrType$13.nil), new syscall.Stat_t.ptr(0, 0, 0, new $Uint64(0, 0), 0, 0, 0, arrayType.zero(), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new syscall.Timespec.ptr(new $Int64(0, 0), new $Int64(0, 0)), new $Int64(0, 0), new $Int64(0, 0), 0, 0, 0, 0, arrayType$2.zero()));
		err = syscall.Lstat(name, fs.sys);
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return [$ifaceNil, new PathError.ptr("lstat", name, err)];
		}
		fillFileStatFromSys(fs, name);
		return [fs, $ifaceNil];
	};
	$pkg.Lstat = Lstat;
	File.ptr.prototype.readdir = function(n) {
		var $ptr, _i, _r, _ref, _tmp, _tmp$1, _tmp$2, _tmp$3, _tuple, _tuple$1, dirname, err, f, fi, filename, fip, lerr, n, names, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _i = $f._i; _r = $f._r; _ref = $f._ref; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tuple = $f._tuple; _tuple$1 = $f._tuple$1; dirname = $f.dirname; err = $f.err; f = $f.f; fi = $f.fi; filename = $f.filename; fip = $f.fip; lerr = $f.lerr; n = $f.n; names = $f.names; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		fi = sliceType$2.nil;
		err = $ifaceNil;
		f = this;
		dirname = f.file.name;
		if (dirname === "") {
			dirname = ".";
		}
		_tuple = f.Readdirnames(n);
		names = _tuple[0];
		err = _tuple[1];
		fi = $makeSlice(sliceType$2, 0, names.$length);
		_ref = names;
		_i = 0;
		/* while (true) { */ case 1:
			/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 2; continue; }
			filename = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			_r = lstat(dirname + "/" + filename); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			_tuple$1 = _r;
			fip = _tuple$1[0];
			lerr = _tuple$1[1];
			if (IsNotExist(lerr)) {
				_i++;
				/* continue; */ $s = 1; continue;
			}
			if (!($interfaceIsEqual(lerr, $ifaceNil))) {
				_tmp = fi;
				_tmp$1 = lerr;
				fi = _tmp;
				err = _tmp$1;
				return [fi, err];
			}
			fi = $append(fi, fip);
			_i++;
		/* } */ $s = 1; continue; case 2:
		_tmp$2 = fi;
		_tmp$3 = err;
		fi = _tmp$2;
		err = _tmp$3;
		return [fi, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: File.ptr.prototype.readdir }; } $f.$ptr = $ptr; $f._i = _i; $f._r = _r; $f._ref = _ref; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tuple = _tuple; $f._tuple$1 = _tuple$1; $f.dirname = dirname; $f.err = err; $f.f = f; $f.fi = fi; $f.filename = filename; $f.fip = fip; $f.lerr = lerr; $f.n = n; $f.names = names; $f.$s = $s; $f.$r = $r; return $f;
	};
	File.prototype.readdir = function(n) { return this.$val.readdir(n); };
	File.ptr.prototype.read = function(b) {
		var $ptr, _tuple, _tuple$1, b, err, f, n;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (true && b.$length > 1073741824) {
			b = $subslice(b, 0, 1073741824);
		}
		_tuple$1 = syscall.Read(f.file.fd, b);
		_tuple = fixCount(_tuple$1[0], _tuple$1[1]);
		n = _tuple[0];
		err = _tuple[1];
		return [n, err];
	};
	File.prototype.read = function(b) { return this.$val.read(b); };
	File.ptr.prototype.pread = function(b, off) {
		var $ptr, _tuple, _tuple$1, b, err, f, n, off;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (true && b.$length > 1073741824) {
			b = $subslice(b, 0, 1073741824);
		}
		_tuple$1 = syscall.Pread(f.file.fd, b, off);
		_tuple = fixCount(_tuple$1[0], _tuple$1[1]);
		n = _tuple[0];
		err = _tuple[1];
		return [n, err];
	};
	File.prototype.pread = function(b, off) { return this.$val.pread(b, off); };
	File.ptr.prototype.write = function(b) {
		var $ptr, _tmp, _tmp$1, _tuple, _tuple$1, b, bcap, err, err$1, f, m, n;
		n = 0;
		err = $ifaceNil;
		f = this;
		while (true) {
			bcap = b;
			if (true && bcap.$length > 1073741824) {
				bcap = $subslice(bcap, 0, 1073741824);
			}
			_tuple$1 = syscall.Write(f.file.fd, bcap);
			_tuple = fixCount(_tuple$1[0], _tuple$1[1]);
			m = _tuple[0];
			err$1 = _tuple[1];
			n = n + (m) >> 0;
			if (0 < m && m < bcap.$length || $interfaceIsEqual(err$1, new syscall.Errno(4))) {
				b = $subslice(b, m);
				continue;
			}
			if (true && !((bcap.$length === b.$length)) && $interfaceIsEqual(err$1, $ifaceNil)) {
				b = $subslice(b, m);
				continue;
			}
			_tmp = n;
			_tmp$1 = err$1;
			n = _tmp;
			err = _tmp$1;
			return [n, err];
		}
	};
	File.prototype.write = function(b) { return this.$val.write(b); };
	File.ptr.prototype.pwrite = function(b, off) {
		var $ptr, _tuple, _tuple$1, b, err, f, n, off;
		n = 0;
		err = $ifaceNil;
		f = this;
		if (true && b.$length > 1073741824) {
			b = $subslice(b, 0, 1073741824);
		}
		_tuple$1 = syscall.Pwrite(f.file.fd, b, off);
		_tuple = fixCount(_tuple$1[0], _tuple$1[1]);
		n = _tuple[0];
		err = _tuple[1];
		return [n, err];
	};
	File.prototype.pwrite = function(b, off) { return this.$val.pwrite(b, off); };
	File.ptr.prototype.seek = function(offset, whence) {
		var $ptr, _tuple, err, f, offset, ret, whence;
		ret = new $Int64(0, 0);
		err = $ifaceNil;
		f = this;
		_tuple = syscall.Seek(f.file.fd, offset, whence);
		ret = _tuple[0];
		err = _tuple[1];
		return [ret, err];
	};
	File.prototype.seek = function(offset, whence) { return this.$val.seek(offset, whence); };
	basename = function(name) {
		var $ptr, i, name;
		i = name.length - 1 >> 0;
		while (true) {
			if (!(i > 0 && (name.charCodeAt(i) === 47))) { break; }
			name = name.substring(0, i);
			i = i - (1) >> 0;
		}
		i = i - (1) >> 0;
		while (true) {
			if (!(i >= 0)) { break; }
			if (name.charCodeAt(i) === 47) {
				name = name.substring((i + 1 >> 0));
				break;
			}
			i = i - (1) >> 0;
		}
		return name;
	};
	init$1 = function() {
		var $ptr;
		useSyscallwd = useSyscallwdDarwin;
	};
	useSyscallwdDarwin = function(err) {
		var $ptr, err;
		return !($interfaceIsEqual(err, new syscall.Errno(45)));
	};
	init$2 = function() {
		var $ptr;
		if (false) {
			return;
		}
		$pkg.Args = runtime_args();
	};
	fillFileStatFromSys = function(fs, name) {
		var $ptr, _1, fs, name;
		fs.name = basename(name);
		fs.size = fs.sys.Size;
		time.Time.copy(fs.modTime, timespecToTime(fs.sys.Mtimespec));
		fs.mode = (((fs.sys.Mode & 511) >>> 0) >>> 0);
		_1 = (fs.sys.Mode & 61440) >>> 0;
		if ((_1 === (24576)) || (_1 === (57344))) {
			fs.mode = (fs.mode | (67108864)) >>> 0;
		} else if (_1 === (8192)) {
			fs.mode = (fs.mode | (69206016)) >>> 0;
		} else if (_1 === (16384)) {
			fs.mode = (fs.mode | (2147483648)) >>> 0;
		} else if (_1 === (4096)) {
			fs.mode = (fs.mode | (33554432)) >>> 0;
		} else if (_1 === (40960)) {
			fs.mode = (fs.mode | (134217728)) >>> 0;
		} else if (_1 === (32768)) {
		} else if (_1 === (49152)) {
			fs.mode = (fs.mode | (16777216)) >>> 0;
		}
		if (!((((fs.sys.Mode & 1024) >>> 0) === 0))) {
			fs.mode = (fs.mode | (4194304)) >>> 0;
		}
		if (!((((fs.sys.Mode & 2048) >>> 0) === 0))) {
			fs.mode = (fs.mode | (8388608)) >>> 0;
		}
		if (!((((fs.sys.Mode & 512) >>> 0) === 0))) {
			fs.mode = (fs.mode | (1048576)) >>> 0;
		}
	};
	timespecToTime = function(ts) {
		var $ptr, ts;
		ts = $clone(ts, syscall.Timespec);
		return time.Unix(ts.Sec, ts.Nsec);
	};
	init$3 = function() {
		var $ptr, _i, _ref, _rune, _tuple, err, i, osver;
		_tuple = syscall.Sysctl("kern.osrelease");
		osver = _tuple[0];
		err = _tuple[1];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return;
		}
		i = 0;
		_ref = osver;
		_i = 0;
		while (true) {
			if (!(_i < _ref.length)) { break; }
			_rune = $decodeRune(_ref, _i);
			i = _i;
			if (!((osver.charCodeAt(i) === 46))) {
				_i += _rune[1];
				continue;
			}
			_i += _rune[1];
		}
		if (i > 2 || (i === 2) && osver.charCodeAt(0) >= 49 && osver.charCodeAt(1) >= 49) {
			supportsCloseOnExec = true;
		}
	};
	FileMode.prototype.String = function() {
		var $ptr, _i, _i$1, _ref, _ref$1, _rune, _rune$1, buf, c, c$1, i, i$1, m, w, y, y$1;
		m = this.$val;
		buf = arrayType$5.zero();
		w = 0;
		_ref = "dalTLDpSugct";
		_i = 0;
		while (true) {
			if (!(_i < _ref.length)) { break; }
			_rune = $decodeRune(_ref, _i);
			i = _i;
			c = _rune[0];
			if (!((((m & (((y = ((31 - i >> 0) >>> 0), y < 32 ? (1 << y) : 0) >>> 0))) >>> 0) === 0))) {
				((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = (c << 24 >>> 24));
				w = w + (1) >> 0;
			}
			_i += _rune[1];
		}
		if (w === 0) {
			((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 45);
			w = w + (1) >> 0;
		}
		_ref$1 = "rwxrwxrwx";
		_i$1 = 0;
		while (true) {
			if (!(_i$1 < _ref$1.length)) { break; }
			_rune$1 = $decodeRune(_ref$1, _i$1);
			i$1 = _i$1;
			c$1 = _rune$1[0];
			if (!((((m & (((y$1 = ((8 - i$1 >> 0) >>> 0), y$1 < 32 ? (1 << y$1) : 0) >>> 0))) >>> 0) === 0))) {
				((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = (c$1 << 24 >>> 24));
			} else {
				((w < 0 || w >= buf.length) ? $throwRuntimeError("index out of range") : buf[w] = 45);
			}
			w = w + (1) >> 0;
			_i$1 += _rune$1[1];
		}
		return $bytesToString($subslice(new sliceType$1(buf), 0, w));
	};
	$ptrType(FileMode).prototype.String = function() { return new FileMode(this.$get()).String(); };
	FileMode.prototype.IsDir = function() {
		var $ptr, m;
		m = this.$val;
		return !((((m & 2147483648) >>> 0) === 0));
	};
	$ptrType(FileMode).prototype.IsDir = function() { return new FileMode(this.$get()).IsDir(); };
	FileMode.prototype.IsRegular = function() {
		var $ptr, m;
		m = this.$val;
		return ((m & 2399141888) >>> 0) === 0;
	};
	$ptrType(FileMode).prototype.IsRegular = function() { return new FileMode(this.$get()).IsRegular(); };
	FileMode.prototype.Perm = function() {
		var $ptr, m;
		m = this.$val;
		return (m & 511) >>> 0;
	};
	$ptrType(FileMode).prototype.Perm = function() { return new FileMode(this.$get()).Perm(); };
	fileStat.ptr.prototype.Name = function() {
		var $ptr, fs;
		fs = this;
		return fs.name;
	};
	fileStat.prototype.Name = function() { return this.$val.Name(); };
	fileStat.ptr.prototype.IsDir = function() {
		var $ptr, fs;
		fs = this;
		return new FileMode(fs.Mode()).IsDir();
	};
	fileStat.prototype.IsDir = function() { return this.$val.IsDir(); };
	fileStat.ptr.prototype.Size = function() {
		var $ptr, fs;
		fs = this;
		return fs.size;
	};
	fileStat.prototype.Size = function() { return this.$val.Size(); };
	fileStat.ptr.prototype.Mode = function() {
		var $ptr, fs;
		fs = this;
		return fs.mode;
	};
	fileStat.prototype.Mode = function() { return this.$val.Mode(); };
	fileStat.ptr.prototype.ModTime = function() {
		var $ptr, fs;
		fs = this;
		return fs.modTime;
	};
	fileStat.prototype.ModTime = function() { return this.$val.ModTime(); };
	fileStat.ptr.prototype.Sys = function() {
		var $ptr, fs;
		fs = this;
		return fs.sys;
	};
	fileStat.prototype.Sys = function() { return this.$val.Sys(); };
	ptrType$2.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$4.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$3.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$1.methods = [{prop: "readdirnames", name: "readdirnames", pkg: "os", typ: $funcType([$Int], [sliceType, $error], false)}, {prop: "Readdir", name: "Readdir", pkg: "", typ: $funcType([$Int], [sliceType$2, $error], false)}, {prop: "Readdirnames", name: "Readdirnames", pkg: "", typ: $funcType([$Int], [sliceType, $error], false)}, {prop: "Name", name: "Name", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Read", name: "Read", pkg: "", typ: $funcType([sliceType$1], [$Int, $error], false)}, {prop: "ReadAt", name: "ReadAt", pkg: "", typ: $funcType([sliceType$1, $Int64], [$Int, $error], false)}, {prop: "Write", name: "Write", pkg: "", typ: $funcType([sliceType$1], [$Int, $error], false)}, {prop: "WriteAt", name: "WriteAt", pkg: "", typ: $funcType([sliceType$1, $Int64], [$Int, $error], false)}, {prop: "Seek", name: "Seek", pkg: "", typ: $funcType([$Int64, $Int], [$Int64, $error], false)}, {prop: "WriteString", name: "WriteString", pkg: "", typ: $funcType([$String], [$Int, $error], false)}, {prop: "Chdir", name: "Chdir", pkg: "", typ: $funcType([], [$error], false)}, {prop: "Chmod", name: "Chmod", pkg: "", typ: $funcType([FileMode], [$error], false)}, {prop: "Chown", name: "Chown", pkg: "", typ: $funcType([$Int, $Int], [$error], false)}, {prop: "Truncate", name: "Truncate", pkg: "", typ: $funcType([$Int64], [$error], false)}, {prop: "Sync", name: "Sync", pkg: "", typ: $funcType([], [$error], false)}, {prop: "Fd", name: "Fd", pkg: "", typ: $funcType([], [$Uintptr], false)}, {prop: "Close", name: "Close", pkg: "", typ: $funcType([], [$error], false)}, {prop: "Stat", name: "Stat", pkg: "", typ: $funcType([], [FileInfo, $error], false)}, {prop: "readdir", name: "readdir", pkg: "os", typ: $funcType([$Int], [sliceType$2, $error], false)}, {prop: "read", name: "read", pkg: "os", typ: $funcType([sliceType$1], [$Int, $error], false)}, {prop: "pread", name: "pread", pkg: "os", typ: $funcType([sliceType$1, $Int64], [$Int, $error], false)}, {prop: "write", name: "write", pkg: "os", typ: $funcType([sliceType$1], [$Int, $error], false)}, {prop: "pwrite", name: "pwrite", pkg: "os", typ: $funcType([sliceType$1, $Int64], [$Int, $error], false)}, {prop: "seek", name: "seek", pkg: "os", typ: $funcType([$Int64, $Int], [$Int64, $error], false)}];
	ptrType$12.methods = [{prop: "close", name: "close", pkg: "os", typ: $funcType([], [$error], false)}];
	FileMode.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "IsDir", name: "IsDir", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "IsRegular", name: "IsRegular", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Perm", name: "Perm", pkg: "", typ: $funcType([], [FileMode], false)}];
	ptrType$15.methods = [{prop: "Name", name: "Name", pkg: "", typ: $funcType([], [$String], false)}, {prop: "IsDir", name: "IsDir", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Size", name: "Size", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "Mode", name: "Mode", pkg: "", typ: $funcType([], [FileMode], false)}, {prop: "ModTime", name: "ModTime", pkg: "", typ: $funcType([], [time.Time], false)}, {prop: "Sys", name: "Sys", pkg: "", typ: $funcType([], [$emptyInterface], false)}];
	PathError.init([{prop: "Op", name: "Op", pkg: "", typ: $String, tag: ""}, {prop: "Path", name: "Path", pkg: "", typ: $String, tag: ""}, {prop: "Err", name: "Err", pkg: "", typ: $error, tag: ""}]);
	SyscallError.init([{prop: "Syscall", name: "Syscall", pkg: "", typ: $String, tag: ""}, {prop: "Err", name: "Err", pkg: "", typ: $error, tag: ""}]);
	LinkError.init([{prop: "Op", name: "Op", pkg: "", typ: $String, tag: ""}, {prop: "Old", name: "Old", pkg: "", typ: $String, tag: ""}, {prop: "New", name: "New", pkg: "", typ: $String, tag: ""}, {prop: "Err", name: "Err", pkg: "", typ: $error, tag: ""}]);
	File.init([{prop: "file", name: "", pkg: "os", typ: ptrType$12, tag: ""}]);
	file.init([{prop: "fd", name: "fd", pkg: "os", typ: $Int, tag: ""}, {prop: "name", name: "name", pkg: "os", typ: $String, tag: ""}, {prop: "dirinfo", name: "dirinfo", pkg: "os", typ: ptrType, tag: ""}]);
	dirInfo.init([{prop: "buf", name: "buf", pkg: "os", typ: sliceType$1, tag: ""}, {prop: "nbuf", name: "nbuf", pkg: "os", typ: $Int, tag: ""}, {prop: "bufp", name: "bufp", pkg: "os", typ: $Int, tag: ""}]);
	FileInfo.init([{prop: "IsDir", name: "IsDir", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "ModTime", name: "ModTime", pkg: "", typ: $funcType([], [time.Time], false)}, {prop: "Mode", name: "Mode", pkg: "", typ: $funcType([], [FileMode], false)}, {prop: "Name", name: "Name", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Size", name: "Size", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "Sys", name: "Sys", pkg: "", typ: $funcType([], [$emptyInterface], false)}]);
	fileStat.init([{prop: "name", name: "name", pkg: "os", typ: $String, tag: ""}, {prop: "size", name: "size", pkg: "os", typ: $Int64, tag: ""}, {prop: "mode", name: "mode", pkg: "os", typ: FileMode, tag: ""}, {prop: "modTime", name: "modTime", pkg: "os", typ: time.Time, tag: ""}, {prop: "sys", name: "sys", pkg: "os", typ: syscall.Stat_t, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = js.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = io.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = runtime.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sync.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = atomic.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = syscall.$init(); /* */ $s = 7; case 7: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = time.$init(); /* */ $s = 8; case 8: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.Args = sliceType.nil;
		supportsCloseOnExec = false;
		$pkg.ErrInvalid = errors.New("invalid argument");
		$pkg.ErrPermission = errors.New("permission denied");
		$pkg.ErrExist = errors.New("file already exists");
		$pkg.ErrNotExist = errors.New("file does not exist");
		errFinished = errors.New("os: process already finished");
		$pkg.Stdin = NewFile((syscall.Stdin >>> 0), "/dev/stdin");
		$pkg.Stdout = NewFile((syscall.Stdout >>> 0), "/dev/stdout");
		$pkg.Stderr = NewFile((syscall.Stderr >>> 0), "/dev/stderr");
		useSyscallwd = (function(param) {
			var $ptr, param;
			return true;
		});
		lstat = Lstat;
		init();
		init$1();
		init$2();
		init$3();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["strconv"] = (function() {
	var $pkg = {}, $init, errors, math, utf8, decimal, leftCheat, extFloat, floatInfo, decimalSlice, sliceType$3, sliceType$4, sliceType$5, arrayType, sliceType$6, arrayType$1, arrayType$2, ptrType$1, arrayType$3, arrayType$4, ptrType$2, ptrType$3, ptrType$4, optimize, leftcheats, smallPowersOfTen, powersOfTen, uint64pow10, float32info, float32info$24ptr, float64info, float64info$24ptr, isPrint16, isNotPrint16, isPrint32, isNotPrint32, isGraphic, shifts, digitZero, trim, rightShift, prefixIsLessThan, leftShift, shouldRoundUp, frexp10Many, adjustLastDigitFixed, adjustLastDigit, AppendFloat, genericFtoa, bigFtoa, formatDigits, roundShortest, fmtE, fmtF, fmtB, min, max, FormatInt, Itoa, formatBits, quoteWith, Quote, QuoteToASCII, QuoteRune, AppendQuoteRune, QuoteRuneToASCII, AppendQuoteRuneToASCII, CanBackquote, unhex, UnquoteChar, Unquote, contains, bsearch16, bsearch32, IsPrint, isInGraphicList;
	errors = $packages["errors"];
	math = $packages["math"];
	utf8 = $packages["unicode/utf8"];
	decimal = $pkg.decimal = $newType(0, $kindStruct, "strconv.decimal", "decimal", "strconv", function(d_, nd_, dp_, neg_, trunc_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.d = arrayType.zero();
			this.nd = 0;
			this.dp = 0;
			this.neg = false;
			this.trunc = false;
			return;
		}
		this.d = d_;
		this.nd = nd_;
		this.dp = dp_;
		this.neg = neg_;
		this.trunc = trunc_;
	});
	leftCheat = $pkg.leftCheat = $newType(0, $kindStruct, "strconv.leftCheat", "leftCheat", "strconv", function(delta_, cutoff_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.delta = 0;
			this.cutoff = "";
			return;
		}
		this.delta = delta_;
		this.cutoff = cutoff_;
	});
	extFloat = $pkg.extFloat = $newType(0, $kindStruct, "strconv.extFloat", "extFloat", "strconv", function(mant_, exp_, neg_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.mant = new $Uint64(0, 0);
			this.exp = 0;
			this.neg = false;
			return;
		}
		this.mant = mant_;
		this.exp = exp_;
		this.neg = neg_;
	});
	floatInfo = $pkg.floatInfo = $newType(0, $kindStruct, "strconv.floatInfo", "floatInfo", "strconv", function(mantbits_, expbits_, bias_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.mantbits = 0;
			this.expbits = 0;
			this.bias = 0;
			return;
		}
		this.mantbits = mantbits_;
		this.expbits = expbits_;
		this.bias = bias_;
	});
	decimalSlice = $pkg.decimalSlice = $newType(0, $kindStruct, "strconv.decimalSlice", "decimalSlice", "strconv", function(d_, nd_, dp_, neg_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.d = sliceType$6.nil;
			this.nd = 0;
			this.dp = 0;
			this.neg = false;
			return;
		}
		this.d = d_;
		this.nd = nd_;
		this.dp = dp_;
		this.neg = neg_;
	});
	sliceType$3 = $sliceType(leftCheat);
	sliceType$4 = $sliceType($Uint16);
	sliceType$5 = $sliceType($Uint32);
	arrayType = $arrayType($Uint8, 800);
	sliceType$6 = $sliceType($Uint8);
	arrayType$1 = $arrayType($Uint8, 24);
	arrayType$2 = $arrayType($Uint8, 32);
	ptrType$1 = $ptrType(floatInfo);
	arrayType$3 = $arrayType($Uint8, 65);
	arrayType$4 = $arrayType($Uint8, 4);
	ptrType$2 = $ptrType(decimal);
	ptrType$3 = $ptrType(decimalSlice);
	ptrType$4 = $ptrType(extFloat);
	decimal.ptr.prototype.String = function() {
		var $ptr, a, buf, n, w;
		a = this;
		n = 10 + a.nd >> 0;
		if (a.dp > 0) {
			n = n + (a.dp) >> 0;
		}
		if (a.dp < 0) {
			n = n + (-a.dp) >> 0;
		}
		buf = $makeSlice(sliceType$6, n);
		w = 0;
		if ((a.nd === 0)) {
			return "0";
		} else if (a.dp <= 0) {
			((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = 48);
			w = w + (1) >> 0;
			((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = 46);
			w = w + (1) >> 0;
			w = w + (digitZero($subslice(buf, w, (w + -a.dp >> 0)))) >> 0;
			w = w + ($copySlice($subslice(buf, w), $subslice(new sliceType$6(a.d), 0, a.nd))) >> 0;
		} else if (a.dp < a.nd) {
			w = w + ($copySlice($subslice(buf, w), $subslice(new sliceType$6(a.d), 0, a.dp))) >> 0;
			((w < 0 || w >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + w] = 46);
			w = w + (1) >> 0;
			w = w + ($copySlice($subslice(buf, w), $subslice(new sliceType$6(a.d), a.dp, a.nd))) >> 0;
		} else {
			w = w + ($copySlice($subslice(buf, w), $subslice(new sliceType$6(a.d), 0, a.nd))) >> 0;
			w = w + (digitZero($subslice(buf, w, ((w + a.dp >> 0) - a.nd >> 0)))) >> 0;
		}
		return $bytesToString($subslice(buf, 0, w));
	};
	decimal.prototype.String = function() { return this.$val.String(); };
	digitZero = function(dst) {
		var $ptr, _i, _ref, dst, i;
		_ref = dst;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			((i < 0 || i >= dst.$length) ? $throwRuntimeError("index out of range") : dst.$array[dst.$offset + i] = 48);
			_i++;
		}
		return dst.$length;
	};
	trim = function(a) {
		var $ptr, a, x, x$1;
		while (true) {
			if (!(a.nd > 0 && ((x = a.d, x$1 = a.nd - 1 >> 0, ((x$1 < 0 || x$1 >= x.length) ? $throwRuntimeError("index out of range") : x[x$1])) === 48))) { break; }
			a.nd = a.nd - (1) >> 0;
		}
		if (a.nd === 0) {
			a.dp = 0;
		}
	};
	decimal.ptr.prototype.Assign = function(v) {
		var $ptr, a, buf, n, v, v1, x, x$1, x$2;
		a = this;
		buf = arrayType$1.zero();
		n = 0;
		while (true) {
			if (!((v.$high > 0 || (v.$high === 0 && v.$low > 0)))) { break; }
			v1 = $div64(v, new $Uint64(0, 10), false);
			v = (x = $mul64(new $Uint64(0, 10), v1), new $Uint64(v.$high - x.$high, v.$low - x.$low));
			((n < 0 || n >= buf.length) ? $throwRuntimeError("index out of range") : buf[n] = (new $Uint64(v.$high + 0, v.$low + 48).$low << 24 >>> 24));
			n = n + (1) >> 0;
			v = v1;
		}
		a.nd = 0;
		n = n - (1) >> 0;
		while (true) {
			if (!(n >= 0)) { break; }
			(x$1 = a.d, x$2 = a.nd, ((x$2 < 0 || x$2 >= x$1.length) ? $throwRuntimeError("index out of range") : x$1[x$2] = ((n < 0 || n >= buf.length) ? $throwRuntimeError("index out of range") : buf[n])));
			a.nd = a.nd + (1) >> 0;
			n = n - (1) >> 0;
		}
		a.dp = a.nd;
		trim(a);
	};
	decimal.prototype.Assign = function(v) { return this.$val.Assign(v); };
	rightShift = function(a, k) {
		var $ptr, a, c, c$1, dig, dig$1, k, n, r, w, x, x$1, x$2, x$3, y, y$1, y$2, y$3, y$4, y$5;
		r = 0;
		w = 0;
		n = 0;
		while (true) {
			if (!(((y = k, y < 32 ? (n >>> y) : 0) >>> 0) === 0)) { break; }
			if (r >= a.nd) {
				if (n === 0) {
					a.nd = 0;
					return;
				}
				while (true) {
					if (!(((y$1 = k, y$1 < 32 ? (n >>> y$1) : 0) >>> 0) === 0)) { break; }
					n = n * 10 >>> 0;
					r = r + (1) >> 0;
				}
				break;
			}
			c = ((x = a.d, ((r < 0 || r >= x.length) ? $throwRuntimeError("index out of range") : x[r])) >>> 0);
			n = ((n * 10 >>> 0) + c >>> 0) - 48 >>> 0;
			r = r + (1) >> 0;
		}
		a.dp = a.dp - ((r - 1 >> 0)) >> 0;
		while (true) {
			if (!(r < a.nd)) { break; }
			c$1 = ((x$1 = a.d, ((r < 0 || r >= x$1.length) ? $throwRuntimeError("index out of range") : x$1[r])) >>> 0);
			dig = (y$2 = k, y$2 < 32 ? (n >>> y$2) : 0) >>> 0;
			n = n - (((y$3 = k, y$3 < 32 ? (dig << y$3) : 0) >>> 0)) >>> 0;
			(x$2 = a.d, ((w < 0 || w >= x$2.length) ? $throwRuntimeError("index out of range") : x$2[w] = ((dig + 48 >>> 0) << 24 >>> 24)));
			w = w + (1) >> 0;
			n = ((n * 10 >>> 0) + c$1 >>> 0) - 48 >>> 0;
			r = r + (1) >> 0;
		}
		while (true) {
			if (!(n > 0)) { break; }
			dig$1 = (y$4 = k, y$4 < 32 ? (n >>> y$4) : 0) >>> 0;
			n = n - (((y$5 = k, y$5 < 32 ? (dig$1 << y$5) : 0) >>> 0)) >>> 0;
			if (w < 800) {
				(x$3 = a.d, ((w < 0 || w >= x$3.length) ? $throwRuntimeError("index out of range") : x$3[w] = ((dig$1 + 48 >>> 0) << 24 >>> 24)));
				w = w + (1) >> 0;
			} else if (dig$1 > 0) {
				a.trunc = true;
			}
			n = n * 10 >>> 0;
		}
		a.nd = w;
		trim(a);
	};
	prefixIsLessThan = function(b, s) {
		var $ptr, b, i, s;
		i = 0;
		while (true) {
			if (!(i < s.length)) { break; }
			if (i >= b.$length) {
				return true;
			}
			if (!((((i < 0 || i >= b.$length) ? $throwRuntimeError("index out of range") : b.$array[b.$offset + i]) === s.charCodeAt(i)))) {
				return ((i < 0 || i >= b.$length) ? $throwRuntimeError("index out of range") : b.$array[b.$offset + i]) < s.charCodeAt(i);
			}
			i = i + (1) >> 0;
		}
		return false;
	};
	leftShift = function(a, k) {
		var $ptr, _q, _q$1, a, delta, k, n, quo, quo$1, r, rem, rem$1, w, x, x$1, x$2, y;
		delta = ((k < 0 || k >= leftcheats.$length) ? $throwRuntimeError("index out of range") : leftcheats.$array[leftcheats.$offset + k]).delta;
		if (prefixIsLessThan($subslice(new sliceType$6(a.d), 0, a.nd), ((k < 0 || k >= leftcheats.$length) ? $throwRuntimeError("index out of range") : leftcheats.$array[leftcheats.$offset + k]).cutoff)) {
			delta = delta - (1) >> 0;
		}
		r = a.nd;
		w = a.nd + delta >> 0;
		n = 0;
		r = r - (1) >> 0;
		while (true) {
			if (!(r >= 0)) { break; }
			n = n + (((y = k, y < 32 ? (((((x = a.d, ((r < 0 || r >= x.length) ? $throwRuntimeError("index out of range") : x[r])) >>> 0) - 48 >>> 0)) << y) : 0) >>> 0)) >>> 0;
			quo = (_q = n / 10, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
			rem = n - (10 * quo >>> 0) >>> 0;
			w = w - (1) >> 0;
			if (w < 800) {
				(x$1 = a.d, ((w < 0 || w >= x$1.length) ? $throwRuntimeError("index out of range") : x$1[w] = ((rem + 48 >>> 0) << 24 >>> 24)));
			} else if (!((rem === 0))) {
				a.trunc = true;
			}
			n = quo;
			r = r - (1) >> 0;
		}
		while (true) {
			if (!(n > 0)) { break; }
			quo$1 = (_q$1 = n / 10, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >>> 0 : $throwRuntimeError("integer divide by zero"));
			rem$1 = n - (10 * quo$1 >>> 0) >>> 0;
			w = w - (1) >> 0;
			if (w < 800) {
				(x$2 = a.d, ((w < 0 || w >= x$2.length) ? $throwRuntimeError("index out of range") : x$2[w] = ((rem$1 + 48 >>> 0) << 24 >>> 24)));
			} else if (!((rem$1 === 0))) {
				a.trunc = true;
			}
			n = quo$1;
		}
		a.nd = a.nd + (delta) >> 0;
		if (a.nd >= 800) {
			a.nd = 800;
		}
		a.dp = a.dp + (delta) >> 0;
		trim(a);
	};
	decimal.ptr.prototype.Shift = function(k) {
		var $ptr, a, k;
		a = this;
		if ((a.nd === 0)) {
		} else if (k > 0) {
			while (true) {
				if (!(k > 28)) { break; }
				leftShift(a, 28);
				k = k - (28) >> 0;
			}
			leftShift(a, (k >>> 0));
		} else if (k < 0) {
			while (true) {
				if (!(k < -28)) { break; }
				rightShift(a, 28);
				k = k + (28) >> 0;
			}
			rightShift(a, (-k >>> 0));
		}
	};
	decimal.prototype.Shift = function(k) { return this.$val.Shift(k); };
	shouldRoundUp = function(a, nd) {
		var $ptr, _r, a, nd, x, x$1, x$2, x$3;
		if (nd < 0 || nd >= a.nd) {
			return false;
		}
		if (((x = a.d, ((nd < 0 || nd >= x.length) ? $throwRuntimeError("index out of range") : x[nd])) === 53) && ((nd + 1 >> 0) === a.nd)) {
			if (a.trunc) {
				return true;
			}
			return nd > 0 && !(((_r = (((x$1 = a.d, x$2 = nd - 1 >> 0, ((x$2 < 0 || x$2 >= x$1.length) ? $throwRuntimeError("index out of range") : x$1[x$2])) - 48 << 24 >>> 24)) % 2, _r === _r ? _r : $throwRuntimeError("integer divide by zero")) === 0));
		}
		return (x$3 = a.d, ((nd < 0 || nd >= x$3.length) ? $throwRuntimeError("index out of range") : x$3[nd])) >= 53;
	};
	decimal.ptr.prototype.Round = function(nd) {
		var $ptr, a, nd;
		a = this;
		if (nd < 0 || nd >= a.nd) {
			return;
		}
		if (shouldRoundUp(a, nd)) {
			a.RoundUp(nd);
		} else {
			a.RoundDown(nd);
		}
	};
	decimal.prototype.Round = function(nd) { return this.$val.Round(nd); };
	decimal.ptr.prototype.RoundDown = function(nd) {
		var $ptr, a, nd;
		a = this;
		if (nd < 0 || nd >= a.nd) {
			return;
		}
		a.nd = nd;
		trim(a);
	};
	decimal.prototype.RoundDown = function(nd) { return this.$val.RoundDown(nd); };
	decimal.ptr.prototype.RoundUp = function(nd) {
		var $ptr, a, c, i, nd, x, x$1, x$2;
		a = this;
		if (nd < 0 || nd >= a.nd) {
			return;
		}
		i = nd - 1 >> 0;
		while (true) {
			if (!(i >= 0)) { break; }
			c = (x = a.d, ((i < 0 || i >= x.length) ? $throwRuntimeError("index out of range") : x[i]));
			if (c < 57) {
				(x$2 = a.d, ((i < 0 || i >= x$2.length) ? $throwRuntimeError("index out of range") : x$2[i] = ((x$1 = a.d, ((i < 0 || i >= x$1.length) ? $throwRuntimeError("index out of range") : x$1[i])) + (1) << 24 >>> 24)));
				a.nd = i + 1 >> 0;
				return;
			}
			i = i - (1) >> 0;
		}
		a.d[0] = 49;
		a.nd = 1;
		a.dp = a.dp + (1) >> 0;
	};
	decimal.prototype.RoundUp = function(nd) { return this.$val.RoundUp(nd); };
	decimal.ptr.prototype.RoundedInteger = function() {
		var $ptr, a, i, n, x, x$1, x$2, x$3;
		a = this;
		if (a.dp > 20) {
			return new $Uint64(4294967295, 4294967295);
		}
		i = 0;
		n = new $Uint64(0, 0);
		i = 0;
		while (true) {
			if (!(i < a.dp && i < a.nd)) { break; }
			n = (x = $mul64(n, new $Uint64(0, 10)), x$1 = new $Uint64(0, ((x$2 = a.d, ((i < 0 || i >= x$2.length) ? $throwRuntimeError("index out of range") : x$2[i])) - 48 << 24 >>> 24)), new $Uint64(x.$high + x$1.$high, x.$low + x$1.$low));
			i = i + (1) >> 0;
		}
		while (true) {
			if (!(i < a.dp)) { break; }
			n = $mul64(n, (new $Uint64(0, 10)));
			i = i + (1) >> 0;
		}
		if (shouldRoundUp(a, a.dp)) {
			n = (x$3 = new $Uint64(0, 1), new $Uint64(n.$high + x$3.$high, n.$low + x$3.$low));
		}
		return n;
	};
	decimal.prototype.RoundedInteger = function() { return this.$val.RoundedInteger(); };
	extFloat.ptr.prototype.AssignComputeBounds = function(mant, exp, neg, flt) {
		var $ptr, _tmp, _tmp$1, exp, expBiased, f, flt, lower, mant, neg, upper, x, x$1, x$2, x$3, x$4;
		lower = new extFloat.ptr(new $Uint64(0, 0), 0, false);
		upper = new extFloat.ptr(new $Uint64(0, 0), 0, false);
		f = this;
		f.mant = mant;
		f.exp = exp - (flt.mantbits >> 0) >> 0;
		f.neg = neg;
		if (f.exp <= 0 && (x = $shiftLeft64(($shiftRightUint64(mant, (-f.exp >>> 0))), (-f.exp >>> 0)), (mant.$high === x.$high && mant.$low === x.$low))) {
			f.mant = $shiftRightUint64(f.mant, ((-f.exp >>> 0)));
			f.exp = 0;
			_tmp = $clone(f, extFloat);
			_tmp$1 = $clone(f, extFloat);
			extFloat.copy(lower, _tmp);
			extFloat.copy(upper, _tmp$1);
			return [lower, upper];
		}
		expBiased = exp - flt.bias >> 0;
		extFloat.copy(upper, new extFloat.ptr((x$1 = $mul64(new $Uint64(0, 2), f.mant), new $Uint64(x$1.$high + 0, x$1.$low + 1)), f.exp - 1 >> 0, f.neg));
		if (!((x$2 = $shiftLeft64(new $Uint64(0, 1), flt.mantbits), (mant.$high === x$2.$high && mant.$low === x$2.$low))) || (expBiased === 1)) {
			extFloat.copy(lower, new extFloat.ptr((x$3 = $mul64(new $Uint64(0, 2), f.mant), new $Uint64(x$3.$high - 0, x$3.$low - 1)), f.exp - 1 >> 0, f.neg));
		} else {
			extFloat.copy(lower, new extFloat.ptr((x$4 = $mul64(new $Uint64(0, 4), f.mant), new $Uint64(x$4.$high - 0, x$4.$low - 1)), f.exp - 2 >> 0, f.neg));
		}
		return [lower, upper];
	};
	extFloat.prototype.AssignComputeBounds = function(mant, exp, neg, flt) { return this.$val.AssignComputeBounds(mant, exp, neg, flt); };
	extFloat.ptr.prototype.Normalize = function() {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, exp, f, mant, shift, x, x$1, x$2, x$3, x$4, x$5;
		shift = 0;
		f = this;
		_tmp = f.mant;
		_tmp$1 = f.exp;
		mant = _tmp;
		exp = _tmp$1;
		if ((mant.$high === 0 && mant.$low === 0)) {
			shift = 0;
			return shift;
		}
		if ((x = $shiftRightUint64(mant, 32), (x.$high === 0 && x.$low === 0))) {
			mant = $shiftLeft64(mant, (32));
			exp = exp - (32) >> 0;
		}
		if ((x$1 = $shiftRightUint64(mant, 48), (x$1.$high === 0 && x$1.$low === 0))) {
			mant = $shiftLeft64(mant, (16));
			exp = exp - (16) >> 0;
		}
		if ((x$2 = $shiftRightUint64(mant, 56), (x$2.$high === 0 && x$2.$low === 0))) {
			mant = $shiftLeft64(mant, (8));
			exp = exp - (8) >> 0;
		}
		if ((x$3 = $shiftRightUint64(mant, 60), (x$3.$high === 0 && x$3.$low === 0))) {
			mant = $shiftLeft64(mant, (4));
			exp = exp - (4) >> 0;
		}
		if ((x$4 = $shiftRightUint64(mant, 62), (x$4.$high === 0 && x$4.$low === 0))) {
			mant = $shiftLeft64(mant, (2));
			exp = exp - (2) >> 0;
		}
		if ((x$5 = $shiftRightUint64(mant, 63), (x$5.$high === 0 && x$5.$low === 0))) {
			mant = $shiftLeft64(mant, (1));
			exp = exp - (1) >> 0;
		}
		shift = ((f.exp - exp >> 0) >>> 0);
		_tmp$2 = mant;
		_tmp$3 = exp;
		f.mant = _tmp$2;
		f.exp = _tmp$3;
		return shift;
	};
	extFloat.prototype.Normalize = function() { return this.$val.Normalize(); };
	extFloat.ptr.prototype.Multiply = function(g) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, cross1, cross2, f, fhi, flo, g, ghi, glo, rem, x, x$1, x$10, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		g = $clone(g, extFloat);
		f = this;
		_tmp = $shiftRightUint64(f.mant, 32);
		_tmp$1 = new $Uint64(0, (f.mant.$low >>> 0));
		fhi = _tmp;
		flo = _tmp$1;
		_tmp$2 = $shiftRightUint64(g.mant, 32);
		_tmp$3 = new $Uint64(0, (g.mant.$low >>> 0));
		ghi = _tmp$2;
		glo = _tmp$3;
		cross1 = $mul64(fhi, glo);
		cross2 = $mul64(flo, ghi);
		f.mant = (x = (x$1 = $mul64(fhi, ghi), x$2 = $shiftRightUint64(cross1, 32), new $Uint64(x$1.$high + x$2.$high, x$1.$low + x$2.$low)), x$3 = $shiftRightUint64(cross2, 32), new $Uint64(x.$high + x$3.$high, x.$low + x$3.$low));
		rem = (x$4 = (x$5 = new $Uint64(0, (cross1.$low >>> 0)), x$6 = new $Uint64(0, (cross2.$low >>> 0)), new $Uint64(x$5.$high + x$6.$high, x$5.$low + x$6.$low)), x$7 = $shiftRightUint64(($mul64(flo, glo)), 32), new $Uint64(x$4.$high + x$7.$high, x$4.$low + x$7.$low));
		rem = (x$8 = new $Uint64(0, 2147483648), new $Uint64(rem.$high + x$8.$high, rem.$low + x$8.$low));
		f.mant = (x$9 = f.mant, x$10 = ($shiftRightUint64(rem, 32)), new $Uint64(x$9.$high + x$10.$high, x$9.$low + x$10.$low));
		f.exp = (f.exp + g.exp >> 0) + 64 >> 0;
	};
	extFloat.prototype.Multiply = function(g) { return this.$val.Multiply(g); };
	extFloat.ptr.prototype.AssignDecimal = function(mantissa, exp10, neg, trunc, flt) {
		var $ptr, _q, _r, adjExp, denormalExp, errors$1, exp10, extrabits, f, flt, halfway, i, mant_extra, mantissa, neg, ok, shift, trunc, x, x$1, x$10, x$11, x$12, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, y;
		ok = false;
		f = this;
		errors$1 = 0;
		if (trunc) {
			errors$1 = errors$1 + (4) >> 0;
		}
		f.mant = mantissa;
		f.exp = 0;
		f.neg = neg;
		i = (_q = ((exp10 - -348 >> 0)) / 8, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
		if (exp10 < -348 || i >= 87) {
			ok = false;
			return ok;
		}
		adjExp = (_r = ((exp10 - -348 >> 0)) % 8, _r === _r ? _r : $throwRuntimeError("integer divide by zero"));
		if (adjExp < 19 && (x = (x$1 = 19 - adjExp >> 0, ((x$1 < 0 || x$1 >= uint64pow10.length) ? $throwRuntimeError("index out of range") : uint64pow10[x$1])), (mantissa.$high < x.$high || (mantissa.$high === x.$high && mantissa.$low < x.$low)))) {
			f.mant = $mul64(f.mant, (((adjExp < 0 || adjExp >= uint64pow10.length) ? $throwRuntimeError("index out of range") : uint64pow10[adjExp])));
			f.Normalize();
		} else {
			f.Normalize();
			f.Multiply(((adjExp < 0 || adjExp >= smallPowersOfTen.length) ? $throwRuntimeError("index out of range") : smallPowersOfTen[adjExp]));
			errors$1 = errors$1 + (4) >> 0;
		}
		f.Multiply(((i < 0 || i >= powersOfTen.length) ? $throwRuntimeError("index out of range") : powersOfTen[i]));
		if (errors$1 > 0) {
			errors$1 = errors$1 + (1) >> 0;
		}
		errors$1 = errors$1 + (4) >> 0;
		shift = f.Normalize();
		errors$1 = (y = (shift), y < 32 ? (errors$1 << y) : 0) >> 0;
		denormalExp = flt.bias - 63 >> 0;
		extrabits = 0;
		if (f.exp <= denormalExp) {
			extrabits = (((63 - flt.mantbits >>> 0) + 1 >>> 0) + ((denormalExp - f.exp >> 0) >>> 0) >>> 0);
		} else {
			extrabits = (63 - flt.mantbits >>> 0);
		}
		halfway = $shiftLeft64(new $Uint64(0, 1), ((extrabits - 1 >>> 0)));
		mant_extra = (x$2 = f.mant, x$3 = (x$4 = $shiftLeft64(new $Uint64(0, 1), extrabits), new $Uint64(x$4.$high - 0, x$4.$low - 1)), new $Uint64(x$2.$high & x$3.$high, (x$2.$low & x$3.$low) >>> 0));
		if ((x$5 = (x$6 = new $Int64(halfway.$high, halfway.$low), x$7 = new $Int64(0, errors$1), new $Int64(x$6.$high - x$7.$high, x$6.$low - x$7.$low)), x$8 = new $Int64(mant_extra.$high, mant_extra.$low), (x$5.$high < x$8.$high || (x$5.$high === x$8.$high && x$5.$low < x$8.$low))) && (x$9 = new $Int64(mant_extra.$high, mant_extra.$low), x$10 = (x$11 = new $Int64(halfway.$high, halfway.$low), x$12 = new $Int64(0, errors$1), new $Int64(x$11.$high + x$12.$high, x$11.$low + x$12.$low)), (x$9.$high < x$10.$high || (x$9.$high === x$10.$high && x$9.$low < x$10.$low)))) {
			ok = false;
			return ok;
		}
		ok = true;
		return ok;
	};
	extFloat.prototype.AssignDecimal = function(mantissa, exp10, neg, trunc, flt) { return this.$val.AssignDecimal(mantissa, exp10, neg, trunc, flt); };
	extFloat.ptr.prototype.frexp10 = function() {
		var $ptr, _q, _q$1, _tmp, _tmp$1, approxExp10, exp, exp10, f, i, index;
		exp10 = 0;
		index = 0;
		f = this;
		approxExp10 = (_q = ($imul(((-46 - f.exp >> 0)), 28)) / 93, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero"));
		i = (_q$1 = ((approxExp10 - -348 >> 0)) / 8, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >> 0 : $throwRuntimeError("integer divide by zero"));
		Loop:
		while (true) {
			exp = (f.exp + ((i < 0 || i >= powersOfTen.length) ? $throwRuntimeError("index out of range") : powersOfTen[i]).exp >> 0) + 64 >> 0;
			if (exp < -60) {
				i = i + (1) >> 0;
			} else if (exp > -32) {
				i = i - (1) >> 0;
			} else {
				break Loop;
			}
		}
		f.Multiply(((i < 0 || i >= powersOfTen.length) ? $throwRuntimeError("index out of range") : powersOfTen[i]));
		_tmp = -((-348 + ($imul(i, 8)) >> 0));
		_tmp$1 = i;
		exp10 = _tmp;
		index = _tmp$1;
		return [exp10, index];
	};
	extFloat.prototype.frexp10 = function() { return this.$val.frexp10(); };
	frexp10Many = function(a, b, c) {
		var $ptr, _tuple, a, b, c, exp10, i;
		exp10 = 0;
		_tuple = c.frexp10();
		exp10 = _tuple[0];
		i = _tuple[1];
		a.Multiply(((i < 0 || i >= powersOfTen.length) ? $throwRuntimeError("index out of range") : powersOfTen[i]));
		b.Multiply(((i < 0 || i >= powersOfTen.length) ? $throwRuntimeError("index out of range") : powersOfTen[i]));
		return exp10;
	};
	extFloat.ptr.prototype.FixedDecimal = function(d, n) {
		var $CE$B5, $ptr, _q, _q$1, _tmp, _tmp$1, _tuple, buf, d, digit, exp10, f, fraction, i, i$1, i$2, integer, integerDigits, n, nd, needed, ok, pos, pow, pow10, rest, shift, v, v1, x, x$1, x$10, x$11, x$12, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		f = this;
		if ((x = f.mant, (x.$high === 0 && x.$low === 0))) {
			d.nd = 0;
			d.dp = 0;
			d.neg = f.neg;
			return true;
		}
		if (n === 0) {
			$panic(new $String("strconv: internal error: extFloat.FixedDecimal called with n == 0"));
		}
		f.Normalize();
		_tuple = f.frexp10();
		exp10 = _tuple[0];
		shift = (-f.exp >>> 0);
		integer = ($shiftRightUint64(f.mant, shift).$low >>> 0);
		fraction = (x$1 = f.mant, x$2 = $shiftLeft64(new $Uint64(0, integer), shift), new $Uint64(x$1.$high - x$2.$high, x$1.$low - x$2.$low));
		$CE$B5 = new $Uint64(0, 1);
		needed = n;
		integerDigits = 0;
		pow10 = new $Uint64(0, 1);
		_tmp = 0;
		_tmp$1 = new $Uint64(0, 1);
		i = _tmp;
		pow = _tmp$1;
		while (true) {
			if (!(i < 20)) { break; }
			if ((x$3 = new $Uint64(0, integer), (pow.$high > x$3.$high || (pow.$high === x$3.$high && pow.$low > x$3.$low)))) {
				integerDigits = i;
				break;
			}
			pow = $mul64(pow, (new $Uint64(0, 10)));
			i = i + (1) >> 0;
		}
		rest = integer;
		if (integerDigits > needed) {
			pow10 = (x$4 = integerDigits - needed >> 0, ((x$4 < 0 || x$4 >= uint64pow10.length) ? $throwRuntimeError("index out of range") : uint64pow10[x$4]));
			integer = (_q = integer / ((pow10.$low >>> 0)), (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
			rest = rest - (($imul(integer, (pow10.$low >>> 0)) >>> 0)) >>> 0;
		} else {
			rest = 0;
		}
		buf = arrayType$2.zero();
		pos = 32;
		v = integer;
		while (true) {
			if (!(v > 0)) { break; }
			v1 = (_q$1 = v / 10, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >>> 0 : $throwRuntimeError("integer divide by zero"));
			v = v - (($imul(10, v1) >>> 0)) >>> 0;
			pos = pos - (1) >> 0;
			((pos < 0 || pos >= buf.length) ? $throwRuntimeError("index out of range") : buf[pos] = ((v + 48 >>> 0) << 24 >>> 24));
			v = v1;
		}
		i$1 = pos;
		while (true) {
			if (!(i$1 < 32)) { break; }
			(x$5 = d.d, x$6 = i$1 - pos >> 0, ((x$6 < 0 || x$6 >= x$5.$length) ? $throwRuntimeError("index out of range") : x$5.$array[x$5.$offset + x$6] = ((i$1 < 0 || i$1 >= buf.length) ? $throwRuntimeError("index out of range") : buf[i$1])));
			i$1 = i$1 + (1) >> 0;
		}
		nd = 32 - pos >> 0;
		d.nd = nd;
		d.dp = integerDigits + exp10 >> 0;
		needed = needed - (nd) >> 0;
		if (needed > 0) {
			if (!((rest === 0)) || !((pow10.$high === 0 && pow10.$low === 1))) {
				$panic(new $String("strconv: internal error, rest != 0 but needed > 0"));
			}
			while (true) {
				if (!(needed > 0)) { break; }
				fraction = $mul64(fraction, (new $Uint64(0, 10)));
				$CE$B5 = $mul64($CE$B5, (new $Uint64(0, 10)));
				if ((x$7 = $mul64(new $Uint64(0, 2), $CE$B5), x$8 = $shiftLeft64(new $Uint64(0, 1), shift), (x$7.$high > x$8.$high || (x$7.$high === x$8.$high && x$7.$low > x$8.$low)))) {
					return false;
				}
				digit = $shiftRightUint64(fraction, shift);
				(x$9 = d.d, ((nd < 0 || nd >= x$9.$length) ? $throwRuntimeError("index out of range") : x$9.$array[x$9.$offset + nd] = (new $Uint64(digit.$high + 0, digit.$low + 48).$low << 24 >>> 24)));
				fraction = (x$10 = $shiftLeft64(digit, shift), new $Uint64(fraction.$high - x$10.$high, fraction.$low - x$10.$low));
				nd = nd + (1) >> 0;
				needed = needed - (1) >> 0;
			}
			d.nd = nd;
		}
		ok = adjustLastDigitFixed(d, (x$11 = $shiftLeft64(new $Uint64(0, rest), shift), new $Uint64(x$11.$high | fraction.$high, (x$11.$low | fraction.$low) >>> 0)), pow10, shift, $CE$B5);
		if (!ok) {
			return false;
		}
		i$2 = d.nd - 1 >> 0;
		while (true) {
			if (!(i$2 >= 0)) { break; }
			if (!(((x$12 = d.d, ((i$2 < 0 || i$2 >= x$12.$length) ? $throwRuntimeError("index out of range") : x$12.$array[x$12.$offset + i$2])) === 48))) {
				d.nd = i$2 + 1 >> 0;
				break;
			}
			i$2 = i$2 - (1) >> 0;
		}
		return true;
	};
	extFloat.prototype.FixedDecimal = function(d, n) { return this.$val.FixedDecimal(d, n); };
	adjustLastDigitFixed = function(d, num, den, shift, $CE$B5) {
		var $CE$B5, $ptr, d, den, i, num, shift, x, x$1, x$10, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		if ((x = $shiftLeft64(den, shift), (num.$high > x.$high || (num.$high === x.$high && num.$low > x.$low)))) {
			$panic(new $String("strconv: num > den<<shift in adjustLastDigitFixed"));
		}
		if ((x$1 = $mul64(new $Uint64(0, 2), $CE$B5), x$2 = $shiftLeft64(den, shift), (x$1.$high > x$2.$high || (x$1.$high === x$2.$high && x$1.$low > x$2.$low)))) {
			$panic(new $String("strconv: \xCE\xB5 > (den<<shift)/2"));
		}
		if ((x$3 = $mul64(new $Uint64(0, 2), (new $Uint64(num.$high + $CE$B5.$high, num.$low + $CE$B5.$low))), x$4 = $shiftLeft64(den, shift), (x$3.$high < x$4.$high || (x$3.$high === x$4.$high && x$3.$low < x$4.$low)))) {
			return true;
		}
		if ((x$5 = $mul64(new $Uint64(0, 2), (new $Uint64(num.$high - $CE$B5.$high, num.$low - $CE$B5.$low))), x$6 = $shiftLeft64(den, shift), (x$5.$high > x$6.$high || (x$5.$high === x$6.$high && x$5.$low > x$6.$low)))) {
			i = d.nd - 1 >> 0;
			while (true) {
				if (!(i >= 0)) { break; }
				if ((x$7 = d.d, ((i < 0 || i >= x$7.$length) ? $throwRuntimeError("index out of range") : x$7.$array[x$7.$offset + i])) === 57) {
					d.nd = d.nd - (1) >> 0;
				} else {
					break;
				}
				i = i - (1) >> 0;
			}
			if (i < 0) {
				(x$8 = d.d, (0 >= x$8.$length ? $throwRuntimeError("index out of range") : x$8.$array[x$8.$offset + 0] = 49));
				d.nd = 1;
				d.dp = d.dp + (1) >> 0;
			} else {
				(x$10 = d.d, ((i < 0 || i >= x$10.$length) ? $throwRuntimeError("index out of range") : x$10.$array[x$10.$offset + i] = ((x$9 = d.d, ((i < 0 || i >= x$9.$length) ? $throwRuntimeError("index out of range") : x$9.$array[x$9.$offset + i])) + (1) << 24 >>> 24)));
			}
			return true;
		}
		return false;
	};
	extFloat.ptr.prototype.ShortestDecimal = function(d, lower, upper) {
		var $ptr, _q, _tmp, _tmp$1, _tmp$2, _tmp$3, allowance, buf, currentDiff, d, digit, digit$1, exp10, f, fraction, i, i$1, i$2, integer, integerDigits, lower, multiplier, n, nd, pow, pow$1, shift, targetDiff, upper, v, v1, x, x$1, x$10, x$11, x$12, x$13, x$14, x$15, x$16, x$17, x$18, x$19, x$2, x$20, x$21, x$22, x$23, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		f = this;
		if ((x = f.mant, (x.$high === 0 && x.$low === 0))) {
			d.nd = 0;
			d.dp = 0;
			d.neg = f.neg;
			return true;
		}
		if ((f.exp === 0) && $equal(lower, f, extFloat) && $equal(lower, upper, extFloat)) {
			buf = arrayType$1.zero();
			n = 23;
			v = f.mant;
			while (true) {
				if (!((v.$high > 0 || (v.$high === 0 && v.$low > 0)))) { break; }
				v1 = $div64(v, new $Uint64(0, 10), false);
				v = (x$1 = $mul64(new $Uint64(0, 10), v1), new $Uint64(v.$high - x$1.$high, v.$low - x$1.$low));
				((n < 0 || n >= buf.length) ? $throwRuntimeError("index out of range") : buf[n] = (new $Uint64(v.$high + 0, v.$low + 48).$low << 24 >>> 24));
				n = n - (1) >> 0;
				v = v1;
			}
			nd = (24 - n >> 0) - 1 >> 0;
			i = 0;
			while (true) {
				if (!(i < nd)) { break; }
				(x$3 = d.d, ((i < 0 || i >= x$3.$length) ? $throwRuntimeError("index out of range") : x$3.$array[x$3.$offset + i] = (x$2 = (n + 1 >> 0) + i >> 0, ((x$2 < 0 || x$2 >= buf.length) ? $throwRuntimeError("index out of range") : buf[x$2]))));
				i = i + (1) >> 0;
			}
			_tmp = nd;
			_tmp$1 = nd;
			d.nd = _tmp;
			d.dp = _tmp$1;
			while (true) {
				if (!(d.nd > 0 && ((x$4 = d.d, x$5 = d.nd - 1 >> 0, ((x$5 < 0 || x$5 >= x$4.$length) ? $throwRuntimeError("index out of range") : x$4.$array[x$4.$offset + x$5])) === 48))) { break; }
				d.nd = d.nd - (1) >> 0;
			}
			if (d.nd === 0) {
				d.dp = 0;
			}
			d.neg = f.neg;
			return true;
		}
		upper.Normalize();
		if (f.exp > upper.exp) {
			f.mant = $shiftLeft64(f.mant, (((f.exp - upper.exp >> 0) >>> 0)));
			f.exp = upper.exp;
		}
		if (lower.exp > upper.exp) {
			lower.mant = $shiftLeft64(lower.mant, (((lower.exp - upper.exp >> 0) >>> 0)));
			lower.exp = upper.exp;
		}
		exp10 = frexp10Many(lower, f, upper);
		upper.mant = (x$6 = upper.mant, x$7 = new $Uint64(0, 1), new $Uint64(x$6.$high + x$7.$high, x$6.$low + x$7.$low));
		lower.mant = (x$8 = lower.mant, x$9 = new $Uint64(0, 1), new $Uint64(x$8.$high - x$9.$high, x$8.$low - x$9.$low));
		shift = (-upper.exp >>> 0);
		integer = ($shiftRightUint64(upper.mant, shift).$low >>> 0);
		fraction = (x$10 = upper.mant, x$11 = $shiftLeft64(new $Uint64(0, integer), shift), new $Uint64(x$10.$high - x$11.$high, x$10.$low - x$11.$low));
		allowance = (x$12 = upper.mant, x$13 = lower.mant, new $Uint64(x$12.$high - x$13.$high, x$12.$low - x$13.$low));
		targetDiff = (x$14 = upper.mant, x$15 = f.mant, new $Uint64(x$14.$high - x$15.$high, x$14.$low - x$15.$low));
		integerDigits = 0;
		_tmp$2 = 0;
		_tmp$3 = new $Uint64(0, 1);
		i$1 = _tmp$2;
		pow = _tmp$3;
		while (true) {
			if (!(i$1 < 20)) { break; }
			if ((x$16 = new $Uint64(0, integer), (pow.$high > x$16.$high || (pow.$high === x$16.$high && pow.$low > x$16.$low)))) {
				integerDigits = i$1;
				break;
			}
			pow = $mul64(pow, (new $Uint64(0, 10)));
			i$1 = i$1 + (1) >> 0;
		}
		i$2 = 0;
		while (true) {
			if (!(i$2 < integerDigits)) { break; }
			pow$1 = (x$17 = (integerDigits - i$2 >> 0) - 1 >> 0, ((x$17 < 0 || x$17 >= uint64pow10.length) ? $throwRuntimeError("index out of range") : uint64pow10[x$17]));
			digit = (_q = integer / (pow$1.$low >>> 0), (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
			(x$18 = d.d, ((i$2 < 0 || i$2 >= x$18.$length) ? $throwRuntimeError("index out of range") : x$18.$array[x$18.$offset + i$2] = ((digit + 48 >>> 0) << 24 >>> 24)));
			integer = integer - (($imul(digit, (pow$1.$low >>> 0)) >>> 0)) >>> 0;
			currentDiff = (x$19 = $shiftLeft64(new $Uint64(0, integer), shift), new $Uint64(x$19.$high + fraction.$high, x$19.$low + fraction.$low));
			if ((currentDiff.$high < allowance.$high || (currentDiff.$high === allowance.$high && currentDiff.$low < allowance.$low))) {
				d.nd = i$2 + 1 >> 0;
				d.dp = integerDigits + exp10 >> 0;
				d.neg = f.neg;
				return adjustLastDigit(d, currentDiff, targetDiff, allowance, $shiftLeft64(pow$1, shift), new $Uint64(0, 2));
			}
			i$2 = i$2 + (1) >> 0;
		}
		d.nd = integerDigits;
		d.dp = d.nd + exp10 >> 0;
		d.neg = f.neg;
		digit$1 = 0;
		multiplier = new $Uint64(0, 1);
		while (true) {
			fraction = $mul64(fraction, (new $Uint64(0, 10)));
			multiplier = $mul64(multiplier, (new $Uint64(0, 10)));
			digit$1 = ($shiftRightUint64(fraction, shift).$low >> 0);
			(x$20 = d.d, x$21 = d.nd, ((x$21 < 0 || x$21 >= x$20.$length) ? $throwRuntimeError("index out of range") : x$20.$array[x$20.$offset + x$21] = ((digit$1 + 48 >> 0) << 24 >>> 24)));
			d.nd = d.nd + (1) >> 0;
			fraction = (x$22 = $shiftLeft64(new $Uint64(0, digit$1), shift), new $Uint64(fraction.$high - x$22.$high, fraction.$low - x$22.$low));
			if ((x$23 = $mul64(allowance, multiplier), (fraction.$high < x$23.$high || (fraction.$high === x$23.$high && fraction.$low < x$23.$low)))) {
				return adjustLastDigit(d, fraction, $mul64(targetDiff, multiplier), $mul64(allowance, multiplier), $shiftLeft64(new $Uint64(0, 1), shift), $mul64(multiplier, new $Uint64(0, 2)));
			}
		}
	};
	extFloat.prototype.ShortestDecimal = function(d, lower, upper) { return this.$val.ShortestDecimal(d, lower, upper); };
	adjustLastDigit = function(d, currentDiff, targetDiff, maxDiff, ulpDecimal, ulpBinary) {
		var $ptr, _index, currentDiff, d, maxDiff, targetDiff, ulpBinary, ulpDecimal, x, x$1, x$10, x$11, x$12, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		if ((x = $mul64(new $Uint64(0, 2), ulpBinary), (ulpDecimal.$high < x.$high || (ulpDecimal.$high === x.$high && ulpDecimal.$low < x.$low)))) {
			return false;
		}
		while (true) {
			if (!((x$1 = (x$2 = (x$3 = $div64(ulpDecimal, new $Uint64(0, 2), false), new $Uint64(currentDiff.$high + x$3.$high, currentDiff.$low + x$3.$low)), new $Uint64(x$2.$high + ulpBinary.$high, x$2.$low + ulpBinary.$low)), (x$1.$high < targetDiff.$high || (x$1.$high === targetDiff.$high && x$1.$low < targetDiff.$low))))) { break; }
			_index = d.nd - 1 >> 0;
			(x$5 = d.d, ((_index < 0 || _index >= x$5.$length) ? $throwRuntimeError("index out of range") : x$5.$array[x$5.$offset + _index] = ((x$4 = d.d, ((_index < 0 || _index >= x$4.$length) ? $throwRuntimeError("index out of range") : x$4.$array[x$4.$offset + _index])) - (1) << 24 >>> 24)));
			currentDiff = (x$6 = ulpDecimal, new $Uint64(currentDiff.$high + x$6.$high, currentDiff.$low + x$6.$low));
		}
		if ((x$7 = new $Uint64(currentDiff.$high + ulpDecimal.$high, currentDiff.$low + ulpDecimal.$low), x$8 = (x$9 = (x$10 = $div64(ulpDecimal, new $Uint64(0, 2), false), new $Uint64(targetDiff.$high + x$10.$high, targetDiff.$low + x$10.$low)), new $Uint64(x$9.$high + ulpBinary.$high, x$9.$low + ulpBinary.$low)), (x$7.$high < x$8.$high || (x$7.$high === x$8.$high && x$7.$low <= x$8.$low)))) {
			return false;
		}
		if ((currentDiff.$high < ulpBinary.$high || (currentDiff.$high === ulpBinary.$high && currentDiff.$low < ulpBinary.$low)) || (x$11 = new $Uint64(maxDiff.$high - ulpBinary.$high, maxDiff.$low - ulpBinary.$low), (currentDiff.$high > x$11.$high || (currentDiff.$high === x$11.$high && currentDiff.$low > x$11.$low)))) {
			return false;
		}
		if ((d.nd === 1) && ((x$12 = d.d, (0 >= x$12.$length ? $throwRuntimeError("index out of range") : x$12.$array[x$12.$offset + 0])) === 48)) {
			d.nd = 0;
			d.dp = 0;
		}
		return true;
	};
	AppendFloat = function(dst, f, fmt, prec, bitSize) {
		var $ptr, bitSize, dst, f, fmt, prec;
		return genericFtoa(dst, f, fmt, prec, bitSize);
	};
	$pkg.AppendFloat = AppendFloat;
	genericFtoa = function(dst, val, fmt, prec, bitSize) {
		var $ptr, _1, _2, _3, _4, _tuple, bitSize, bits, buf, buf$1, digits, digs, dst, exp, f, f$1, flt, fmt, lower, mant, neg, ok, prec, s, shortest, upper, val, x, x$1, x$2, x$3, y, y$1;
		bits = new $Uint64(0, 0);
		flt = ptrType$1.nil;
		_1 = bitSize;
		if (_1 === (32)) {
			bits = new $Uint64(0, math.Float32bits($fround(val)));
			flt = float32info;
		} else if (_1 === (64)) {
			bits = math.Float64bits(val);
			flt = float64info;
		} else {
			$panic(new $String("strconv: illegal AppendFloat/FormatFloat bitSize"));
		}
		neg = !((x = $shiftRightUint64(bits, ((flt.expbits + flt.mantbits >>> 0))), (x.$high === 0 && x.$low === 0)));
		exp = ($shiftRightUint64(bits, flt.mantbits).$low >> 0) & ((((y = flt.expbits, y < 32 ? (1 << y) : 0) >> 0) - 1 >> 0));
		mant = (x$1 = (x$2 = $shiftLeft64(new $Uint64(0, 1), flt.mantbits), new $Uint64(x$2.$high - 0, x$2.$low - 1)), new $Uint64(bits.$high & x$1.$high, (bits.$low & x$1.$low) >>> 0));
		_2 = exp;
		if (_2 === ((((y$1 = flt.expbits, y$1 < 32 ? (1 << y$1) : 0) >> 0) - 1 >> 0))) {
			s = "";
			if (!((mant.$high === 0 && mant.$low === 0))) {
				s = "NaN";
			} else if (neg) {
				s = "-Inf";
			} else {
				s = "+Inf";
			}
			return $appendSlice(dst, s);
		} else if (_2 === (0)) {
			exp = exp + (1) >> 0;
		} else {
			mant = (x$3 = $shiftLeft64(new $Uint64(0, 1), flt.mantbits), new $Uint64(mant.$high | x$3.$high, (mant.$low | x$3.$low) >>> 0));
		}
		exp = exp + (flt.bias) >> 0;
		if (fmt === 98) {
			return fmtB(dst, neg, mant, exp, flt);
		}
		if (!optimize) {
			return bigFtoa(dst, prec, fmt, neg, mant, exp, flt);
		}
		digs = new decimalSlice.ptr(sliceType$6.nil, 0, 0, false);
		ok = false;
		shortest = prec < 0;
		if (shortest) {
			f = new extFloat.ptr(new $Uint64(0, 0), 0, false);
			_tuple = f.AssignComputeBounds(mant, exp, neg, flt);
			lower = $clone(_tuple[0], extFloat);
			upper = $clone(_tuple[1], extFloat);
			buf = arrayType$2.zero();
			digs.d = new sliceType$6(buf);
			ok = f.ShortestDecimal(digs, lower, upper);
			if (!ok) {
				return bigFtoa(dst, prec, fmt, neg, mant, exp, flt);
			}
			_3 = fmt;
			if ((_3 === (101)) || (_3 === (69))) {
				prec = max(digs.nd - 1 >> 0, 0);
			} else if (_3 === (102)) {
				prec = max(digs.nd - digs.dp >> 0, 0);
			} else if ((_3 === (103)) || (_3 === (71))) {
				prec = digs.nd;
			}
		} else if (!((fmt === 102))) {
			digits = prec;
			_4 = fmt;
			if ((_4 === (101)) || (_4 === (69))) {
				digits = digits + (1) >> 0;
			} else if ((_4 === (103)) || (_4 === (71))) {
				if (prec === 0) {
					prec = 1;
				}
				digits = prec;
			}
			if (digits <= 15) {
				buf$1 = arrayType$1.zero();
				digs.d = new sliceType$6(buf$1);
				f$1 = new extFloat.ptr(mant, exp - (flt.mantbits >> 0) >> 0, neg);
				ok = f$1.FixedDecimal(digs, digits);
			}
		}
		if (!ok) {
			return bigFtoa(dst, prec, fmt, neg, mant, exp, flt);
		}
		return formatDigits(dst, shortest, neg, digs, prec, fmt);
	};
	bigFtoa = function(dst, prec, fmt, neg, mant, exp, flt) {
		var $ptr, _5, _6, d, digs, dst, exp, flt, fmt, mant, neg, prec, shortest;
		d = new decimal.ptr(arrayType.zero(), 0, 0, false, false);
		d.Assign(mant);
		d.Shift(exp - (flt.mantbits >> 0) >> 0);
		digs = new decimalSlice.ptr(sliceType$6.nil, 0, 0, false);
		shortest = prec < 0;
		if (shortest) {
			roundShortest(d, mant, exp, flt);
			decimalSlice.copy(digs, new decimalSlice.ptr(new sliceType$6(d.d), d.nd, d.dp, false));
			_5 = fmt;
			if ((_5 === (101)) || (_5 === (69))) {
				prec = digs.nd - 1 >> 0;
			} else if (_5 === (102)) {
				prec = max(digs.nd - digs.dp >> 0, 0);
			} else if ((_5 === (103)) || (_5 === (71))) {
				prec = digs.nd;
			}
		} else {
			_6 = fmt;
			if ((_6 === (101)) || (_6 === (69))) {
				d.Round(prec + 1 >> 0);
			} else if (_6 === (102)) {
				d.Round(d.dp + prec >> 0);
			} else if ((_6 === (103)) || (_6 === (71))) {
				if (prec === 0) {
					prec = 1;
				}
				d.Round(prec);
			}
			decimalSlice.copy(digs, new decimalSlice.ptr(new sliceType$6(d.d), d.nd, d.dp, false));
		}
		return formatDigits(dst, shortest, neg, digs, prec, fmt);
	};
	formatDigits = function(dst, shortest, neg, digs, prec, fmt) {
		var $ptr, _7, digs, dst, eprec, exp, fmt, neg, prec, shortest;
		digs = $clone(digs, decimalSlice);
		_7 = fmt;
		if ((_7 === (101)) || (_7 === (69))) {
			return fmtE(dst, neg, digs, prec, fmt);
		} else if (_7 === (102)) {
			return fmtF(dst, neg, digs, prec);
		} else if ((_7 === (103)) || (_7 === (71))) {
			eprec = prec;
			if (eprec > digs.nd && digs.nd >= digs.dp) {
				eprec = digs.nd;
			}
			if (shortest) {
				eprec = 6;
			}
			exp = digs.dp - 1 >> 0;
			if (exp < -4 || exp >= eprec) {
				if (prec > digs.nd) {
					prec = digs.nd;
				}
				return fmtE(dst, neg, digs, prec - 1 >> 0, (fmt + 101 << 24 >>> 24) - 103 << 24 >>> 24);
			}
			if (prec > digs.dp) {
				prec = digs.nd;
			}
			return fmtF(dst, neg, digs, max(prec - digs.dp >> 0, 0));
		}
		return $append(dst, 37, fmt);
	};
	roundShortest = function(d, mant, exp, flt) {
		var $ptr, d, exp, explo, flt, i, inclusive, l, lower, m, mant, mantlo, minexp, okdown, okup, u, upper, x, x$1, x$2, x$3, x$4, x$5, x$6, x$7;
		if ((mant.$high === 0 && mant.$low === 0)) {
			d.nd = 0;
			return;
		}
		minexp = flt.bias + 1 >> 0;
		if (exp > minexp && ($imul(332, ((d.dp - d.nd >> 0)))) >= ($imul(100, ((exp - (flt.mantbits >> 0) >> 0))))) {
			return;
		}
		upper = new decimal.ptr(arrayType.zero(), 0, 0, false, false);
		upper.Assign((x = $mul64(mant, new $Uint64(0, 2)), new $Uint64(x.$high + 0, x.$low + 1)));
		upper.Shift((exp - (flt.mantbits >> 0) >> 0) - 1 >> 0);
		mantlo = new $Uint64(0, 0);
		explo = 0;
		if ((x$1 = $shiftLeft64(new $Uint64(0, 1), flt.mantbits), (mant.$high > x$1.$high || (mant.$high === x$1.$high && mant.$low > x$1.$low))) || (exp === minexp)) {
			mantlo = new $Uint64(mant.$high - 0, mant.$low - 1);
			explo = exp;
		} else {
			mantlo = (x$2 = $mul64(mant, new $Uint64(0, 2)), new $Uint64(x$2.$high - 0, x$2.$low - 1));
			explo = exp - 1 >> 0;
		}
		lower = new decimal.ptr(arrayType.zero(), 0, 0, false, false);
		lower.Assign((x$3 = $mul64(mantlo, new $Uint64(0, 2)), new $Uint64(x$3.$high + 0, x$3.$low + 1)));
		lower.Shift((explo - (flt.mantbits >> 0) >> 0) - 1 >> 0);
		inclusive = (x$4 = $div64(mant, new $Uint64(0, 2), true), (x$4.$high === 0 && x$4.$low === 0));
		i = 0;
		while (true) {
			if (!(i < d.nd)) { break; }
			l = 48;
			if (i < lower.nd) {
				l = (x$5 = lower.d, ((i < 0 || i >= x$5.length) ? $throwRuntimeError("index out of range") : x$5[i]));
			}
			m = (x$6 = d.d, ((i < 0 || i >= x$6.length) ? $throwRuntimeError("index out of range") : x$6[i]));
			u = 48;
			if (i < upper.nd) {
				u = (x$7 = upper.d, ((i < 0 || i >= x$7.length) ? $throwRuntimeError("index out of range") : x$7[i]));
			}
			okdown = !((l === m)) || inclusive && ((i + 1 >> 0) === lower.nd);
			okup = !((m === u)) && (inclusive || (m + 1 << 24 >>> 24) < u || (i + 1 >> 0) < upper.nd);
			if (okdown && okup) {
				d.Round(i + 1 >> 0);
				return;
			} else if (okdown) {
				d.RoundDown(i + 1 >> 0);
				return;
			} else if (okup) {
				d.RoundUp(i + 1 >> 0);
				return;
			}
			i = i + (1) >> 0;
		}
	};
	fmtE = function(dst, neg, d, prec, fmt) {
		var $ptr, _q, _q$1, _q$2, _r, _r$1, _r$2, ch, d, dst, exp, fmt, i, m, neg, prec, x;
		d = $clone(d, decimalSlice);
		if (neg) {
			dst = $append(dst, 45);
		}
		ch = 48;
		if (!((d.nd === 0))) {
			ch = (x = d.d, (0 >= x.$length ? $throwRuntimeError("index out of range") : x.$array[x.$offset + 0]));
		}
		dst = $append(dst, ch);
		if (prec > 0) {
			dst = $append(dst, 46);
			i = 1;
			m = min(d.nd, prec + 1 >> 0);
			if (i < m) {
				dst = $appendSlice(dst, $subslice(d.d, i, m));
				i = m;
			}
			while (true) {
				if (!(i <= prec)) { break; }
				dst = $append(dst, 48);
				i = i + (1) >> 0;
			}
		}
		dst = $append(dst, fmt);
		exp = d.dp - 1 >> 0;
		if (d.nd === 0) {
			exp = 0;
		}
		if (exp < 0) {
			ch = 45;
			exp = -exp;
		} else {
			ch = 43;
		}
		dst = $append(dst, ch);
		if (exp < 10) {
			dst = $append(dst, 48, (exp << 24 >>> 24) + 48 << 24 >>> 24);
		} else if (exp < 100) {
			dst = $append(dst, ((_q = exp / 10, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) << 24 >>> 24) + 48 << 24 >>> 24, ((_r = exp % 10, _r === _r ? _r : $throwRuntimeError("integer divide by zero")) << 24 >>> 24) + 48 << 24 >>> 24);
		} else {
			dst = $append(dst, ((_q$1 = exp / 100, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >> 0 : $throwRuntimeError("integer divide by zero")) << 24 >>> 24) + 48 << 24 >>> 24, (_r$1 = ((_q$2 = exp / 10, (_q$2 === _q$2 && _q$2 !== 1/0 && _q$2 !== -1/0) ? _q$2 >> 0 : $throwRuntimeError("integer divide by zero")) << 24 >>> 24) % 10, _r$1 === _r$1 ? _r$1 : $throwRuntimeError("integer divide by zero")) + 48 << 24 >>> 24, ((_r$2 = exp % 10, _r$2 === _r$2 ? _r$2 : $throwRuntimeError("integer divide by zero")) << 24 >>> 24) + 48 << 24 >>> 24);
		}
		return dst;
	};
	fmtF = function(dst, neg, d, prec) {
		var $ptr, ch, d, dst, i, j, m, neg, prec, x;
		d = $clone(d, decimalSlice);
		if (neg) {
			dst = $append(dst, 45);
		}
		if (d.dp > 0) {
			m = min(d.nd, d.dp);
			dst = $appendSlice(dst, $subslice(d.d, 0, m));
			while (true) {
				if (!(m < d.dp)) { break; }
				dst = $append(dst, 48);
				m = m + (1) >> 0;
			}
		} else {
			dst = $append(dst, 48);
		}
		if (prec > 0) {
			dst = $append(dst, 46);
			i = 0;
			while (true) {
				if (!(i < prec)) { break; }
				ch = 48;
				j = d.dp + i >> 0;
				if (0 <= j && j < d.nd) {
					ch = (x = d.d, ((j < 0 || j >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + j]));
				}
				dst = $append(dst, ch);
				i = i + (1) >> 0;
			}
		}
		return dst;
	};
	fmtB = function(dst, neg, mant, exp, flt) {
		var $ptr, _tuple, _tuple$1, dst, exp, flt, mant, neg;
		if (neg) {
			dst = $append(dst, 45);
		}
		_tuple = formatBits(dst, mant, 10, false, true);
		dst = _tuple[0];
		dst = $append(dst, 112);
		exp = exp - ((flt.mantbits >> 0)) >> 0;
		if (exp >= 0) {
			dst = $append(dst, 43);
		}
		_tuple$1 = formatBits(dst, new $Uint64(0, exp), 10, exp < 0, true);
		dst = _tuple$1[0];
		return dst;
	};
	min = function(a, b) {
		var $ptr, a, b;
		if (a < b) {
			return a;
		}
		return b;
	};
	max = function(a, b) {
		var $ptr, a, b;
		if (a > b) {
			return a;
		}
		return b;
	};
	FormatInt = function(i, base) {
		var $ptr, _tuple, base, i, s;
		_tuple = formatBits(sliceType$6.nil, new $Uint64(i.$high, i.$low), base, (i.$high < 0 || (i.$high === 0 && i.$low < 0)), false);
		s = _tuple[1];
		return s;
	};
	$pkg.FormatInt = FormatInt;
	Itoa = function(i) {
		var $ptr, i;
		return FormatInt(new $Int64(0, i), 10);
	};
	$pkg.Itoa = Itoa;
	formatBits = function(dst, u, base, neg, append_) {
		var $ptr, _q, _q$1, a, append_, b, b$1, base, d, dst, i, j, m, neg, q, q$1, q$2, qs, s, s$1, u, us, us$1, x, x$1;
		d = sliceType$6.nil;
		s = "";
		if (base < 2 || base > 36) {
			$panic(new $String("strconv: illegal AppendInt/FormatInt base"));
		}
		a = arrayType$3.zero();
		i = 65;
		if (neg) {
			u = new $Uint64(-u.$high, -u.$low);
		}
		if (base === 10) {
			if (true) {
				while (true) {
					if (!((u.$high > 0 || (u.$high === 0 && u.$low > 4294967295)))) { break; }
					q = $div64(u, new $Uint64(0, 1000000000), false);
					us = ((x = $mul64(q, new $Uint64(0, 1000000000)), new $Uint64(u.$high - x.$high, u.$low - x.$low)).$low >>> 0);
					j = 9;
					while (true) {
						if (!(j > 0)) { break; }
						i = i - (1) >> 0;
						qs = (_q = us / 10, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >>> 0 : $throwRuntimeError("integer divide by zero"));
						((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = (((us - ($imul(qs, 10) >>> 0) >>> 0) + 48 >>> 0) << 24 >>> 24));
						us = qs;
						j = j - (1) >> 0;
					}
					u = q;
				}
			}
			us$1 = (u.$low >>> 0);
			while (true) {
				if (!(us$1 >= 10)) { break; }
				i = i - (1) >> 0;
				q$1 = (_q$1 = us$1 / 10, (_q$1 === _q$1 && _q$1 !== 1/0 && _q$1 !== -1/0) ? _q$1 >>> 0 : $throwRuntimeError("integer divide by zero"));
				((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = (((us$1 - ($imul(q$1, 10) >>> 0) >>> 0) + 48 >>> 0) << 24 >>> 24));
				us$1 = q$1;
			}
			i = i - (1) >> 0;
			((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = ((us$1 + 48 >>> 0) << 24 >>> 24));
		} else {
			s$1 = ((base < 0 || base >= shifts.length) ? $throwRuntimeError("index out of range") : shifts[base]);
			if (s$1 > 0) {
				b = new $Uint64(0, base);
				m = (b.$low >>> 0) - 1 >>> 0;
				while (true) {
					if (!((u.$high > b.$high || (u.$high === b.$high && u.$low >= b.$low)))) { break; }
					i = i - (1) >> 0;
					((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = "0123456789abcdefghijklmnopqrstuvwxyz".charCodeAt((((u.$low >>> 0) & m) >>> 0)));
					u = $shiftRightUint64(u, (s$1));
				}
				i = i - (1) >> 0;
				((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = "0123456789abcdefghijklmnopqrstuvwxyz".charCodeAt((u.$low >>> 0)));
			} else {
				b$1 = new $Uint64(0, base);
				while (true) {
					if (!((u.$high > b$1.$high || (u.$high === b$1.$high && u.$low >= b$1.$low)))) { break; }
					i = i - (1) >> 0;
					q$2 = $div64(u, b$1, false);
					((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = "0123456789abcdefghijklmnopqrstuvwxyz".charCodeAt(((x$1 = $mul64(q$2, b$1), new $Uint64(u.$high - x$1.$high, u.$low - x$1.$low)).$low >>> 0)));
					u = q$2;
				}
				i = i - (1) >> 0;
				((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = "0123456789abcdefghijklmnopqrstuvwxyz".charCodeAt((u.$low >>> 0)));
			}
		}
		if (neg) {
			i = i - (1) >> 0;
			((i < 0 || i >= a.length) ? $throwRuntimeError("index out of range") : a[i] = 45);
		}
		if (append_) {
			d = $appendSlice(dst, $subslice(new sliceType$6(a), i));
			return [d, s];
		}
		s = $bytesToString($subslice(new sliceType$6(a), i));
		return [d, s];
	};
	quoteWith = function(s, quote, ASCIIonly, graphicOnly) {
		var $ptr, ASCIIonly, _1, _q, _tuple, buf, graphicOnly, n, quote, r, runeTmp, s, s$1, s$2, width;
		runeTmp = arrayType$4.zero();
		buf = $makeSlice(sliceType$6, 0, (_q = ($imul(3, s.length)) / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")));
		buf = $append(buf, quote);
		width = 0;
		while (true) {
			if (!(s.length > 0)) { break; }
			r = (s.charCodeAt(0) >> 0);
			width = 1;
			if (r >= 128) {
				_tuple = utf8.DecodeRuneInString(s);
				r = _tuple[0];
				width = _tuple[1];
			}
			if ((width === 1) && (r === 65533)) {
				buf = $appendSlice(buf, "\\x");
				buf = $append(buf, "0123456789abcdef".charCodeAt((s.charCodeAt(0) >>> 4 << 24 >>> 24)));
				buf = $append(buf, "0123456789abcdef".charCodeAt(((s.charCodeAt(0) & 15) >>> 0)));
				s = s.substring(width);
				continue;
			}
			if ((r === (quote >> 0)) || (r === 92)) {
				buf = $append(buf, 92);
				buf = $append(buf, (r << 24 >>> 24));
				s = s.substring(width);
				continue;
			}
			if (ASCIIonly) {
				if (r < 128 && IsPrint(r)) {
					buf = $append(buf, (r << 24 >>> 24));
					s = s.substring(width);
					continue;
				}
			} else if (IsPrint(r) || graphicOnly && isInGraphicList(r)) {
				n = utf8.EncodeRune(new sliceType$6(runeTmp), r);
				buf = $appendSlice(buf, $subslice(new sliceType$6(runeTmp), 0, n));
				s = s.substring(width);
				continue;
			}
			_1 = r;
			if (_1 === (7)) {
				buf = $appendSlice(buf, "\\a");
			} else if (_1 === (8)) {
				buf = $appendSlice(buf, "\\b");
			} else if (_1 === (12)) {
				buf = $appendSlice(buf, "\\f");
			} else if (_1 === (10)) {
				buf = $appendSlice(buf, "\\n");
			} else if (_1 === (13)) {
				buf = $appendSlice(buf, "\\r");
			} else if (_1 === (9)) {
				buf = $appendSlice(buf, "\\t");
			} else if (_1 === (11)) {
				buf = $appendSlice(buf, "\\v");
			} else {
				if (r < 32) {
					buf = $appendSlice(buf, "\\x");
					buf = $append(buf, "0123456789abcdef".charCodeAt((s.charCodeAt(0) >>> 4 << 24 >>> 24)));
					buf = $append(buf, "0123456789abcdef".charCodeAt(((s.charCodeAt(0) & 15) >>> 0)));
				} else if (r > 1114111) {
					r = 65533;
					buf = $appendSlice(buf, "\\u");
					s$1 = 12;
					while (true) {
						if (!(s$1 >= 0)) { break; }
						buf = $append(buf, "0123456789abcdef".charCodeAt((((r >> $min((s$1 >>> 0), 31)) >> 0) & 15)));
						s$1 = s$1 - (4) >> 0;
					}
				} else if (r < 65536) {
					buf = $appendSlice(buf, "\\u");
					s$1 = 12;
					while (true) {
						if (!(s$1 >= 0)) { break; }
						buf = $append(buf, "0123456789abcdef".charCodeAt((((r >> $min((s$1 >>> 0), 31)) >> 0) & 15)));
						s$1 = s$1 - (4) >> 0;
					}
				} else {
					buf = $appendSlice(buf, "\\U");
					s$2 = 28;
					while (true) {
						if (!(s$2 >= 0)) { break; }
						buf = $append(buf, "0123456789abcdef".charCodeAt((((r >> $min((s$2 >>> 0), 31)) >> 0) & 15)));
						s$2 = s$2 - (4) >> 0;
					}
				}
			}
			s = s.substring(width);
		}
		buf = $append(buf, quote);
		return $bytesToString(buf);
	};
	Quote = function(s) {
		var $ptr, s;
		return quoteWith(s, 34, false, false);
	};
	$pkg.Quote = Quote;
	QuoteToASCII = function(s) {
		var $ptr, s;
		return quoteWith(s, 34, true, false);
	};
	$pkg.QuoteToASCII = QuoteToASCII;
	QuoteRune = function(r) {
		var $ptr, r;
		return quoteWith($encodeRune(r), 39, false, false);
	};
	$pkg.QuoteRune = QuoteRune;
	AppendQuoteRune = function(dst, r) {
		var $ptr, dst, r;
		return $appendSlice(dst, QuoteRune(r));
	};
	$pkg.AppendQuoteRune = AppendQuoteRune;
	QuoteRuneToASCII = function(r) {
		var $ptr, r;
		return quoteWith($encodeRune(r), 39, true, false);
	};
	$pkg.QuoteRuneToASCII = QuoteRuneToASCII;
	AppendQuoteRuneToASCII = function(dst, r) {
		var $ptr, dst, r;
		return $appendSlice(dst, QuoteRuneToASCII(r));
	};
	$pkg.AppendQuoteRuneToASCII = AppendQuoteRuneToASCII;
	CanBackquote = function(s) {
		var $ptr, _tuple, r, s, wid;
		while (true) {
			if (!(s.length > 0)) { break; }
			_tuple = utf8.DecodeRuneInString(s);
			r = _tuple[0];
			wid = _tuple[1];
			s = s.substring(wid);
			if (wid > 1) {
				if (r === 65279) {
					return false;
				}
				continue;
			}
			if (r === 65533) {
				return false;
			}
			if ((r < 32 && !((r === 9))) || (r === 96) || (r === 127)) {
				return false;
			}
		}
		return true;
	};
	$pkg.CanBackquote = CanBackquote;
	unhex = function(b) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, b, c, ok, v;
		v = 0;
		ok = false;
		c = (b >> 0);
		if (48 <= c && c <= 57) {
			_tmp = c - 48 >> 0;
			_tmp$1 = true;
			v = _tmp;
			ok = _tmp$1;
			return [v, ok];
		} else if (97 <= c && c <= 102) {
			_tmp$2 = (c - 97 >> 0) + 10 >> 0;
			_tmp$3 = true;
			v = _tmp$2;
			ok = _tmp$3;
			return [v, ok];
		} else if (65 <= c && c <= 70) {
			_tmp$4 = (c - 65 >> 0) + 10 >> 0;
			_tmp$5 = true;
			v = _tmp$4;
			ok = _tmp$5;
			return [v, ok];
		}
		return [v, ok];
	};
	UnquoteChar = function(s, quote) {
		var $ptr, _2, _3, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tuple, _tuple$1, c, c$1, err, j, j$1, multibyte, n, ok, quote, r, s, size, tail, v, v$1, value, x, x$1;
		value = 0;
		multibyte = false;
		tail = "";
		err = $ifaceNil;
		c = s.charCodeAt(0);
		if ((c === quote) && ((quote === 39) || (quote === 34))) {
			err = $pkg.ErrSyntax;
			return [value, multibyte, tail, err];
		} else if (c >= 128) {
			_tuple = utf8.DecodeRuneInString(s);
			r = _tuple[0];
			size = _tuple[1];
			_tmp = r;
			_tmp$1 = true;
			_tmp$2 = s.substring(size);
			_tmp$3 = $ifaceNil;
			value = _tmp;
			multibyte = _tmp$1;
			tail = _tmp$2;
			err = _tmp$3;
			return [value, multibyte, tail, err];
		} else if (!((c === 92))) {
			_tmp$4 = (s.charCodeAt(0) >> 0);
			_tmp$5 = false;
			_tmp$6 = s.substring(1);
			_tmp$7 = $ifaceNil;
			value = _tmp$4;
			multibyte = _tmp$5;
			tail = _tmp$6;
			err = _tmp$7;
			return [value, multibyte, tail, err];
		}
		if (s.length <= 1) {
			err = $pkg.ErrSyntax;
			return [value, multibyte, tail, err];
		}
		c$1 = s.charCodeAt(1);
		s = s.substring(2);
		switch (0) { default:
			_2 = c$1;
			if (_2 === (97)) {
				value = 7;
			} else if (_2 === (98)) {
				value = 8;
			} else if (_2 === (102)) {
				value = 12;
			} else if (_2 === (110)) {
				value = 10;
			} else if (_2 === (114)) {
				value = 13;
			} else if (_2 === (116)) {
				value = 9;
			} else if (_2 === (118)) {
				value = 11;
			} else if ((_2 === (120)) || (_2 === (117)) || (_2 === (85))) {
				n = 0;
				_3 = c$1;
				if (_3 === (120)) {
					n = 2;
				} else if (_3 === (117)) {
					n = 4;
				} else if (_3 === (85)) {
					n = 8;
				}
				v = 0;
				if (s.length < n) {
					err = $pkg.ErrSyntax;
					return [value, multibyte, tail, err];
				}
				j = 0;
				while (true) {
					if (!(j < n)) { break; }
					_tuple$1 = unhex(s.charCodeAt(j));
					x = _tuple$1[0];
					ok = _tuple$1[1];
					if (!ok) {
						err = $pkg.ErrSyntax;
						return [value, multibyte, tail, err];
					}
					v = (v << 4 >> 0) | x;
					j = j + (1) >> 0;
				}
				s = s.substring(n);
				if (c$1 === 120) {
					value = v;
					break;
				}
				if (v > 1114111) {
					err = $pkg.ErrSyntax;
					return [value, multibyte, tail, err];
				}
				value = v;
				multibyte = true;
			} else if ((_2 === (48)) || (_2 === (49)) || (_2 === (50)) || (_2 === (51)) || (_2 === (52)) || (_2 === (53)) || (_2 === (54)) || (_2 === (55))) {
				v$1 = (c$1 >> 0) - 48 >> 0;
				if (s.length < 2) {
					err = $pkg.ErrSyntax;
					return [value, multibyte, tail, err];
				}
				j$1 = 0;
				while (true) {
					if (!(j$1 < 2)) { break; }
					x$1 = (s.charCodeAt(j$1) >> 0) - 48 >> 0;
					if (x$1 < 0 || x$1 > 7) {
						err = $pkg.ErrSyntax;
						return [value, multibyte, tail, err];
					}
					v$1 = ((v$1 << 3 >> 0)) | x$1;
					j$1 = j$1 + (1) >> 0;
				}
				s = s.substring(2);
				if (v$1 > 255) {
					err = $pkg.ErrSyntax;
					return [value, multibyte, tail, err];
				}
				value = v$1;
			} else if (_2 === (92)) {
				value = 92;
			} else if ((_2 === (39)) || (_2 === (34))) {
				if (!((c$1 === quote))) {
					err = $pkg.ErrSyntax;
					return [value, multibyte, tail, err];
				}
				value = (c$1 >> 0);
			} else {
				err = $pkg.ErrSyntax;
				return [value, multibyte, tail, err];
			}
		}
		tail = s;
		return [value, multibyte, tail, err];
	};
	$pkg.UnquoteChar = UnquoteChar;
	Unquote = function(s) {
		var $ptr, _4, _q, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$12, _tmp$13, _tmp$14, _tmp$15, _tmp$16, _tmp$17, _tmp$18, _tmp$19, _tmp$2, _tmp$20, _tmp$21, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, _tuple, _tuple$1, buf, c, err, err$1, multibyte, n, n$1, quote, r, runeTmp, s, size, ss, t;
		t = "";
		err = $ifaceNil;
		n = s.length;
		if (n < 2) {
			_tmp = "";
			_tmp$1 = $pkg.ErrSyntax;
			t = _tmp;
			err = _tmp$1;
			return [t, err];
		}
		quote = s.charCodeAt(0);
		if (!((quote === s.charCodeAt((n - 1 >> 0))))) {
			_tmp$2 = "";
			_tmp$3 = $pkg.ErrSyntax;
			t = _tmp$2;
			err = _tmp$3;
			return [t, err];
		}
		s = s.substring(1, (n - 1 >> 0));
		if (quote === 96) {
			if (contains(s, 96)) {
				_tmp$4 = "";
				_tmp$5 = $pkg.ErrSyntax;
				t = _tmp$4;
				err = _tmp$5;
				return [t, err];
			}
			_tmp$6 = s;
			_tmp$7 = $ifaceNil;
			t = _tmp$6;
			err = _tmp$7;
			return [t, err];
		}
		if (!((quote === 34)) && !((quote === 39))) {
			_tmp$8 = "";
			_tmp$9 = $pkg.ErrSyntax;
			t = _tmp$8;
			err = _tmp$9;
			return [t, err];
		}
		if (contains(s, 10)) {
			_tmp$10 = "";
			_tmp$11 = $pkg.ErrSyntax;
			t = _tmp$10;
			err = _tmp$11;
			return [t, err];
		}
		if (!contains(s, 92) && !contains(s, quote)) {
			_4 = quote;
			if (_4 === (34)) {
				_tmp$12 = s;
				_tmp$13 = $ifaceNil;
				t = _tmp$12;
				err = _tmp$13;
				return [t, err];
			} else if (_4 === (39)) {
				_tuple = utf8.DecodeRuneInString(s);
				r = _tuple[0];
				size = _tuple[1];
				if ((size === s.length) && (!((r === 65533)) || !((size === 1)))) {
					_tmp$14 = s;
					_tmp$15 = $ifaceNil;
					t = _tmp$14;
					err = _tmp$15;
					return [t, err];
				}
			}
		}
		runeTmp = arrayType$4.zero();
		buf = $makeSlice(sliceType$6, 0, (_q = ($imul(3, s.length)) / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")));
		while (true) {
			if (!(s.length > 0)) { break; }
			_tuple$1 = UnquoteChar(s, quote);
			c = _tuple$1[0];
			multibyte = _tuple$1[1];
			ss = _tuple$1[2];
			err$1 = _tuple$1[3];
			if (!($interfaceIsEqual(err$1, $ifaceNil))) {
				_tmp$16 = "";
				_tmp$17 = err$1;
				t = _tmp$16;
				err = _tmp$17;
				return [t, err];
			}
			s = ss;
			if (c < 128 || !multibyte) {
				buf = $append(buf, (c << 24 >>> 24));
			} else {
				n$1 = utf8.EncodeRune(new sliceType$6(runeTmp), c);
				buf = $appendSlice(buf, $subslice(new sliceType$6(runeTmp), 0, n$1));
			}
			if ((quote === 39) && !((s.length === 0))) {
				_tmp$18 = "";
				_tmp$19 = $pkg.ErrSyntax;
				t = _tmp$18;
				err = _tmp$19;
				return [t, err];
			}
		}
		_tmp$20 = $bytesToString(buf);
		_tmp$21 = $ifaceNil;
		t = _tmp$20;
		err = _tmp$21;
		return [t, err];
	};
	$pkg.Unquote = Unquote;
	contains = function(s, c) {
		var $ptr, c, i, s;
		i = 0;
		while (true) {
			if (!(i < s.length)) { break; }
			if (s.charCodeAt(i) === c) {
				return true;
			}
			i = i + (1) >> 0;
		}
		return false;
	};
	bsearch16 = function(a, x) {
		var $ptr, _q, _tmp, _tmp$1, a, h, i, j, x;
		_tmp = 0;
		_tmp$1 = a.$length;
		i = _tmp;
		j = _tmp$1;
		while (true) {
			if (!(i < j)) { break; }
			h = i + (_q = ((j - i >> 0)) / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) >> 0;
			if (((h < 0 || h >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + h]) < x) {
				i = h + 1 >> 0;
			} else {
				j = h;
			}
		}
		return i;
	};
	bsearch32 = function(a, x) {
		var $ptr, _q, _tmp, _tmp$1, a, h, i, j, x;
		_tmp = 0;
		_tmp$1 = a.$length;
		i = _tmp;
		j = _tmp$1;
		while (true) {
			if (!(i < j)) { break; }
			h = i + (_q = ((j - i >> 0)) / 2, (_q === _q && _q !== 1/0 && _q !== -1/0) ? _q >> 0 : $throwRuntimeError("integer divide by zero")) >> 0;
			if (((h < 0 || h >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + h]) < x) {
				i = h + 1 >> 0;
			} else {
				j = h;
			}
		}
		return i;
	};
	IsPrint = function(r) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, i, i$1, isNotPrint, isNotPrint$1, isPrint, isPrint$1, j, j$1, r, rr, rr$1, x, x$1, x$2, x$3;
		if (r <= 255) {
			if (32 <= r && r <= 126) {
				return true;
			}
			if (161 <= r && r <= 255) {
				return !((r === 173));
			}
			return false;
		}
		if (0 <= r && r < 65536) {
			_tmp = (r << 16 >>> 16);
			_tmp$1 = isPrint16;
			_tmp$2 = isNotPrint16;
			rr = _tmp;
			isPrint = _tmp$1;
			isNotPrint = _tmp$2;
			i = bsearch16(isPrint, rr);
			if (i >= isPrint.$length || rr < (x = (i & ~1) >> 0, ((x < 0 || x >= isPrint.$length) ? $throwRuntimeError("index out of range") : isPrint.$array[isPrint.$offset + x])) || (x$1 = i | 1, ((x$1 < 0 || x$1 >= isPrint.$length) ? $throwRuntimeError("index out of range") : isPrint.$array[isPrint.$offset + x$1])) < rr) {
				return false;
			}
			j = bsearch16(isNotPrint, rr);
			return j >= isNotPrint.$length || !((((j < 0 || j >= isNotPrint.$length) ? $throwRuntimeError("index out of range") : isNotPrint.$array[isNotPrint.$offset + j]) === rr));
		}
		_tmp$3 = (r >>> 0);
		_tmp$4 = isPrint32;
		_tmp$5 = isNotPrint32;
		rr$1 = _tmp$3;
		isPrint$1 = _tmp$4;
		isNotPrint$1 = _tmp$5;
		i$1 = bsearch32(isPrint$1, rr$1);
		if (i$1 >= isPrint$1.$length || rr$1 < (x$2 = (i$1 & ~1) >> 0, ((x$2 < 0 || x$2 >= isPrint$1.$length) ? $throwRuntimeError("index out of range") : isPrint$1.$array[isPrint$1.$offset + x$2])) || (x$3 = i$1 | 1, ((x$3 < 0 || x$3 >= isPrint$1.$length) ? $throwRuntimeError("index out of range") : isPrint$1.$array[isPrint$1.$offset + x$3])) < rr$1) {
			return false;
		}
		if (r >= 131072) {
			return true;
		}
		r = r - (65536) >> 0;
		j$1 = bsearch16(isNotPrint$1, (r << 16 >>> 16));
		return j$1 >= isNotPrint$1.$length || !((((j$1 < 0 || j$1 >= isNotPrint$1.$length) ? $throwRuntimeError("index out of range") : isNotPrint$1.$array[isNotPrint$1.$offset + j$1]) === (r << 16 >>> 16)));
	};
	$pkg.IsPrint = IsPrint;
	isInGraphicList = function(r) {
		var $ptr, i, r, rr;
		if (r > 65535) {
			return false;
		}
		rr = (r << 16 >>> 16);
		i = bsearch16(isGraphic, rr);
		return i < isGraphic.$length && (rr === ((i < 0 || i >= isGraphic.$length) ? $throwRuntimeError("index out of range") : isGraphic.$array[isGraphic.$offset + i]));
	};
	ptrType$2.methods = [{prop: "set", name: "set", pkg: "strconv", typ: $funcType([$String], [$Bool], false)}, {prop: "floatBits", name: "floatBits", pkg: "strconv", typ: $funcType([ptrType$1], [$Uint64, $Bool], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Assign", name: "Assign", pkg: "", typ: $funcType([$Uint64], [], false)}, {prop: "Shift", name: "Shift", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "Round", name: "Round", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "RoundDown", name: "RoundDown", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "RoundUp", name: "RoundUp", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "RoundedInteger", name: "RoundedInteger", pkg: "", typ: $funcType([], [$Uint64], false)}];
	ptrType$4.methods = [{prop: "floatBits", name: "floatBits", pkg: "strconv", typ: $funcType([ptrType$1], [$Uint64, $Bool], false)}, {prop: "AssignComputeBounds", name: "AssignComputeBounds", pkg: "", typ: $funcType([$Uint64, $Int, $Bool, ptrType$1], [extFloat, extFloat], false)}, {prop: "Normalize", name: "Normalize", pkg: "", typ: $funcType([], [$Uint], false)}, {prop: "Multiply", name: "Multiply", pkg: "", typ: $funcType([extFloat], [], false)}, {prop: "AssignDecimal", name: "AssignDecimal", pkg: "", typ: $funcType([$Uint64, $Int, $Bool, $Bool, ptrType$1], [$Bool], false)}, {prop: "frexp10", name: "frexp10", pkg: "strconv", typ: $funcType([], [$Int, $Int], false)}, {prop: "FixedDecimal", name: "FixedDecimal", pkg: "", typ: $funcType([ptrType$3, $Int], [$Bool], false)}, {prop: "ShortestDecimal", name: "ShortestDecimal", pkg: "", typ: $funcType([ptrType$3, ptrType$4, ptrType$4], [$Bool], false)}];
	decimal.init([{prop: "d", name: "d", pkg: "strconv", typ: arrayType, tag: ""}, {prop: "nd", name: "nd", pkg: "strconv", typ: $Int, tag: ""}, {prop: "dp", name: "dp", pkg: "strconv", typ: $Int, tag: ""}, {prop: "neg", name: "neg", pkg: "strconv", typ: $Bool, tag: ""}, {prop: "trunc", name: "trunc", pkg: "strconv", typ: $Bool, tag: ""}]);
	leftCheat.init([{prop: "delta", name: "delta", pkg: "strconv", typ: $Int, tag: ""}, {prop: "cutoff", name: "cutoff", pkg: "strconv", typ: $String, tag: ""}]);
	extFloat.init([{prop: "mant", name: "mant", pkg: "strconv", typ: $Uint64, tag: ""}, {prop: "exp", name: "exp", pkg: "strconv", typ: $Int, tag: ""}, {prop: "neg", name: "neg", pkg: "strconv", typ: $Bool, tag: ""}]);
	floatInfo.init([{prop: "mantbits", name: "mantbits", pkg: "strconv", typ: $Uint, tag: ""}, {prop: "expbits", name: "expbits", pkg: "strconv", typ: $Uint, tag: ""}, {prop: "bias", name: "bias", pkg: "strconv", typ: $Int, tag: ""}]);
	decimalSlice.init([{prop: "d", name: "d", pkg: "strconv", typ: sliceType$6, tag: ""}, {prop: "nd", name: "nd", pkg: "strconv", typ: $Int, tag: ""}, {prop: "dp", name: "dp", pkg: "strconv", typ: $Int, tag: ""}, {prop: "neg", name: "neg", pkg: "strconv", typ: $Bool, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = math.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = utf8.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		optimize = true;
		$pkg.ErrRange = errors.New("value out of range");
		$pkg.ErrSyntax = errors.New("invalid syntax");
		leftcheats = new sliceType$3([new leftCheat.ptr(0, ""), new leftCheat.ptr(1, "5"), new leftCheat.ptr(1, "25"), new leftCheat.ptr(1, "125"), new leftCheat.ptr(2, "625"), new leftCheat.ptr(2, "3125"), new leftCheat.ptr(2, "15625"), new leftCheat.ptr(3, "78125"), new leftCheat.ptr(3, "390625"), new leftCheat.ptr(3, "1953125"), new leftCheat.ptr(4, "9765625"), new leftCheat.ptr(4, "48828125"), new leftCheat.ptr(4, "244140625"), new leftCheat.ptr(4, "1220703125"), new leftCheat.ptr(5, "6103515625"), new leftCheat.ptr(5, "30517578125"), new leftCheat.ptr(5, "152587890625"), new leftCheat.ptr(6, "762939453125"), new leftCheat.ptr(6, "3814697265625"), new leftCheat.ptr(6, "19073486328125"), new leftCheat.ptr(7, "95367431640625"), new leftCheat.ptr(7, "476837158203125"), new leftCheat.ptr(7, "2384185791015625"), new leftCheat.ptr(7, "11920928955078125"), new leftCheat.ptr(8, "59604644775390625"), new leftCheat.ptr(8, "298023223876953125"), new leftCheat.ptr(8, "1490116119384765625"), new leftCheat.ptr(9, "7450580596923828125"), new leftCheat.ptr(9, "37252902984619140625"), new leftCheat.ptr(9, "186264514923095703125"), new leftCheat.ptr(10, "931322574615478515625"), new leftCheat.ptr(10, "4656612873077392578125"), new leftCheat.ptr(10, "23283064365386962890625"), new leftCheat.ptr(10, "116415321826934814453125"), new leftCheat.ptr(11, "582076609134674072265625"), new leftCheat.ptr(11, "2910383045673370361328125"), new leftCheat.ptr(11, "14551915228366851806640625"), new leftCheat.ptr(12, "72759576141834259033203125"), new leftCheat.ptr(12, "363797880709171295166015625"), new leftCheat.ptr(12, "1818989403545856475830078125"), new leftCheat.ptr(13, "9094947017729282379150390625"), new leftCheat.ptr(13, "45474735088646411895751953125"), new leftCheat.ptr(13, "227373675443232059478759765625"), new leftCheat.ptr(13, "1136868377216160297393798828125"), new leftCheat.ptr(14, "5684341886080801486968994140625"), new leftCheat.ptr(14, "28421709430404007434844970703125"), new leftCheat.ptr(14, "142108547152020037174224853515625"), new leftCheat.ptr(15, "710542735760100185871124267578125"), new leftCheat.ptr(15, "3552713678800500929355621337890625"), new leftCheat.ptr(15, "17763568394002504646778106689453125"), new leftCheat.ptr(16, "88817841970012523233890533447265625"), new leftCheat.ptr(16, "444089209850062616169452667236328125"), new leftCheat.ptr(16, "2220446049250313080847263336181640625"), new leftCheat.ptr(16, "11102230246251565404236316680908203125"), new leftCheat.ptr(17, "55511151231257827021181583404541015625"), new leftCheat.ptr(17, "277555756156289135105907917022705078125"), new leftCheat.ptr(17, "1387778780781445675529539585113525390625"), new leftCheat.ptr(18, "6938893903907228377647697925567626953125"), new leftCheat.ptr(18, "34694469519536141888238489627838134765625"), new leftCheat.ptr(18, "173472347597680709441192448139190673828125"), new leftCheat.ptr(19, "867361737988403547205962240695953369140625")]);
		smallPowersOfTen = $toNativeArray($kindStruct, [new extFloat.ptr(new $Uint64(2147483648, 0), -63, false), new extFloat.ptr(new $Uint64(2684354560, 0), -60, false), new extFloat.ptr(new $Uint64(3355443200, 0), -57, false), new extFloat.ptr(new $Uint64(4194304000, 0), -54, false), new extFloat.ptr(new $Uint64(2621440000, 0), -50, false), new extFloat.ptr(new $Uint64(3276800000, 0), -47, false), new extFloat.ptr(new $Uint64(4096000000, 0), -44, false), new extFloat.ptr(new $Uint64(2560000000, 0), -40, false)]);
		powersOfTen = $toNativeArray($kindStruct, [new extFloat.ptr(new $Uint64(4203730336, 136053384), -1220, false), new extFloat.ptr(new $Uint64(3132023167, 2722021238), -1193, false), new extFloat.ptr(new $Uint64(2333539104, 810921078), -1166, false), new extFloat.ptr(new $Uint64(3477244234, 1573795306), -1140, false), new extFloat.ptr(new $Uint64(2590748842, 1432697645), -1113, false), new extFloat.ptr(new $Uint64(3860516611, 1025131999), -1087, false), new extFloat.ptr(new $Uint64(2876309015, 3348809418), -1060, false), new extFloat.ptr(new $Uint64(4286034428, 3200048207), -1034, false), new extFloat.ptr(new $Uint64(3193344495, 1097586188), -1007, false), new extFloat.ptr(new $Uint64(2379227053, 2424306748), -980, false), new extFloat.ptr(new $Uint64(3545324584, 827693699), -954, false), new extFloat.ptr(new $Uint64(2641472655, 2913388981), -927, false), new extFloat.ptr(new $Uint64(3936100983, 602835915), -901, false), new extFloat.ptr(new $Uint64(2932623761, 1081627501), -874, false), new extFloat.ptr(new $Uint64(2184974969, 1572261463), -847, false), new extFloat.ptr(new $Uint64(3255866422, 1308317239), -821, false), new extFloat.ptr(new $Uint64(2425809519, 944281679), -794, false), new extFloat.ptr(new $Uint64(3614737867, 629291719), -768, false), new extFloat.ptr(new $Uint64(2693189581, 2545915892), -741, false), new extFloat.ptr(new $Uint64(4013165208, 388672741), -715, false), new extFloat.ptr(new $Uint64(2990041083, 708162190), -688, false), new extFloat.ptr(new $Uint64(2227754207, 3536207675), -661, false), new extFloat.ptr(new $Uint64(3319612455, 450088378), -635, false), new extFloat.ptr(new $Uint64(2473304014, 3139815830), -608, false), new extFloat.ptr(new $Uint64(3685510180, 2103616900), -582, false), new extFloat.ptr(new $Uint64(2745919064, 224385782), -555, false), new extFloat.ptr(new $Uint64(4091738259, 3737383206), -529, false), new extFloat.ptr(new $Uint64(3048582568, 2868871352), -502, false), new extFloat.ptr(new $Uint64(2271371013, 1820084875), -475, false), new extFloat.ptr(new $Uint64(3384606560, 885076051), -449, false), new extFloat.ptr(new $Uint64(2521728396, 2444895829), -422, false), new extFloat.ptr(new $Uint64(3757668132, 1881767613), -396, false), new extFloat.ptr(new $Uint64(2799680927, 3102062735), -369, false), new extFloat.ptr(new $Uint64(4171849679, 2289335700), -343, false), new extFloat.ptr(new $Uint64(3108270227, 2410191823), -316, false), new extFloat.ptr(new $Uint64(2315841784, 3205436779), -289, false), new extFloat.ptr(new $Uint64(3450873173, 1697722806), -263, false), new extFloat.ptr(new $Uint64(2571100870, 3497754540), -236, false), new extFloat.ptr(new $Uint64(3831238852, 707476230), -210, false), new extFloat.ptr(new $Uint64(2854495385, 1769181907), -183, false), new extFloat.ptr(new $Uint64(4253529586, 2197867022), -157, false), new extFloat.ptr(new $Uint64(3169126500, 2450594539), -130, false), new extFloat.ptr(new $Uint64(2361183241, 1867548876), -103, false), new extFloat.ptr(new $Uint64(3518437208, 3793315116), -77, false), new extFloat.ptr(new $Uint64(2621440000, 0), -50, false), new extFloat.ptr(new $Uint64(3906250000, 0), -24, false), new extFloat.ptr(new $Uint64(2910383045, 2892103680), 3, false), new extFloat.ptr(new $Uint64(2168404344, 4170451332), 30, false), new extFloat.ptr(new $Uint64(3231174267, 3372684723), 56, false), new extFloat.ptr(new $Uint64(2407412430, 2078956656), 83, false), new extFloat.ptr(new $Uint64(3587324068, 2884206696), 109, false), new extFloat.ptr(new $Uint64(2672764710, 395977285), 136, false), new extFloat.ptr(new $Uint64(3982729777, 3569679143), 162, false), new extFloat.ptr(new $Uint64(2967364920, 2361961896), 189, false), new extFloat.ptr(new $Uint64(2210859150, 447440347), 216, false), new extFloat.ptr(new $Uint64(3294436857, 1114709402), 242, false), new extFloat.ptr(new $Uint64(2454546732, 2786846552), 269, false), new extFloat.ptr(new $Uint64(3657559652, 443583978), 295, false), new extFloat.ptr(new $Uint64(2725094297, 2599384906), 322, false), new extFloat.ptr(new $Uint64(4060706939, 3028118405), 348, false), new extFloat.ptr(new $Uint64(3025462433, 2044532855), 375, false), new extFloat.ptr(new $Uint64(2254145170, 1536935362), 402, false), new extFloat.ptr(new $Uint64(3358938053, 3365297469), 428, false), new extFloat.ptr(new $Uint64(2502603868, 4204241075), 455, false), new extFloat.ptr(new $Uint64(3729170365, 2577424355), 481, false), new extFloat.ptr(new $Uint64(2778448436, 3677981733), 508, false), new extFloat.ptr(new $Uint64(4140210802, 2744688476), 534, false), new extFloat.ptr(new $Uint64(3084697427, 1424604878), 561, false), new extFloat.ptr(new $Uint64(2298278679, 4062331362), 588, false), new extFloat.ptr(new $Uint64(3424702107, 3546052773), 614, false), new extFloat.ptr(new $Uint64(2551601907, 2065781727), 641, false), new extFloat.ptr(new $Uint64(3802183132, 2535403578), 667, false), new extFloat.ptr(new $Uint64(2832847187, 1558426518), 694, false), new extFloat.ptr(new $Uint64(4221271257, 2762425404), 720, false), new extFloat.ptr(new $Uint64(3145092172, 2812560400), 747, false), new extFloat.ptr(new $Uint64(2343276271, 3057687578), 774, false), new extFloat.ptr(new $Uint64(3491753744, 2790753324), 800, false), new extFloat.ptr(new $Uint64(2601559269, 3918606633), 827, false), new extFloat.ptr(new $Uint64(3876625403, 2711358621), 853, false), new extFloat.ptr(new $Uint64(2888311001, 1648096297), 880, false), new extFloat.ptr(new $Uint64(2151959390, 2057817989), 907, false), new extFloat.ptr(new $Uint64(3206669376, 61660461), 933, false), new extFloat.ptr(new $Uint64(2389154863, 1581580175), 960, false), new extFloat.ptr(new $Uint64(3560118173, 2626467905), 986, false), new extFloat.ptr(new $Uint64(2652494738, 3034782633), 1013, false), new extFloat.ptr(new $Uint64(3952525166, 3135207385), 1039, false), new extFloat.ptr(new $Uint64(2944860731, 2616258155), 1066, false)]);
		uint64pow10 = $toNativeArray($kindUint64, [new $Uint64(0, 1), new $Uint64(0, 10), new $Uint64(0, 100), new $Uint64(0, 1000), new $Uint64(0, 10000), new $Uint64(0, 100000), new $Uint64(0, 1000000), new $Uint64(0, 10000000), new $Uint64(0, 100000000), new $Uint64(0, 1000000000), new $Uint64(2, 1410065408), new $Uint64(23, 1215752192), new $Uint64(232, 3567587328), new $Uint64(2328, 1316134912), new $Uint64(23283, 276447232), new $Uint64(232830, 2764472320), new $Uint64(2328306, 1874919424), new $Uint64(23283064, 1569325056), new $Uint64(232830643, 2808348672), new $Uint64(2328306436, 2313682944)]);
		float32info = new floatInfo.ptr(23, 8, -127);
		float64info = new floatInfo.ptr(52, 11, -1023);
		isPrint16 = new sliceType$4([32, 126, 161, 887, 890, 895, 900, 1366, 1369, 1418, 1421, 1479, 1488, 1514, 1520, 1524, 1542, 1563, 1566, 1805, 1808, 1866, 1869, 1969, 1984, 2042, 2048, 2093, 2096, 2139, 2142, 2142, 2208, 2228, 2275, 2444, 2447, 2448, 2451, 2482, 2486, 2489, 2492, 2500, 2503, 2504, 2507, 2510, 2519, 2519, 2524, 2531, 2534, 2555, 2561, 2570, 2575, 2576, 2579, 2617, 2620, 2626, 2631, 2632, 2635, 2637, 2641, 2641, 2649, 2654, 2662, 2677, 2689, 2745, 2748, 2765, 2768, 2768, 2784, 2787, 2790, 2801, 2809, 2809, 2817, 2828, 2831, 2832, 2835, 2873, 2876, 2884, 2887, 2888, 2891, 2893, 2902, 2903, 2908, 2915, 2918, 2935, 2946, 2954, 2958, 2965, 2969, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3006, 3010, 3014, 3021, 3024, 3024, 3031, 3031, 3046, 3066, 3072, 3129, 3133, 3149, 3157, 3162, 3168, 3171, 3174, 3183, 3192, 3257, 3260, 3277, 3285, 3286, 3294, 3299, 3302, 3314, 3329, 3386, 3389, 3406, 3415, 3415, 3423, 3427, 3430, 3445, 3449, 3455, 3458, 3478, 3482, 3517, 3520, 3526, 3530, 3530, 3535, 3551, 3558, 3567, 3570, 3572, 3585, 3642, 3647, 3675, 3713, 3716, 3719, 3722, 3725, 3725, 3732, 3751, 3754, 3773, 3776, 3789, 3792, 3801, 3804, 3807, 3840, 3948, 3953, 4058, 4096, 4295, 4301, 4301, 4304, 4685, 4688, 4701, 4704, 4749, 4752, 4789, 4792, 4805, 4808, 4885, 4888, 4954, 4957, 4988, 4992, 5017, 5024, 5109, 5112, 5117, 5120, 5788, 5792, 5880, 5888, 5908, 5920, 5942, 5952, 5971, 5984, 6003, 6016, 6109, 6112, 6121, 6128, 6137, 6144, 6157, 6160, 6169, 6176, 6263, 6272, 6314, 6320, 6389, 6400, 6443, 6448, 6459, 6464, 6464, 6468, 6509, 6512, 6516, 6528, 6571, 6576, 6601, 6608, 6618, 6622, 6683, 6686, 6780, 6783, 6793, 6800, 6809, 6816, 6829, 6832, 6846, 6912, 6987, 6992, 7036, 7040, 7155, 7164, 7223, 7227, 7241, 7245, 7295, 7360, 7367, 7376, 7417, 7424, 7669, 7676, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8061, 8064, 8147, 8150, 8175, 8178, 8190, 8208, 8231, 8240, 8286, 8304, 8305, 8308, 8348, 8352, 8382, 8400, 8432, 8448, 8587, 8592, 9210, 9216, 9254, 9280, 9290, 9312, 11123, 11126, 11157, 11160, 11193, 11197, 11217, 11244, 11247, 11264, 11507, 11513, 11559, 11565, 11565, 11568, 11623, 11631, 11632, 11647, 11670, 11680, 11842, 11904, 12019, 12032, 12245, 12272, 12283, 12289, 12438, 12441, 12543, 12549, 12589, 12593, 12730, 12736, 12771, 12784, 19893, 19904, 40917, 40960, 42124, 42128, 42182, 42192, 42539, 42560, 42743, 42752, 42925, 42928, 42935, 42999, 43051, 43056, 43065, 43072, 43127, 43136, 43204, 43214, 43225, 43232, 43261, 43264, 43347, 43359, 43388, 43392, 43481, 43486, 43574, 43584, 43597, 43600, 43609, 43612, 43714, 43739, 43766, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43877, 43888, 44013, 44016, 44025, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64449, 64467, 64831, 64848, 64911, 64914, 64967, 65008, 65021, 65024, 65049, 65056, 65131, 65136, 65276, 65281, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500, 65504, 65518, 65532, 65533]);
		isNotPrint16 = new sliceType$4([173, 907, 909, 930, 1328, 1376, 1416, 1424, 1757, 2111, 2436, 2473, 2481, 2526, 2564, 2601, 2609, 2612, 2615, 2621, 2653, 2692, 2702, 2706, 2729, 2737, 2740, 2758, 2762, 2820, 2857, 2865, 2868, 2910, 2948, 2961, 2971, 2973, 3017, 3076, 3085, 3089, 3113, 3141, 3145, 3159, 3200, 3204, 3213, 3217, 3241, 3252, 3269, 3273, 3295, 3312, 3332, 3341, 3345, 3397, 3401, 3460, 3506, 3516, 3541, 3543, 3715, 3721, 3736, 3744, 3748, 3750, 3756, 3770, 3781, 3783, 3912, 3992, 4029, 4045, 4294, 4681, 4695, 4697, 4745, 4785, 4799, 4801, 4823, 4881, 5760, 5901, 5997, 6001, 6431, 6751, 7415, 8024, 8026, 8028, 8030, 8117, 8133, 8156, 8181, 8335, 11209, 11311, 11359, 11558, 11687, 11695, 11703, 11711, 11719, 11727, 11735, 11743, 11930, 12352, 12687, 12831, 13055, 43470, 43519, 43815, 43823, 64311, 64317, 64319, 64322, 64325, 65107, 65127, 65141, 65511]);
		isPrint32 = new sliceType$5([65536, 65613, 65616, 65629, 65664, 65786, 65792, 65794, 65799, 65843, 65847, 65932, 65936, 65947, 65952, 65952, 66000, 66045, 66176, 66204, 66208, 66256, 66272, 66299, 66304, 66339, 66352, 66378, 66384, 66426, 66432, 66499, 66504, 66517, 66560, 66717, 66720, 66729, 66816, 66855, 66864, 66915, 66927, 66927, 67072, 67382, 67392, 67413, 67424, 67431, 67584, 67589, 67592, 67640, 67644, 67644, 67647, 67742, 67751, 67759, 67808, 67829, 67835, 67867, 67871, 67897, 67903, 67903, 67968, 68023, 68028, 68047, 68050, 68102, 68108, 68147, 68152, 68154, 68159, 68167, 68176, 68184, 68192, 68255, 68288, 68326, 68331, 68342, 68352, 68405, 68409, 68437, 68440, 68466, 68472, 68497, 68505, 68508, 68521, 68527, 68608, 68680, 68736, 68786, 68800, 68850, 68858, 68863, 69216, 69246, 69632, 69709, 69714, 69743, 69759, 69825, 69840, 69864, 69872, 69881, 69888, 69955, 69968, 70006, 70016, 70093, 70096, 70132, 70144, 70205, 70272, 70313, 70320, 70378, 70384, 70393, 70400, 70412, 70415, 70416, 70419, 70457, 70460, 70468, 70471, 70472, 70475, 70477, 70480, 70480, 70487, 70487, 70493, 70499, 70502, 70508, 70512, 70516, 70784, 70855, 70864, 70873, 71040, 71093, 71096, 71133, 71168, 71236, 71248, 71257, 71296, 71351, 71360, 71369, 71424, 71449, 71453, 71467, 71472, 71487, 71840, 71922, 71935, 71935, 72384, 72440, 73728, 74649, 74752, 74868, 74880, 75075, 77824, 78894, 82944, 83526, 92160, 92728, 92736, 92777, 92782, 92783, 92880, 92909, 92912, 92917, 92928, 92997, 93008, 93047, 93053, 93071, 93952, 94020, 94032, 94078, 94095, 94111, 110592, 110593, 113664, 113770, 113776, 113788, 113792, 113800, 113808, 113817, 113820, 113823, 118784, 119029, 119040, 119078, 119081, 119154, 119163, 119272, 119296, 119365, 119552, 119638, 119648, 119665, 119808, 119967, 119970, 119970, 119973, 119974, 119977, 120074, 120077, 120134, 120138, 120485, 120488, 120779, 120782, 121483, 121499, 121519, 124928, 125124, 125127, 125142, 126464, 126500, 126503, 126523, 126530, 126530, 126535, 126548, 126551, 126564, 126567, 126619, 126625, 126651, 126704, 126705, 126976, 127019, 127024, 127123, 127136, 127150, 127153, 127221, 127232, 127244, 127248, 127339, 127344, 127386, 127462, 127490, 127504, 127546, 127552, 127560, 127568, 127569, 127744, 128720, 128736, 128748, 128752, 128755, 128768, 128883, 128896, 128980, 129024, 129035, 129040, 129095, 129104, 129113, 129120, 129159, 129168, 129197, 129296, 129304, 129408, 129412, 129472, 129472, 131072, 173782, 173824, 177972, 177984, 178205, 178208, 183969, 194560, 195101, 917760, 917999]);
		isNotPrint32 = new sliceType$4([12, 39, 59, 62, 926, 2057, 2102, 2134, 2291, 2564, 2580, 2584, 4285, 4405, 4576, 4626, 4743, 4745, 4750, 4766, 4868, 4905, 4913, 4916, 9327, 27231, 27482, 27490, 54357, 54429, 54445, 54458, 54460, 54468, 54534, 54549, 54557, 54586, 54591, 54597, 54609, 55968, 60932, 60960, 60963, 60968, 60979, 60984, 60986, 61000, 61002, 61004, 61008, 61011, 61016, 61018, 61020, 61022, 61024, 61027, 61035, 61043, 61048, 61053, 61055, 61066, 61092, 61098, 61632, 61648, 61743, 62842, 62884]);
		isGraphic = new sliceType$4([160, 5760, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201, 8202, 8239, 8287, 12288]);
		shifts = $toNativeArray($kindUint, [0, 0, 1, 0, 2, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0]);
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["reflect"] = (function() {
	var $pkg = {}, $init, errors, js, math, runtime, strconv, sync, mapIter, Type, Kind, rtype, typeAlg, method, uncommonType, ChanDir, arrayType, chanType, funcType, imethod, interfaceType, mapType, ptrType, sliceType, structField, structType, Method, StructField, StructTag, fieldScan, Value, flag, ValueError, nonEmptyInterface, ptrType$1, sliceType$1, sliceType$2, ptrType$3, funcType$1, sliceType$3, ptrType$4, ptrType$5, ptrType$6, ptrType$7, sliceType$4, sliceType$5, sliceType$6, structType$6, sliceType$7, sliceType$8, ptrType$8, arrayType$1, structType$7, ptrType$9, sliceType$9, ptrType$10, ptrType$11, ptrType$12, sliceType$11, sliceType$12, ptrType$13, sliceType$13, ptrType$18, sliceType$15, funcType$3, funcType$4, funcType$5, arrayType$3, ptrType$20, initialized, stringPtrMap, callHelper, jsObjectPtr, selectHelper, kindNames, uint8Type, init, jsType, reflectType, setKindType, newStringPtr, isWrapped, copyStruct, makeValue, MakeSlice, TypeOf, ValueOf, SliceOf, Zero, unsafe_New, makeInt, typedmemmove, keyFor, mapaccess, mapassign, mapdelete, mapiterinit, mapiterkey, mapiternext, maplen, cvtDirect, methodReceiver, valueInterface, ifaceE2I, methodName, makeMethodValue, wrapJsObject, unwrapJsObject, getJsTag, chanrecv, chansend, PtrTo, implements$1, directlyAssignable, haveIdenticalUnderlyingType, toType, ifaceIndir, overflowFloat32, New, convertOp, makeFloat, makeComplex, makeString, makeBytes, makeRunes, cvtInt, cvtUint, cvtFloatInt, cvtFloatUint, cvtIntFloat, cvtUintFloat, cvtFloat, cvtComplex, cvtIntString, cvtUintString, cvtBytesString, cvtStringBytes, cvtRunesString, cvtStringRunes, cvtT2I, cvtI2I;
	errors = $packages["errors"];
	js = $packages["github.com/gopherjs/gopherjs/js"];
	math = $packages["math"];
	runtime = $packages["runtime"];
	strconv = $packages["strconv"];
	sync = $packages["sync"];
	mapIter = $pkg.mapIter = $newType(0, $kindStruct, "reflect.mapIter", "mapIter", "reflect", function(t_, m_, keys_, i_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.t = $ifaceNil;
			this.m = null;
			this.keys = null;
			this.i = 0;
			return;
		}
		this.t = t_;
		this.m = m_;
		this.keys = keys_;
		this.i = i_;
	});
	Type = $pkg.Type = $newType(8, $kindInterface, "reflect.Type", "Type", "reflect", null);
	Kind = $pkg.Kind = $newType(4, $kindUint, "reflect.Kind", "Kind", "reflect", null);
	rtype = $pkg.rtype = $newType(0, $kindStruct, "reflect.rtype", "rtype", "reflect", function(size_, ptrdata_, hash_, _$3_, align_, fieldAlign_, kind_, alg_, gcdata_, string_, uncommonType_, ptrToThis_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.size = 0;
			this.ptrdata = 0;
			this.hash = 0;
			this._$3 = 0;
			this.align = 0;
			this.fieldAlign = 0;
			this.kind = 0;
			this.alg = ptrType$4.nil;
			this.gcdata = ptrType$5.nil;
			this.string = ptrType$6.nil;
			this.uncommonType = ptrType$7.nil;
			this.ptrToThis = ptrType$1.nil;
			return;
		}
		this.size = size_;
		this.ptrdata = ptrdata_;
		this.hash = hash_;
		this._$3 = _$3_;
		this.align = align_;
		this.fieldAlign = fieldAlign_;
		this.kind = kind_;
		this.alg = alg_;
		this.gcdata = gcdata_;
		this.string = string_;
		this.uncommonType = uncommonType_;
		this.ptrToThis = ptrToThis_;
	});
	typeAlg = $pkg.typeAlg = $newType(0, $kindStruct, "reflect.typeAlg", "typeAlg", "reflect", function(hash_, equal_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.hash = $throwNilPointerError;
			this.equal = $throwNilPointerError;
			return;
		}
		this.hash = hash_;
		this.equal = equal_;
	});
	method = $pkg.method = $newType(0, $kindStruct, "reflect.method", "method", "reflect", function(name_, pkgPath_, mtyp_, typ_, ifn_, tfn_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = ptrType$6.nil;
			this.pkgPath = ptrType$6.nil;
			this.mtyp = ptrType$1.nil;
			this.typ = ptrType$1.nil;
			this.ifn = 0;
			this.tfn = 0;
			return;
		}
		this.name = name_;
		this.pkgPath = pkgPath_;
		this.mtyp = mtyp_;
		this.typ = typ_;
		this.ifn = ifn_;
		this.tfn = tfn_;
	});
	uncommonType = $pkg.uncommonType = $newType(0, $kindStruct, "reflect.uncommonType", "uncommonType", "reflect", function(name_, pkgPath_, methods_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = ptrType$6.nil;
			this.pkgPath = ptrType$6.nil;
			this.methods = sliceType$4.nil;
			return;
		}
		this.name = name_;
		this.pkgPath = pkgPath_;
		this.methods = methods_;
	});
	ChanDir = $pkg.ChanDir = $newType(4, $kindInt, "reflect.ChanDir", "ChanDir", "reflect", null);
	arrayType = $pkg.arrayType = $newType(0, $kindStruct, "reflect.arrayType", "arrayType", "reflect", function(rtype_, elem_, slice_, len_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.elem = ptrType$1.nil;
			this.slice = ptrType$1.nil;
			this.len = 0;
			return;
		}
		this.rtype = rtype_;
		this.elem = elem_;
		this.slice = slice_;
		this.len = len_;
	});
	chanType = $pkg.chanType = $newType(0, $kindStruct, "reflect.chanType", "chanType", "reflect", function(rtype_, elem_, dir_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.elem = ptrType$1.nil;
			this.dir = 0;
			return;
		}
		this.rtype = rtype_;
		this.elem = elem_;
		this.dir = dir_;
	});
	funcType = $pkg.funcType = $newType(0, $kindStruct, "reflect.funcType", "funcType", "reflect", function(rtype_, dotdotdot_, in$2_, out_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.dotdotdot = false;
			this.in$2 = sliceType$1.nil;
			this.out = sliceType$1.nil;
			return;
		}
		this.rtype = rtype_;
		this.dotdotdot = dotdotdot_;
		this.in$2 = in$2_;
		this.out = out_;
	});
	imethod = $pkg.imethod = $newType(0, $kindStruct, "reflect.imethod", "imethod", "reflect", function(name_, pkgPath_, typ_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = ptrType$6.nil;
			this.pkgPath = ptrType$6.nil;
			this.typ = ptrType$1.nil;
			return;
		}
		this.name = name_;
		this.pkgPath = pkgPath_;
		this.typ = typ_;
	});
	interfaceType = $pkg.interfaceType = $newType(0, $kindStruct, "reflect.interfaceType", "interfaceType", "reflect", function(rtype_, methods_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.methods = sliceType$5.nil;
			return;
		}
		this.rtype = rtype_;
		this.methods = methods_;
	});
	mapType = $pkg.mapType = $newType(0, $kindStruct, "reflect.mapType", "mapType", "reflect", function(rtype_, key_, elem_, bucket_, hmap_, keysize_, indirectkey_, valuesize_, indirectvalue_, bucketsize_, reflexivekey_, needkeyupdate_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.key = ptrType$1.nil;
			this.elem = ptrType$1.nil;
			this.bucket = ptrType$1.nil;
			this.hmap = ptrType$1.nil;
			this.keysize = 0;
			this.indirectkey = 0;
			this.valuesize = 0;
			this.indirectvalue = 0;
			this.bucketsize = 0;
			this.reflexivekey = false;
			this.needkeyupdate = false;
			return;
		}
		this.rtype = rtype_;
		this.key = key_;
		this.elem = elem_;
		this.bucket = bucket_;
		this.hmap = hmap_;
		this.keysize = keysize_;
		this.indirectkey = indirectkey_;
		this.valuesize = valuesize_;
		this.indirectvalue = indirectvalue_;
		this.bucketsize = bucketsize_;
		this.reflexivekey = reflexivekey_;
		this.needkeyupdate = needkeyupdate_;
	});
	ptrType = $pkg.ptrType = $newType(0, $kindStruct, "reflect.ptrType", "ptrType", "reflect", function(rtype_, elem_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.elem = ptrType$1.nil;
			return;
		}
		this.rtype = rtype_;
		this.elem = elem_;
	});
	sliceType = $pkg.sliceType = $newType(0, $kindStruct, "reflect.sliceType", "sliceType", "reflect", function(rtype_, elem_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.elem = ptrType$1.nil;
			return;
		}
		this.rtype = rtype_;
		this.elem = elem_;
	});
	structField = $pkg.structField = $newType(0, $kindStruct, "reflect.structField", "structField", "reflect", function(name_, pkgPath_, typ_, tag_, offset_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.name = ptrType$6.nil;
			this.pkgPath = ptrType$6.nil;
			this.typ = ptrType$1.nil;
			this.tag = ptrType$6.nil;
			this.offset = 0;
			return;
		}
		this.name = name_;
		this.pkgPath = pkgPath_;
		this.typ = typ_;
		this.tag = tag_;
		this.offset = offset_;
	});
	structType = $pkg.structType = $newType(0, $kindStruct, "reflect.structType", "structType", "reflect", function(rtype_, fields_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rtype = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
			this.fields = sliceType$6.nil;
			return;
		}
		this.rtype = rtype_;
		this.fields = fields_;
	});
	Method = $pkg.Method = $newType(0, $kindStruct, "reflect.Method", "Method", "reflect", function(Name_, PkgPath_, Type_, Func_, Index_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Name = "";
			this.PkgPath = "";
			this.Type = $ifaceNil;
			this.Func = new Value.ptr(ptrType$1.nil, 0, 0);
			this.Index = 0;
			return;
		}
		this.Name = Name_;
		this.PkgPath = PkgPath_;
		this.Type = Type_;
		this.Func = Func_;
		this.Index = Index_;
	});
	StructField = $pkg.StructField = $newType(0, $kindStruct, "reflect.StructField", "StructField", "reflect", function(Name_, PkgPath_, Type_, Tag_, Offset_, Index_, Anonymous_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Name = "";
			this.PkgPath = "";
			this.Type = $ifaceNil;
			this.Tag = "";
			this.Offset = 0;
			this.Index = sliceType$11.nil;
			this.Anonymous = false;
			return;
		}
		this.Name = Name_;
		this.PkgPath = PkgPath_;
		this.Type = Type_;
		this.Tag = Tag_;
		this.Offset = Offset_;
		this.Index = Index_;
		this.Anonymous = Anonymous_;
	});
	StructTag = $pkg.StructTag = $newType(8, $kindString, "reflect.StructTag", "StructTag", "reflect", null);
	fieldScan = $pkg.fieldScan = $newType(0, $kindStruct, "reflect.fieldScan", "fieldScan", "reflect", function(typ_, index_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.typ = ptrType$13.nil;
			this.index = sliceType$11.nil;
			return;
		}
		this.typ = typ_;
		this.index = index_;
	});
	Value = $pkg.Value = $newType(0, $kindStruct, "reflect.Value", "Value", "reflect", function(typ_, ptr_, flag_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.typ = ptrType$1.nil;
			this.ptr = 0;
			this.flag = 0;
			return;
		}
		this.typ = typ_;
		this.ptr = ptr_;
		this.flag = flag_;
	});
	flag = $pkg.flag = $newType(4, $kindUintptr, "reflect.flag", "flag", "reflect", null);
	ValueError = $pkg.ValueError = $newType(0, $kindStruct, "reflect.ValueError", "ValueError", "reflect", function(Method_, Kind_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Method = "";
			this.Kind = 0;
			return;
		}
		this.Method = Method_;
		this.Kind = Kind_;
	});
	nonEmptyInterface = $pkg.nonEmptyInterface = $newType(0, $kindStruct, "reflect.nonEmptyInterface", "nonEmptyInterface", "reflect", function(itab_, word_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.itab = ptrType$9.nil;
			this.word = 0;
			return;
		}
		this.itab = itab_;
		this.word = word_;
	});
	ptrType$1 = $ptrType(rtype);
	sliceType$1 = $sliceType(ptrType$1);
	sliceType$2 = $sliceType($emptyInterface);
	ptrType$3 = $ptrType(js.Object);
	funcType$1 = $funcType([sliceType$2], [ptrType$3], true);
	sliceType$3 = $sliceType($String);
	ptrType$4 = $ptrType(typeAlg);
	ptrType$5 = $ptrType($Uint8);
	ptrType$6 = $ptrType($String);
	ptrType$7 = $ptrType(uncommonType);
	sliceType$4 = $sliceType(method);
	sliceType$5 = $sliceType(imethod);
	sliceType$6 = $sliceType(structField);
	structType$6 = $structType([{prop: "str", name: "str", pkg: "reflect", typ: $String, tag: ""}]);
	sliceType$7 = $sliceType(ptrType$3);
	sliceType$8 = $sliceType(Value);
	ptrType$8 = $ptrType(nonEmptyInterface);
	arrayType$1 = $arrayType($UnsafePointer, 100000);
	structType$7 = $structType([{prop: "ityp", name: "ityp", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "typ", name: "typ", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "link", name: "link", pkg: "reflect", typ: $UnsafePointer, tag: ""}, {prop: "bad", name: "bad", pkg: "reflect", typ: $Int32, tag: ""}, {prop: "unused", name: "unused", pkg: "reflect", typ: $Int32, tag: ""}, {prop: "fun", name: "fun", pkg: "reflect", typ: arrayType$1, tag: ""}]);
	ptrType$9 = $ptrType(structType$7);
	sliceType$9 = $sliceType(sliceType$7);
	ptrType$10 = $ptrType(method);
	ptrType$11 = $ptrType(interfaceType);
	ptrType$12 = $ptrType(imethod);
	sliceType$11 = $sliceType($Int);
	sliceType$12 = $sliceType(fieldScan);
	ptrType$13 = $ptrType(structType);
	sliceType$13 = $sliceType($Uint8);
	ptrType$18 = $ptrType($UnsafePointer);
	sliceType$15 = $sliceType($Int32);
	funcType$3 = $funcType([$String], [$Bool], false);
	funcType$4 = $funcType([$UnsafePointer, $Uintptr], [$Uintptr], false);
	funcType$5 = $funcType([$UnsafePointer, $UnsafePointer], [$Bool], false);
	arrayType$3 = $arrayType($Uintptr, 2);
	ptrType$20 = $ptrType(ValueError);
	init = function() {
		var $ptr, used, x, x$1, x$10, x$11, x$12, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; used = $f.used; x = $f.x; x$1 = $f.x$1; x$10 = $f.x$10; x$11 = $f.x$11; x$12 = $f.x$12; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; x$6 = $f.x$6; x$7 = $f.x$7; x$8 = $f.x$8; x$9 = $f.x$9; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		used = (function(i) {
			var $ptr, i;
		});
		$r = used((x = new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), new x.constructor.elem(x))); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$1 = new uncommonType.ptr(ptrType$6.nil, ptrType$6.nil, sliceType$4.nil), new x$1.constructor.elem(x$1))); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$2 = new method.ptr(ptrType$6.nil, ptrType$6.nil, ptrType$1.nil, ptrType$1.nil, 0, 0), new x$2.constructor.elem(x$2))); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$3 = new arrayType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), ptrType$1.nil, ptrType$1.nil, 0), new x$3.constructor.elem(x$3))); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$4 = new chanType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), ptrType$1.nil, 0), new x$4.constructor.elem(x$4))); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$5 = new funcType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), false, sliceType$1.nil, sliceType$1.nil), new x$5.constructor.elem(x$5))); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$6 = new interfaceType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), sliceType$5.nil), new x$6.constructor.elem(x$6))); /* */ $s = 7; case 7: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$7 = new mapType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), ptrType$1.nil, ptrType$1.nil, ptrType$1.nil, ptrType$1.nil, 0, 0, 0, 0, 0, false, false), new x$7.constructor.elem(x$7))); /* */ $s = 8; case 8: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$8 = new ptrType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), ptrType$1.nil), new x$8.constructor.elem(x$8))); /* */ $s = 9; case 9: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$9 = new sliceType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), ptrType$1.nil), new x$9.constructor.elem(x$9))); /* */ $s = 10; case 10: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$10 = new structType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), sliceType$6.nil), new x$10.constructor.elem(x$10))); /* */ $s = 11; case 11: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$11 = new imethod.ptr(ptrType$6.nil, ptrType$6.nil, ptrType$1.nil), new x$11.constructor.elem(x$11))); /* */ $s = 12; case 12: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = used((x$12 = new structField.ptr(ptrType$6.nil, ptrType$6.nil, ptrType$1.nil, ptrType$6.nil, 0), new x$12.constructor.elem(x$12))); /* */ $s = 13; case 13: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		initialized = true;
		uint8Type = $assertType(TypeOf(new $Uint8(0)), ptrType$1);
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: init }; } $f.$ptr = $ptr; $f.used = used; $f.x = x; $f.x$1 = x$1; $f.x$10 = x$10; $f.x$11 = x$11; $f.x$12 = x$12; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.x$6 = x$6; $f.x$7 = x$7; $f.x$8 = x$8; $f.x$9 = x$9; $f.$s = $s; $f.$r = $r; return $f;
	};
	jsType = function(typ) {
		var $ptr, typ;
		return typ.jsType;
	};
	reflectType = function(typ) {
		var $ptr, _1, _i, _i$1, _i$2, _i$3, _i$4, _ref, _ref$1, _ref$2, _ref$3, _ref$4, dir, f, fields, i, i$1, i$2, i$3, i$4, imethods, in$1, m, m$1, methodSet, methods, out, params, reflectFields, reflectMethods, results, rt, t, typ;
		if (typ.reflectType === undefined) {
			rt = new rtype.ptr((($parseInt(typ.size) >> 0) >>> 0), 0, 0, 0, 0, 0, (($parseInt(typ.kind) >> 0) << 24 >>> 24), ptrType$4.nil, ptrType$5.nil, newStringPtr(typ.string), ptrType$7.nil, ptrType$1.nil);
			rt.jsType = typ;
			typ.reflectType = rt;
			methodSet = $methodSet(typ);
			if (!($internalize(typ.typeName, $String) === "") || !(($parseInt(methodSet.length) === 0))) {
				reflectMethods = $makeSlice(sliceType$4, $parseInt(methodSet.length));
				_ref = reflectMethods;
				_i = 0;
				while (true) {
					if (!(_i < _ref.$length)) { break; }
					i = _i;
					m = methodSet[i];
					t = m.typ;
					method.copy(((i < 0 || i >= reflectMethods.$length) ? $throwRuntimeError("index out of range") : reflectMethods.$array[reflectMethods.$offset + i]), new method.ptr(newStringPtr(m.name), newStringPtr(m.pkg), reflectType(t), reflectType($funcType(new ($global.Array)(typ).concat(t.params), t.results, t.variadic)), 0, 0));
					_i++;
				}
				rt.uncommonType = new uncommonType.ptr(newStringPtr(typ.typeName), newStringPtr(typ.pkg), reflectMethods);
				rt.uncommonType.jsType = typ;
			}
			_1 = rt.Kind();
			if (_1 === (17)) {
				setKindType(rt, new arrayType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), reflectType(typ.elem), ptrType$1.nil, (($parseInt(typ.len) >> 0) >>> 0)));
			} else if (_1 === (18)) {
				dir = 3;
				if (!!(typ.sendOnly)) {
					dir = 2;
				}
				if (!!(typ.recvOnly)) {
					dir = 1;
				}
				setKindType(rt, new chanType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), reflectType(typ.elem), (dir >>> 0)));
			} else if (_1 === (19)) {
				params = typ.params;
				in$1 = $makeSlice(sliceType$1, $parseInt(params.length));
				_ref$1 = in$1;
				_i$1 = 0;
				while (true) {
					if (!(_i$1 < _ref$1.$length)) { break; }
					i$1 = _i$1;
					((i$1 < 0 || i$1 >= in$1.$length) ? $throwRuntimeError("index out of range") : in$1.$array[in$1.$offset + i$1] = reflectType(params[i$1]));
					_i$1++;
				}
				results = typ.results;
				out = $makeSlice(sliceType$1, $parseInt(results.length));
				_ref$2 = out;
				_i$2 = 0;
				while (true) {
					if (!(_i$2 < _ref$2.$length)) { break; }
					i$2 = _i$2;
					((i$2 < 0 || i$2 >= out.$length) ? $throwRuntimeError("index out of range") : out.$array[out.$offset + i$2] = reflectType(results[i$2]));
					_i$2++;
				}
				setKindType(rt, new funcType.ptr($clone(rt, rtype), !!(typ.variadic), in$1, out));
			} else if (_1 === (20)) {
				methods = typ.methods;
				imethods = $makeSlice(sliceType$5, $parseInt(methods.length));
				_ref$3 = imethods;
				_i$3 = 0;
				while (true) {
					if (!(_i$3 < _ref$3.$length)) { break; }
					i$3 = _i$3;
					m$1 = methods[i$3];
					imethod.copy(((i$3 < 0 || i$3 >= imethods.$length) ? $throwRuntimeError("index out of range") : imethods.$array[imethods.$offset + i$3]), new imethod.ptr(newStringPtr(m$1.name), newStringPtr(m$1.pkg), reflectType(m$1.typ)));
					_i$3++;
				}
				setKindType(rt, new interfaceType.ptr($clone(rt, rtype), imethods));
			} else if (_1 === (21)) {
				setKindType(rt, new mapType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), reflectType(typ.key), reflectType(typ.elem), ptrType$1.nil, ptrType$1.nil, 0, 0, 0, 0, 0, false, false));
			} else if (_1 === (22)) {
				setKindType(rt, new ptrType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), reflectType(typ.elem)));
			} else if (_1 === (23)) {
				setKindType(rt, new sliceType.ptr(new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil), reflectType(typ.elem)));
			} else if (_1 === (25)) {
				fields = typ.fields;
				reflectFields = $makeSlice(sliceType$6, $parseInt(fields.length));
				_ref$4 = reflectFields;
				_i$4 = 0;
				while (true) {
					if (!(_i$4 < _ref$4.$length)) { break; }
					i$4 = _i$4;
					f = fields[i$4];
					structField.copy(((i$4 < 0 || i$4 >= reflectFields.$length) ? $throwRuntimeError("index out of range") : reflectFields.$array[reflectFields.$offset + i$4]), new structField.ptr(newStringPtr(f.name), newStringPtr(f.pkg), reflectType(f.typ), newStringPtr(f.tag), (i$4 >>> 0)));
					_i$4++;
				}
				setKindType(rt, new structType.ptr($clone(rt, rtype), reflectFields));
			}
		}
		return typ.reflectType;
	};
	setKindType = function(rt, kindType) {
		var $ptr, kindType, rt;
		rt.kindType = kindType;
		kindType.rtype = rt;
	};
	newStringPtr = function(strObj) {
		var $ptr, _entry, _key, _tuple, c, ok, ptr, str, str$24ptr, strObj;
		c = new structType$6.ptr("");
		c.str = strObj;
		str = c.str;
		if (str === "") {
			return ptrType$6.nil;
		}
		_tuple = (_entry = stringPtrMap[$String.keyFor(str)], _entry !== undefined ? [_entry.v, true] : [ptrType$6.nil, false]);
		ptr = _tuple[0];
		ok = _tuple[1];
		if (!ok) {
			ptr = (str$24ptr || (str$24ptr = new ptrType$6(function() { return str; }, function($v) { str = $v; })));
			_key = str; (stringPtrMap || $throwRuntimeError("assignment to entry in nil map"))[$String.keyFor(_key)] = { k: _key, v: ptr };
		}
		return ptr;
	};
	isWrapped = function(typ) {
		var $ptr, typ;
		return !!(jsType(typ).wrapped);
	};
	copyStruct = function(dst, src, typ) {
		var $ptr, dst, fields, i, prop, src, typ;
		fields = jsType(typ).fields;
		i = 0;
		while (true) {
			if (!(i < $parseInt(fields.length))) { break; }
			prop = $internalize(fields[i].prop, $String);
			dst[$externalize(prop, $String)] = src[$externalize(prop, $String)];
			i = i + (1) >> 0;
		}
	};
	makeValue = function(t, v, fl) {
		var $ptr, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _v, _v$1, fl, rt, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _v = $f._v; _v$1 = $f._v$1; fl = $f.fl; rt = $f.rt; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = t.common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		rt = _r;
		_r$1 = t.Kind(); /* */ $s = 6; case 6: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		if (_r$1 === 17) { _v$1 = true; $s = 5; continue s; }
		_r$2 = t.Kind(); /* */ $s = 7; case 7: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_v$1 = _r$2 === 25; case 5:
		if (_v$1) { _v = true; $s = 4; continue s; }
		_r$3 = t.Kind(); /* */ $s = 8; case 8: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		_v = _r$3 === 22; case 4:
		/* */ if (_v) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (_v) { */ case 2:
			_r$4 = t.Kind(); /* */ $s = 9; case 9: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			/* */ $s = 10; case 10:
			return new Value.ptr(rt, v, (fl | (_r$4 >>> 0)) >>> 0);
		/* } */ case 3:
		_r$5 = t.Kind(); /* */ $s = 11; case 11: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
		/* */ $s = 12; case 12:
		return new Value.ptr(rt, $newDataPointer(v, jsType(rt.ptrTo())), (((fl | (_r$5 >>> 0)) >>> 0) | 128) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeValue }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._v = _v; $f._v$1 = _v$1; $f.fl = fl; $f.rt = rt; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	MakeSlice = function(typ, len, cap) {
		var $ptr, _r, _r$1, cap, len, typ, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; cap = $f.cap; len = $f.len; typ = $f.typ; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		typ = [typ];
		_r = typ[0].Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (!((_r === 23))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((_r === 23))) { */ case 1:
			$panic(new $String("reflect.MakeSlice of non-slice type"));
		/* } */ case 2:
		if (len < 0) {
			$panic(new $String("reflect.MakeSlice: negative len"));
		}
		if (cap < 0) {
			$panic(new $String("reflect.MakeSlice: negative cap"));
		}
		if (len > cap) {
			$panic(new $String("reflect.MakeSlice: len > cap"));
		}
		_r$1 = makeValue(typ[0], $makeSlice(jsType(typ[0]), len, cap, (function(typ) { return function $b() {
			var $ptr, _r$1, _r$2, $s, $r;
			/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
			_r$1 = typ[0].Elem(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			_r$2 = jsType(_r$1); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			/* */ $s = 3; case 3:
			return _r$2.zero();
			/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.$s = $s; $f.$r = $r; return $f;
		}; })(typ)), 0); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 5; case 5:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: MakeSlice }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.cap = cap; $f.len = len; $f.typ = typ; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.MakeSlice = MakeSlice;
	TypeOf = function(i) {
		var $ptr, i;
		if (!initialized) {
			return new rtype.ptr(0, 0, 0, 0, 0, 0, 0, ptrType$4.nil, ptrType$5.nil, ptrType$6.nil, ptrType$7.nil, ptrType$1.nil);
		}
		if ($interfaceIsEqual(i, $ifaceNil)) {
			return $ifaceNil;
		}
		return reflectType(i.constructor);
	};
	$pkg.TypeOf = TypeOf;
	ValueOf = function(i) {
		var $ptr, _r, i, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; i = $f.i; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		if ($interfaceIsEqual(i, $ifaceNil)) {
			return new Value.ptr(ptrType$1.nil, 0, 0);
		}
		_r = makeValue(reflectType(i.constructor), i.$val, 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: ValueOf }; } $f.$ptr = $ptr; $f._r = _r; $f.i = i; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.ValueOf = ValueOf;
	rtype.ptr.prototype.ptrTo = function() {
		var $ptr, t;
		t = this;
		return reflectType($ptrType(jsType(t)));
	};
	rtype.prototype.ptrTo = function() { return this.$val.ptrTo(); };
	SliceOf = function(t) {
		var $ptr, t;
		return reflectType($sliceType(jsType(t)));
	};
	$pkg.SliceOf = SliceOf;
	Zero = function(typ) {
		var $ptr, _r, typ, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; typ = $f.typ; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = makeValue(typ, jsType(typ).zero(), 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Zero }; } $f.$ptr = $ptr; $f._r = _r; $f.typ = typ; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Zero = Zero;
	unsafe_New = function(typ) {
		var $ptr, _3, typ;
		_3 = typ.Kind();
		if (_3 === (25)) {
			return new (jsType(typ).ptr)();
		} else if (_3 === (17)) {
			return jsType(typ).zero();
		} else {
			return $newDataPointer(jsType(typ).zero(), jsType(typ.ptrTo()));
		}
	};
	makeInt = function(f, bits, t) {
		var $ptr, _4, _r, bits, f, ptr, t, typ, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _4 = $f._4; _r = $f._r; bits = $f.bits; f = $f.f; ptr = $f.ptr; t = $f.t; typ = $f.typ; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = t.common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		typ = _r;
		ptr = unsafe_New(typ);
		_4 = typ.Kind();
		if (_4 === (3)) {
			ptr.$set((bits.$low << 24 >> 24));
		} else if (_4 === (4)) {
			ptr.$set((bits.$low << 16 >> 16));
		} else if ((_4 === (2)) || (_4 === (5))) {
			ptr.$set((bits.$low >> 0));
		} else if (_4 === (6)) {
			ptr.$set(new $Int64(bits.$high, bits.$low));
		} else if (_4 === (8)) {
			ptr.$set((bits.$low << 24 >>> 24));
		} else if (_4 === (9)) {
			ptr.$set((bits.$low << 16 >>> 16));
		} else if ((_4 === (7)) || (_4 === (10)) || (_4 === (12))) {
			ptr.$set((bits.$low >>> 0));
		} else if (_4 === (11)) {
			ptr.$set(bits);
		}
		return new Value.ptr(typ, ptr, (((f | 128) >>> 0) | (typ.Kind() >>> 0)) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeInt }; } $f.$ptr = $ptr; $f._4 = _4; $f._r = _r; $f.bits = bits; $f.f = f; $f.ptr = ptr; $f.t = t; $f.typ = typ; $f.$s = $s; $f.$r = $r; return $f;
	};
	typedmemmove = function(t, dst, src) {
		var $ptr, dst, src, t;
		dst.$set(src.$get());
	};
	keyFor = function(t, key) {
		var $ptr, k, key, kv, t;
		kv = key;
		if (!(kv.$get === undefined)) {
			kv = kv.$get();
		}
		k = $internalize(jsType(t.Key()).keyFor(kv), $String);
		return [kv, k];
	};
	mapaccess = function(t, m, key) {
		var $ptr, _tuple, entry, k, key, m, t;
		_tuple = keyFor(t, key);
		k = _tuple[1];
		entry = m[$externalize(k, $String)];
		if (entry === undefined) {
			return 0;
		}
		return $newDataPointer(entry.v, jsType(PtrTo(t.Elem())));
	};
	mapassign = function(t, m, key, val) {
		var $ptr, _r, _tuple, entry, et, jsVal, k, key, kv, m, newVal, t, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tuple = $f._tuple; entry = $f.entry; et = $f.et; jsVal = $f.jsVal; k = $f.k; key = $f.key; kv = $f.kv; m = $f.m; newVal = $f.newVal; t = $f.t; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_tuple = keyFor(t, key);
		kv = _tuple[0];
		k = _tuple[1];
		jsVal = val.$get();
		et = t.Elem();
		_r = et.Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (_r === 25) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (_r === 25) { */ case 1:
			newVal = jsType(et).zero();
			copyStruct(newVal, jsVal, et);
			jsVal = newVal;
		/* } */ case 2:
		entry = new ($global.Object)();
		entry.k = kv;
		entry.v = jsVal;
		m[$externalize(k, $String)] = entry;
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: mapassign }; } $f.$ptr = $ptr; $f._r = _r; $f._tuple = _tuple; $f.entry = entry; $f.et = et; $f.jsVal = jsVal; $f.k = k; $f.key = key; $f.kv = kv; $f.m = m; $f.newVal = newVal; $f.t = t; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	mapdelete = function(t, m, key) {
		var $ptr, _tuple, k, key, m, t;
		_tuple = keyFor(t, key);
		k = _tuple[1];
		delete m[$externalize(k, $String)];
	};
	mapiterinit = function(t, m) {
		var $ptr, m, t;
		return new mapIter.ptr(t, m, $keys(m), 0);
	};
	mapiterkey = function(it) {
		var $ptr, _r, _r$1, _r$2, it, iter, k, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; it = $f.it; iter = $f.iter; k = $f.k; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		iter = it;
		k = iter.keys[iter.i];
		_r = iter.t.Key(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r$1 = PtrTo(_r); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_r$2 = jsType(_r$1); /* */ $s = 3; case 3: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		/* */ $s = 4; case 4:
		return $newDataPointer(iter.m[$externalize($internalize(k, $String), $String)].k, _r$2);
		/* */ } return; } if ($f === undefined) { $f = { $blk: mapiterkey }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.it = it; $f.iter = iter; $f.k = k; $f.$s = $s; $f.$r = $r; return $f;
	};
	mapiternext = function(it) {
		var $ptr, it, iter;
		iter = it;
		iter.i = iter.i + (1) >> 0;
	};
	maplen = function(m) {
		var $ptr, m;
		return $parseInt($keys(m).length);
	};
	cvtDirect = function(v, typ) {
		var $ptr, _6, _arg, _arg$1, _arg$2, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, k, slice, srcVal, typ, v, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _6 = $f._6; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; k = $f.k; slice = $f.slice; srcVal = $f.srcVal; typ = $f.typ; v = $f.v; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		srcVal = v.object();
		/* */ if (srcVal === jsType(v.typ).nil) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (srcVal === jsType(v.typ).nil) { */ case 1:
			_r = makeValue(typ, jsType(typ).nil, v.flag); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			/* */ $s = 4; case 4:
			return _r;
		/* } */ case 2:
		val = null;
			_r$1 = typ.Kind(); /* */ $s = 6; case 6: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			k = _r$1;
			_6 = k;
			/* */ if (_6 === (23)) { $s = 7; continue; }
			/* */ if (_6 === (22)) { $s = 8; continue; }
			/* */ if (_6 === (25)) { $s = 9; continue; }
			/* */ if ((_6 === (17)) || (_6 === (1)) || (_6 === (18)) || (_6 === (19)) || (_6 === (20)) || (_6 === (21)) || (_6 === (24))) { $s = 10; continue; }
			/* */ $s = 11; continue;
			/* if (_6 === (23)) { */ case 7:
				slice = new (jsType(typ))(srcVal.$array);
				slice.$offset = srcVal.$offset;
				slice.$length = srcVal.$length;
				slice.$capacity = srcVal.$capacity;
				val = $newDataPointer(slice, jsType(PtrTo(typ)));
				$s = 12; continue;
			/* } else if (_6 === (22)) { */ case 8:
				_r$2 = typ.Elem(); /* */ $s = 15; case 15: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_r$3 = _r$2.Kind(); /* */ $s = 16; case 16: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				/* */ if (_r$3 === 25) { $s = 13; continue; }
				/* */ $s = 14; continue;
				/* if (_r$3 === 25) { */ case 13:
					_r$4 = typ.Elem(); /* */ $s = 19; case 19: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
					/* */ if ($interfaceIsEqual(_r$4, v.typ.Elem())) { $s = 17; continue; }
					/* */ $s = 18; continue;
					/* if ($interfaceIsEqual(_r$4, v.typ.Elem())) { */ case 17:
						val = srcVal;
						/* break; */ $s = 5; continue;
					/* } */ case 18:
					val = new (jsType(typ))();
					_arg = val;
					_arg$1 = srcVal;
					_r$5 = typ.Elem(); /* */ $s = 20; case 20: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
					_arg$2 = _r$5;
					$r = copyStruct(_arg, _arg$1, _arg$2); /* */ $s = 21; case 21: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
					/* break; */ $s = 5; continue;
				/* } */ case 14:
				val = new (jsType(typ))(srcVal.$get, srcVal.$set);
				$s = 12; continue;
			/* } else if (_6 === (25)) { */ case 9:
				val = new (jsType(typ).ptr)();
				copyStruct(val, srcVal, typ);
				$s = 12; continue;
			/* } else if ((_6 === (17)) || (_6 === (1)) || (_6 === (18)) || (_6 === (19)) || (_6 === (20)) || (_6 === (21)) || (_6 === (24))) { */ case 10:
				val = v.ptr;
				$s = 12; continue;
			/* } else { */ case 11:
				$panic(new ValueError.ptr("reflect.Convert", k));
			/* } */ case 12:
		case 5:
		_r$6 = typ.common(); /* */ $s = 22; case 22: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
		_r$7 = typ.Kind(); /* */ $s = 23; case 23: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
		/* */ $s = 24; case 24:
		return new Value.ptr(_r$6, val, (((v.flag & 224) >>> 0) | (_r$7 >>> 0)) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtDirect }; } $f.$ptr = $ptr; $f._6 = _6; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f.k = k; $f.slice = slice; $f.srcVal = srcVal; $f.typ = typ; $f.v = v; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	methodReceiver = function(op, v, i) {
		var $ptr, fn, i, iface, m, m$1, op, prop, rcvr, rcvrtype, t, tt, ut, v, x, x$1;
		rcvrtype = ptrType$1.nil;
		t = ptrType$1.nil;
		fn = 0;
		v = v;
		prop = "";
		if (v.typ.Kind() === 20) {
			tt = v.typ.kindType;
			if (i < 0 || i >= tt.methods.$length) {
				$panic(new $String("reflect: internal error: invalid method index"));
			}
			m = (x = tt.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
			if (!(m.pkgPath === ptrType$6.nil)) {
				$panic(new $String("reflect: " + op + " of unexported method"));
			}
			iface = $pointerOfStructConversion(v.ptr, ptrType$8);
			if (iface.itab === ptrType$9.nil) {
				$panic(new $String("reflect: " + op + " of method on nil interface value"));
			}
			t = m.typ;
			prop = m.name.$get();
		} else {
			ut = v.typ.uncommonType.uncommon();
			if (ut === ptrType$7.nil || i < 0 || i >= ut.methods.$length) {
				$panic(new $String("reflect: internal error: invalid method index"));
			}
			m$1 = (x$1 = ut.methods, ((i < 0 || i >= x$1.$length) ? $throwRuntimeError("index out of range") : x$1.$array[x$1.$offset + i]));
			if (!(m$1.pkgPath === ptrType$6.nil)) {
				$panic(new $String("reflect: " + op + " of unexported method"));
			}
			t = m$1.mtyp;
			prop = $internalize($methodSet(jsType(v.typ))[i].prop, $String);
		}
		rcvr = v.object();
		if (isWrapped(v.typ)) {
			rcvr = new (jsType(v.typ))(rcvr);
		}
		fn = rcvr[$externalize(prop, $String)];
		return [rcvrtype, t, fn];
	};
	valueInterface = function(v, safe) {
		var $ptr, _r, safe, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; safe = $f.safe; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		if (v.flag === 0) {
			$panic(new ValueError.ptr("reflect.Value.Interface", 0));
		}
		if (safe && !((((v.flag & 96) >>> 0) === 0))) {
			$panic(new $String("reflect.Value.Interface: cannot return value obtained from unexported field or method"));
		}
		/* */ if (!((((v.flag & 512) >>> 0) === 0))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((((v.flag & 512) >>> 0) === 0))) { */ case 1:
			_r = makeMethodValue("Interface", v); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			v = _r;
		/* } */ case 2:
		if (isWrapped(v.typ)) {
			return new (jsType(v.typ))(v.object());
		}
		return v.object();
		/* */ } return; } if ($f === undefined) { $f = { $blk: valueInterface }; } $f.$ptr = $ptr; $f._r = _r; $f.safe = safe; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	ifaceE2I = function(t, src, dst) {
		var $ptr, dst, src, t;
		dst.$set(src);
	};
	methodName = function() {
		var $ptr;
		return "?FIXME?";
	};
	makeMethodValue = function(op, v) {
		var $ptr, _r, _tuple, fn, fv, op, rcvr, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tuple = $f._tuple; fn = $f.fn; fv = $f.fv; op = $f.op; rcvr = $f.rcvr; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		fn = [fn];
		rcvr = [rcvr];
		v = v;
		if (((v.flag & 512) >>> 0) === 0) {
			$panic(new $String("reflect: internal error: invalid use of makePartialFunc"));
		}
		_tuple = methodReceiver(op, v, (v.flag >> 0) >> 10 >> 0);
		fn[0] = _tuple[2];
		rcvr[0] = v.object();
		if (isWrapped(v.typ)) {
			rcvr[0] = new (jsType(v.typ))(rcvr[0]);
		}
		fv = $makeFunc((function(fn, rcvr) { return function(arguments$1) {
			var $ptr, arguments$1;
			return fn[0].apply(rcvr[0], $externalize(arguments$1, sliceType$7));
		}; })(fn, rcvr));
		_r = v.Type().common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return new Value.ptr(_r, fv, (((v.flag & 96) >>> 0) | 19) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeMethodValue }; } $f.$ptr = $ptr; $f._r = _r; $f._tuple = _tuple; $f.fn = fn; $f.fv = fv; $f.op = op; $f.rcvr = rcvr; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.ptr.prototype.pointers = function() {
		var $ptr, _7, t;
		t = this;
		_7 = t.Kind();
		if ((_7 === (22)) || (_7 === (21)) || (_7 === (18)) || (_7 === (19)) || (_7 === (25)) || (_7 === (17))) {
			return true;
		} else {
			return false;
		}
	};
	rtype.prototype.pointers = function() { return this.$val.pointers(); };
	rtype.ptr.prototype.Comparable = function() {
		var $ptr, _8, _r, _r$1, _r$2, i, t, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _8 = $f._8; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; i = $f.i; t = $f.t; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
			_8 = t.Kind();
			/* */ if ((_8 === (19)) || (_8 === (23)) || (_8 === (21))) { $s = 2; continue; }
			/* */ if (_8 === (17)) { $s = 3; continue; }
			/* */ if (_8 === (25)) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if ((_8 === (19)) || (_8 === (23)) || (_8 === (21))) { */ case 2:
				return false;
			/* } else if (_8 === (17)) { */ case 3:
				_r = t.Elem().Comparable(); /* */ $s = 6; case 6: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				/* */ $s = 7; case 7:
				return _r;
			/* } else if (_8 === (25)) { */ case 4:
				i = 0;
				/* while (true) { */ case 8:
					/* if (!(i < t.NumField())) { break; } */ if(!(i < t.NumField())) { $s = 9; continue; }
					_r$1 = t.Field(i); /* */ $s = 12; case 12: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
					_r$2 = _r$1.Type.Comparable(); /* */ $s = 13; case 13: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					/* */ if (!_r$2) { $s = 10; continue; }
					/* */ $s = 11; continue;
					/* if (!_r$2) { */ case 10:
						return false;
					/* } */ case 11:
					i = i + (1) >> 0;
				/* } */ $s = 8; continue; case 9:
			/* } */ case 5:
		case 1:
		return true;
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.Comparable }; } $f.$ptr = $ptr; $f._8 = _8; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.i = i; $f.t = t; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.Comparable = function() { return this.$val.Comparable(); };
	uncommonType.ptr.prototype.Method = function(i) {
		var $ptr, fl, fn, i, m, mt, p, prop, t, x;
		m = new Method.ptr("", "", $ifaceNil, new Value.ptr(ptrType$1.nil, 0, 0), 0);
		t = this;
		if (t === ptrType$7.nil || i < 0 || i >= t.methods.$length) {
			$panic(new $String("reflect: Method index out of range"));
		}
		p = (x = t.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
		if (!(p.name === ptrType$6.nil)) {
			m.Name = p.name.$get();
		}
		fl = 19;
		if (!(p.pkgPath === ptrType$6.nil)) {
			m.PkgPath = p.pkgPath.$get();
			fl = (fl | (32)) >>> 0;
		}
		mt = p.typ;
		m.Type = mt;
		prop = $internalize($methodSet(t.jsType)[i].prop, $String);
		fn = $makeFunc((function(arguments$1) {
			var $ptr, arguments$1, rcvr;
			rcvr = (0 >= arguments$1.$length ? $throwRuntimeError("index out of range") : arguments$1.$array[arguments$1.$offset + 0]);
			return rcvr[$externalize(prop, $String)].apply(rcvr, $externalize($subslice(arguments$1, 1), sliceType$7));
		}));
		m.Func = new Value.ptr(mt, fn, fl);
		m.Index = i;
		return m;
	};
	uncommonType.prototype.Method = function(i) { return this.$val.Method(i); };
	Value.ptr.prototype.object = function() {
		var $ptr, _9, newVal, v, val;
		v = this;
		if ((v.typ.Kind() === 17) || (v.typ.Kind() === 25)) {
			return v.ptr;
		}
		if (!((((v.flag & 128) >>> 0) === 0))) {
			val = v.ptr.$get();
			if (!(val === $ifaceNil) && !(val.constructor === jsType(v.typ))) {
				switch (0) { default:
					_9 = v.typ.Kind();
					if ((_9 === (11)) || (_9 === (6))) {
						val = new (jsType(v.typ))(val.$high, val.$low);
					} else if ((_9 === (15)) || (_9 === (16))) {
						val = new (jsType(v.typ))(val.$real, val.$imag);
					} else if (_9 === (23)) {
						if (val === val.constructor.nil) {
							val = jsType(v.typ).nil;
							break;
						}
						newVal = new (jsType(v.typ))(val.$array);
						newVal.$offset = val.$offset;
						newVal.$length = val.$length;
						newVal.$capacity = val.$capacity;
						val = newVal;
					}
				}
			}
			return val;
		}
		return v.ptr;
	};
	Value.prototype.object = function() { return this.$val.object(); };
	Value.ptr.prototype.call = function(op, in$1) {
		var $ptr, _10, _arg, _arg$1, _arg$2, _arg$3, _i, _i$1, _i$2, _r, _r$1, _r$10, _r$11, _r$12, _r$13, _r$14, _r$15, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, _r$8, _r$9, _ref, _ref$1, _ref$2, _tmp, _tmp$1, _tuple, arg, argsArray, elem, fn, i, i$1, i$2, i$3, in$1, isSlice, m, n, nin, nout, op, origIn, rcvr, results, ret, slice, t, targ, v, x, x$1, x$2, xt, xt$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _10 = $f._10; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _arg$3 = $f._arg$3; _i = $f._i; _i$1 = $f._i$1; _i$2 = $f._i$2; _r = $f._r; _r$1 = $f._r$1; _r$10 = $f._r$10; _r$11 = $f._r$11; _r$12 = $f._r$12; _r$13 = $f._r$13; _r$14 = $f._r$14; _r$15 = $f._r$15; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _r$8 = $f._r$8; _r$9 = $f._r$9; _ref = $f._ref; _ref$1 = $f._ref$1; _ref$2 = $f._ref$2; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tuple = $f._tuple; arg = $f.arg; argsArray = $f.argsArray; elem = $f.elem; fn = $f.fn; i = $f.i; i$1 = $f.i$1; i$2 = $f.i$2; i$3 = $f.i$3; in$1 = $f.in$1; isSlice = $f.isSlice; m = $f.m; n = $f.n; nin = $f.nin; nout = $f.nout; op = $f.op; origIn = $f.origIn; rcvr = $f.rcvr; results = $f.results; ret = $f.ret; slice = $f.slice; t = $f.t; targ = $f.targ; v = $f.v; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; xt = $f.xt; xt$1 = $f.xt$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		t = v.typ;
		fn = 0;
		rcvr = null;
		if (!((((v.flag & 512) >>> 0) === 0))) {
			_tuple = methodReceiver(op, v, (v.flag >> 0) >> 10 >> 0);
			t = _tuple[1];
			fn = _tuple[2];
			rcvr = v.object();
			if (isWrapped(v.typ)) {
				rcvr = new (jsType(v.typ))(rcvr);
			}
		} else {
			fn = v.object();
			rcvr = undefined;
		}
		if (fn === 0) {
			$panic(new $String("reflect.Value.Call: call of nil function"));
		}
		isSlice = op === "CallSlice";
		n = t.NumIn();
		if (isSlice) {
			if (!t.IsVariadic()) {
				$panic(new $String("reflect: CallSlice of non-variadic function"));
			}
			if (in$1.$length < n) {
				$panic(new $String("reflect: CallSlice with too few input arguments"));
			}
			if (in$1.$length > n) {
				$panic(new $String("reflect: CallSlice with too many input arguments"));
			}
		} else {
			if (t.IsVariadic()) {
				n = n - (1) >> 0;
			}
			if (in$1.$length < n) {
				$panic(new $String("reflect: Call with too few input arguments"));
			}
			if (!t.IsVariadic() && in$1.$length > n) {
				$panic(new $String("reflect: Call with too many input arguments"));
			}
		}
		_ref = in$1;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			x = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			if (x.Kind() === 0) {
				$panic(new $String("reflect: " + op + " using zero Value argument"));
			}
			_i++;
		}
		i = 0;
		/* while (true) { */ case 1:
			/* if (!(i < n)) { break; } */ if(!(i < n)) { $s = 2; continue; }
			_tmp = ((i < 0 || i >= in$1.$length) ? $throwRuntimeError("index out of range") : in$1.$array[in$1.$offset + i]).Type();
			_tmp$1 = t.In(i);
			xt = _tmp;
			targ = _tmp$1;
			_r = xt.AssignableTo(targ); /* */ $s = 5; case 5: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			/* */ if (!_r) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!_r) { */ case 3:
				_r$1 = xt.String(); /* */ $s = 6; case 6: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				_r$2 = targ.String(); /* */ $s = 7; case 7: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				$panic(new $String("reflect: " + op + " using " + _r$1 + " as type " + _r$2));
			/* } */ case 4:
			i = i + (1) >> 0;
		/* } */ $s = 1; continue; case 2:
		/* */ if (!isSlice && t.IsVariadic()) { $s = 8; continue; }
		/* */ $s = 9; continue;
		/* if (!isSlice && t.IsVariadic()) { */ case 8:
			m = in$1.$length - n >> 0;
			_r$3 = MakeSlice(t.In(n), m, m); /* */ $s = 10; case 10: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			slice = _r$3;
			_r$4 = t.In(n).Elem(); /* */ $s = 11; case 11: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			elem = _r$4;
			i$1 = 0;
			/* while (true) { */ case 12:
				/* if (!(i$1 < m)) { break; } */ if(!(i$1 < m)) { $s = 13; continue; }
				x$2 = (x$1 = n + i$1 >> 0, ((x$1 < 0 || x$1 >= in$1.$length) ? $throwRuntimeError("index out of range") : in$1.$array[in$1.$offset + x$1]));
				xt$1 = x$2.Type();
				_r$5 = xt$1.AssignableTo(elem); /* */ $s = 16; case 16: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
				/* */ if (!_r$5) { $s = 14; continue; }
				/* */ $s = 15; continue;
				/* if (!_r$5) { */ case 14:
					_r$6 = xt$1.String(); /* */ $s = 17; case 17: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
					_r$7 = elem.String(); /* */ $s = 18; case 18: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
					$panic(new $String("reflect: cannot use " + _r$6 + " as type " + _r$7 + " in " + op));
				/* } */ case 15:
				_r$8 = slice.Index(i$1); /* */ $s = 19; case 19: if($c) { $c = false; _r$8 = _r$8.$blk(); } if (_r$8 && _r$8.$blk !== undefined) { break s; }
				$r = _r$8.Set(x$2); /* */ $s = 20; case 20: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				i$1 = i$1 + (1) >> 0;
			/* } */ $s = 12; continue; case 13:
			origIn = in$1;
			in$1 = $makeSlice(sliceType$8, (n + 1 >> 0));
			$copySlice($subslice(in$1, 0, n), origIn);
			((n < 0 || n >= in$1.$length) ? $throwRuntimeError("index out of range") : in$1.$array[in$1.$offset + n] = slice);
		/* } */ case 9:
		nin = in$1.$length;
		if (!((nin === t.NumIn()))) {
			$panic(new $String("reflect.Value.Call: wrong argument count"));
		}
		nout = t.NumOut();
		argsArray = new ($global.Array)(t.NumIn());
		_ref$1 = in$1;
		_i$1 = 0;
		/* while (true) { */ case 21:
			/* if (!(_i$1 < _ref$1.$length)) { break; } */ if(!(_i$1 < _ref$1.$length)) { $s = 22; continue; }
			i$2 = _i$1;
			arg = ((_i$1 < 0 || _i$1 >= _ref$1.$length) ? $throwRuntimeError("index out of range") : _ref$1.$array[_ref$1.$offset + _i$1]);
			_arg = t.In(i$2);
			_r$9 = t.In(i$2).common(); /* */ $s = 23; case 23: if($c) { $c = false; _r$9 = _r$9.$blk(); } if (_r$9 && _r$9.$blk !== undefined) { break s; }
			_arg$1 = _r$9;
			_arg$2 = 0;
			_r$10 = arg.assignTo("reflect.Value.Call", _arg$1, _arg$2); /* */ $s = 24; case 24: if($c) { $c = false; _r$10 = _r$10.$blk(); } if (_r$10 && _r$10.$blk !== undefined) { break s; }
			_r$11 = _r$10.object(); /* */ $s = 25; case 25: if($c) { $c = false; _r$11 = _r$11.$blk(); } if (_r$11 && _r$11.$blk !== undefined) { break s; }
			_arg$3 = _r$11;
			_r$12 = unwrapJsObject(_arg, _arg$3); /* */ $s = 26; case 26: if($c) { $c = false; _r$12 = _r$12.$blk(); } if (_r$12 && _r$12.$blk !== undefined) { break s; }
			argsArray[i$2] = _r$12;
			_i$1++;
		/* } */ $s = 21; continue; case 22:
		_r$13 = callHelper(new sliceType$2([new $jsObjectPtr(fn), new $jsObjectPtr(rcvr), new $jsObjectPtr(argsArray)])); /* */ $s = 27; case 27: if($c) { $c = false; _r$13 = _r$13.$blk(); } if (_r$13 && _r$13.$blk !== undefined) { break s; }
		results = _r$13;
			_10 = nout;
			/* */ if (_10 === (0)) { $s = 29; continue; }
			/* */ if (_10 === (1)) { $s = 30; continue; }
			/* */ $s = 31; continue;
			/* if (_10 === (0)) { */ case 29:
				return sliceType$8.nil;
			/* } else if (_10 === (1)) { */ case 30:
				_r$14 = makeValue(t.Out(0), wrapJsObject(t.Out(0), results), 0); /* */ $s = 33; case 33: if($c) { $c = false; _r$14 = _r$14.$blk(); } if (_r$14 && _r$14.$blk !== undefined) { break s; }
				/* */ $s = 34; case 34:
				return new sliceType$8([$clone(_r$14, Value)]);
			/* } else { */ case 31:
				ret = $makeSlice(sliceType$8, nout);
				_ref$2 = ret;
				_i$2 = 0;
				/* while (true) { */ case 35:
					/* if (!(_i$2 < _ref$2.$length)) { break; } */ if(!(_i$2 < _ref$2.$length)) { $s = 36; continue; }
					i$3 = _i$2;
					_r$15 = makeValue(t.Out(i$3), wrapJsObject(t.Out(i$3), results[i$3]), 0); /* */ $s = 37; case 37: if($c) { $c = false; _r$15 = _r$15.$blk(); } if (_r$15 && _r$15.$blk !== undefined) { break s; }
					((i$3 < 0 || i$3 >= ret.$length) ? $throwRuntimeError("index out of range") : ret.$array[ret.$offset + i$3] = _r$15);
					_i$2++;
				/* } */ $s = 35; continue; case 36:
				return ret;
			/* } */ case 32:
		case 28:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.call }; } $f.$ptr = $ptr; $f._10 = _10; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._arg$3 = _arg$3; $f._i = _i; $f._i$1 = _i$1; $f._i$2 = _i$2; $f._r = _r; $f._r$1 = _r$1; $f._r$10 = _r$10; $f._r$11 = _r$11; $f._r$12 = _r$12; $f._r$13 = _r$13; $f._r$14 = _r$14; $f._r$15 = _r$15; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._r$8 = _r$8; $f._r$9 = _r$9; $f._ref = _ref; $f._ref$1 = _ref$1; $f._ref$2 = _ref$2; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tuple = _tuple; $f.arg = arg; $f.argsArray = argsArray; $f.elem = elem; $f.fn = fn; $f.i = i; $f.i$1 = i$1; $f.i$2 = i$2; $f.i$3 = i$3; $f.in$1 = in$1; $f.isSlice = isSlice; $f.m = m; $f.n = n; $f.nin = nin; $f.nout = nout; $f.op = op; $f.origIn = origIn; $f.rcvr = rcvr; $f.results = results; $f.ret = ret; $f.slice = slice; $f.t = t; $f.targ = targ; $f.v = v; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.xt = xt; $f.xt$1 = xt$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.call = function(op, in$1) { return this.$val.call(op, in$1); };
	Value.ptr.prototype.Cap = function() {
		var $ptr, _11, k, v;
		v = this;
		k = new flag(v.flag).kind();
		_11 = k;
		if (_11 === (17)) {
			return v.typ.Len();
		} else if ((_11 === (18)) || (_11 === (23))) {
			return $parseInt(v.object().$capacity) >> 0;
		}
		$panic(new ValueError.ptr("reflect.Value.Cap", k));
	};
	Value.prototype.Cap = function() { return this.$val.Cap(); };
	wrapJsObject = function(typ, val) {
		var $ptr, typ, val;
		if ($interfaceIsEqual(typ, jsObjectPtr)) {
			return new (jsType(jsObjectPtr))(val);
		}
		return val;
	};
	unwrapJsObject = function(typ, val) {
		var $ptr, typ, val;
		if ($interfaceIsEqual(typ, jsObjectPtr)) {
			return val.object;
		}
		return val;
	};
	Value.ptr.prototype.Elem = function() {
		var $ptr, _12, _r, fl, k, tt, typ, v, val, val$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _12 = $f._12; _r = $f._r; fl = $f.fl; k = $f.k; tt = $f.tt; typ = $f.typ; v = $f.v; val = $f.val; val$1 = $f.val$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
			k = new flag(v.flag).kind();
			_12 = k;
			/* */ if (_12 === (20)) { $s = 2; continue; }
			/* */ if (_12 === (22)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (_12 === (20)) { */ case 2:
				val = v.object();
				if (val === $ifaceNil) {
					return new Value.ptr(ptrType$1.nil, 0, 0);
				}
				typ = reflectType(val.constructor);
				_r = makeValue(typ, val.$val, (v.flag & 96) >>> 0); /* */ $s = 6; case 6: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				/* */ $s = 7; case 7:
				return _r;
			/* } else if (_12 === (22)) { */ case 3:
				if (v.IsNil()) {
					return new Value.ptr(ptrType$1.nil, 0, 0);
				}
				val$1 = v.object();
				tt = v.typ.kindType;
				fl = (((((v.flag & 96) >>> 0) | 128) >>> 0) | 256) >>> 0;
				fl = (fl | ((tt.elem.Kind() >>> 0))) >>> 0;
				return new Value.ptr(tt.elem, wrapJsObject(tt.elem, val$1), fl);
			/* } else { */ case 4:
				$panic(new ValueError.ptr("reflect.Value.Elem", k));
			/* } */ case 5:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Elem }; } $f.$ptr = $ptr; $f._12 = _12; $f._r = _r; $f.fl = fl; $f.k = k; $f.tt = tt; $f.typ = typ; $f.v = v; $f.val = val; $f.val$1 = val$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Elem = function() { return this.$val.Elem(); };
	Value.ptr.prototype.Field = function(i) {
		var $ptr, _r, _r$1, _r$2, field, fl, i, jsTag, o, prop, s, tag, tt, typ, v, x, x$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; field = $f.field; fl = $f.fl; i = $f.i; jsTag = $f.jsTag; o = $f.o; prop = $f.prop; s = $f.s; tag = $f.tag; tt = $f.tt; typ = $f.typ; v = $f.v; x = $f.x; x$1 = $f.x$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		jsTag = [jsTag];
		prop = [prop];
		s = [s];
		typ = [typ];
		v = this;
		new flag(v.flag).mustBe(25);
		tt = v.typ.kindType;
		if (i < 0 || i >= tt.fields.$length) {
			$panic(new $String("reflect: Field index out of range"));
		}
		prop[0] = $internalize(jsType(v.typ).fields[i].prop, $String);
		field = (x = tt.fields, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
		typ[0] = field.typ;
		fl = (((v.flag & 416) >>> 0) | (typ[0].Kind() >>> 0)) >>> 0;
		if (!(field.pkgPath === ptrType$6.nil)) {
			if (field.name === ptrType$6.nil) {
				fl = (fl | (64)) >>> 0;
			} else {
				fl = (fl | (32)) >>> 0;
			}
		}
		tag = (x$1 = tt.fields, ((i < 0 || i >= x$1.$length) ? $throwRuntimeError("index out of range") : x$1.$array[x$1.$offset + i])).tag;
		/* */ if (!(tag === ptrType$6.nil) && !((i === 0))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!(tag === ptrType$6.nil) && !((i === 0))) { */ case 1:
			jsTag[0] = getJsTag(tag.$get());
			/* */ if (!(jsTag[0] === "")) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!(jsTag[0] === "")) { */ case 3:
				/* while (true) { */ case 5:
					o = [o];
					_r = v.Field(0); /* */ $s = 7; case 7: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
					v = _r;
					/* */ if (v.typ === jsObjectPtr) { $s = 8; continue; }
					/* */ $s = 9; continue;
					/* if (v.typ === jsObjectPtr) { */ case 8:
						o[0] = v.object().object;
						return new Value.ptr(typ[0], new (jsType(PtrTo(typ[0])))((function(jsTag, o, prop, s, typ) { return function() {
							var $ptr;
							return $internalize(o[0][$externalize(jsTag[0], $String)], jsType(typ[0]));
						}; })(jsTag, o, prop, s, typ), (function(jsTag, o, prop, s, typ) { return function(x$2) {
							var $ptr, x$2;
							o[0][$externalize(jsTag[0], $String)] = $externalize(x$2, jsType(typ[0]));
						}; })(jsTag, o, prop, s, typ)), fl);
					/* } */ case 9:
					/* */ if (v.typ.Kind() === 22) { $s = 10; continue; }
					/* */ $s = 11; continue;
					/* if (v.typ.Kind() === 22) { */ case 10:
						_r$1 = v.Elem(); /* */ $s = 12; case 12: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
						v = _r$1;
					/* } */ case 11:
				/* } */ $s = 5; continue; case 6:
			/* } */ case 4:
		/* } */ case 2:
		s[0] = v.ptr;
		/* */ if (!((((fl & 128) >>> 0) === 0)) && !((typ[0].Kind() === 17)) && !((typ[0].Kind() === 25))) { $s = 13; continue; }
		/* */ $s = 14; continue;
		/* if (!((((fl & 128) >>> 0) === 0)) && !((typ[0].Kind() === 17)) && !((typ[0].Kind() === 25))) { */ case 13:
			return new Value.ptr(typ[0], new (jsType(PtrTo(typ[0])))((function(jsTag, prop, s, typ) { return function() {
				var $ptr;
				return wrapJsObject(typ[0], s[0][$externalize(prop[0], $String)]);
			}; })(jsTag, prop, s, typ), (function(jsTag, prop, s, typ) { return function(x$2) {
				var $ptr, x$2;
				s[0][$externalize(prop[0], $String)] = unwrapJsObject(typ[0], x$2);
			}; })(jsTag, prop, s, typ)), fl);
		/* } */ case 14:
		_r$2 = makeValue(typ[0], wrapJsObject(typ[0], s[0][$externalize(prop[0], $String)]), fl); /* */ $s = 15; case 15: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		/* */ $s = 16; case 16:
		return _r$2;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Field }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.field = field; $f.fl = fl; $f.i = i; $f.jsTag = jsTag; $f.o = o; $f.prop = prop; $f.s = s; $f.tag = tag; $f.tt = tt; $f.typ = typ; $f.v = v; $f.x = x; $f.x$1 = x$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Field = function(i) { return this.$val.Field(i); };
	getJsTag = function(tag) {
		var $ptr, _tuple, i, name, qvalue, tag, value;
		while (true) {
			if (!(!(tag === ""))) { break; }
			i = 0;
			while (true) {
				if (!(i < tag.length && (tag.charCodeAt(i) === 32))) { break; }
				i = i + (1) >> 0;
			}
			tag = tag.substring(i);
			if (tag === "") {
				break;
			}
			i = 0;
			while (true) {
				if (!(i < tag.length && !((tag.charCodeAt(i) === 32)) && !((tag.charCodeAt(i) === 58)) && !((tag.charCodeAt(i) === 34)))) { break; }
				i = i + (1) >> 0;
			}
			if ((i + 1 >> 0) >= tag.length || !((tag.charCodeAt(i) === 58)) || !((tag.charCodeAt((i + 1 >> 0)) === 34))) {
				break;
			}
			name = tag.substring(0, i);
			tag = tag.substring((i + 1 >> 0));
			i = 1;
			while (true) {
				if (!(i < tag.length && !((tag.charCodeAt(i) === 34)))) { break; }
				if (tag.charCodeAt(i) === 92) {
					i = i + (1) >> 0;
				}
				i = i + (1) >> 0;
			}
			if (i >= tag.length) {
				break;
			}
			qvalue = tag.substring(0, (i + 1 >> 0));
			tag = tag.substring((i + 1 >> 0));
			if (name === "js") {
				_tuple = strconv.Unquote(qvalue);
				value = _tuple[0];
				return value;
			}
		}
		return "";
	};
	Value.ptr.prototype.Index = function(i) {
		var $ptr, _13, _r, _r$1, a, a$1, c, fl, fl$1, fl$2, i, k, s, str, tt, tt$1, typ, typ$1, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _13 = $f._13; _r = $f._r; _r$1 = $f._r$1; a = $f.a; a$1 = $f.a$1; c = $f.c; fl = $f.fl; fl$1 = $f.fl$1; fl$2 = $f.fl$2; i = $f.i; k = $f.k; s = $f.s; str = $f.str; tt = $f.tt; tt$1 = $f.tt$1; typ = $f.typ; typ$1 = $f.typ$1; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		a = [a];
		a$1 = [a$1];
		c = [c];
		i = [i];
		typ = [typ];
		typ$1 = [typ$1];
		v = this;
			k = new flag(v.flag).kind();
			_13 = k;
			/* */ if (_13 === (17)) { $s = 2; continue; }
			/* */ if (_13 === (23)) { $s = 3; continue; }
			/* */ if (_13 === (24)) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if (_13 === (17)) { */ case 2:
				tt = v.typ.kindType;
				if (i[0] < 0 || i[0] > (tt.len >> 0)) {
					$panic(new $String("reflect: array index out of range"));
				}
				typ[0] = tt.elem;
				fl = (v.flag & 480) >>> 0;
				fl = (fl | ((typ[0].Kind() >>> 0))) >>> 0;
				a[0] = v.ptr;
				/* */ if (!((((fl & 128) >>> 0) === 0)) && !((typ[0].Kind() === 17)) && !((typ[0].Kind() === 25))) { $s = 7; continue; }
				/* */ $s = 8; continue;
				/* if (!((((fl & 128) >>> 0) === 0)) && !((typ[0].Kind() === 17)) && !((typ[0].Kind() === 25))) { */ case 7:
					return new Value.ptr(typ[0], new (jsType(PtrTo(typ[0])))((function(a, a$1, c, i, typ, typ$1) { return function() {
						var $ptr;
						return wrapJsObject(typ[0], a[0][i[0]]);
					}; })(a, a$1, c, i, typ, typ$1), (function(a, a$1, c, i, typ, typ$1) { return function(x) {
						var $ptr, x;
						a[0][i[0]] = unwrapJsObject(typ[0], x);
					}; })(a, a$1, c, i, typ, typ$1)), fl);
				/* } */ case 8:
				_r = makeValue(typ[0], wrapJsObject(typ[0], a[0][i[0]]), fl); /* */ $s = 9; case 9: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				/* */ $s = 10; case 10:
				return _r;
			/* } else if (_13 === (23)) { */ case 3:
				s = v.object();
				if (i[0] < 0 || i[0] >= ($parseInt(s.$length) >> 0)) {
					$panic(new $String("reflect: slice index out of range"));
				}
				tt$1 = v.typ.kindType;
				typ$1[0] = tt$1.elem;
				fl$1 = (384 | ((v.flag & 96) >>> 0)) >>> 0;
				fl$1 = (fl$1 | ((typ$1[0].Kind() >>> 0))) >>> 0;
				i[0] = i[0] + (($parseInt(s.$offset) >> 0)) >> 0;
				a$1[0] = s.$array;
				/* */ if (!((((fl$1 & 128) >>> 0) === 0)) && !((typ$1[0].Kind() === 17)) && !((typ$1[0].Kind() === 25))) { $s = 11; continue; }
				/* */ $s = 12; continue;
				/* if (!((((fl$1 & 128) >>> 0) === 0)) && !((typ$1[0].Kind() === 17)) && !((typ$1[0].Kind() === 25))) { */ case 11:
					return new Value.ptr(typ$1[0], new (jsType(PtrTo(typ$1[0])))((function(a, a$1, c, i, typ, typ$1) { return function() {
						var $ptr;
						return wrapJsObject(typ$1[0], a$1[0][i[0]]);
					}; })(a, a$1, c, i, typ, typ$1), (function(a, a$1, c, i, typ, typ$1) { return function(x) {
						var $ptr, x;
						a$1[0][i[0]] = unwrapJsObject(typ$1[0], x);
					}; })(a, a$1, c, i, typ, typ$1)), fl$1);
				/* } */ case 12:
				_r$1 = makeValue(typ$1[0], wrapJsObject(typ$1[0], a$1[0][i[0]]), fl$1); /* */ $s = 13; case 13: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				/* */ $s = 14; case 14:
				return _r$1;
			/* } else if (_13 === (24)) { */ case 4:
				str = v.ptr.$get();
				if (i[0] < 0 || i[0] >= str.length) {
					$panic(new $String("reflect: string index out of range"));
				}
				fl$2 = (((v.flag & 96) >>> 0) | 8) >>> 0;
				c[0] = str.charCodeAt(i[0]);
				return new Value.ptr(uint8Type, (c.$ptr || (c.$ptr = new ptrType$5(function() { return this.$target[0]; }, function($v) { this.$target[0] = $v; }, c))), (fl$2 | 128) >>> 0);
			/* } else { */ case 5:
				$panic(new ValueError.ptr("reflect.Value.Index", k));
			/* } */ case 6:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Index }; } $f.$ptr = $ptr; $f._13 = _13; $f._r = _r; $f._r$1 = _r$1; $f.a = a; $f.a$1 = a$1; $f.c = c; $f.fl = fl; $f.fl$1 = fl$1; $f.fl$2 = fl$2; $f.i = i; $f.k = k; $f.s = s; $f.str = str; $f.tt = tt; $f.tt$1 = tt$1; $f.typ = typ; $f.typ$1 = typ$1; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Index = function(i) { return this.$val.Index(i); };
	Value.ptr.prototype.InterfaceData = function() {
		var $ptr, v;
		v = this;
		$panic(errors.New("InterfaceData is not supported by GopherJS"));
	};
	Value.prototype.InterfaceData = function() { return this.$val.InterfaceData(); };
	Value.ptr.prototype.IsNil = function() {
		var $ptr, _14, k, v;
		v = this;
		k = new flag(v.flag).kind();
		_14 = k;
		if ((_14 === (22)) || (_14 === (23))) {
			return v.object() === jsType(v.typ).nil;
		} else if (_14 === (18)) {
			return v.object() === $chanNil;
		} else if (_14 === (19)) {
			return v.object() === $throwNilPointerError;
		} else if (_14 === (21)) {
			return v.object() === false;
		} else if (_14 === (20)) {
			return v.object() === $ifaceNil;
		} else {
			$panic(new ValueError.ptr("reflect.Value.IsNil", k));
		}
	};
	Value.prototype.IsNil = function() { return this.$val.IsNil(); };
	Value.ptr.prototype.Len = function() {
		var $ptr, _15, k, v;
		v = this;
		k = new flag(v.flag).kind();
		_15 = k;
		if ((_15 === (17)) || (_15 === (24))) {
			return $parseInt(v.object().length);
		} else if (_15 === (23)) {
			return $parseInt(v.object().$length) >> 0;
		} else if (_15 === (18)) {
			return $parseInt(v.object().$buffer.length) >> 0;
		} else if (_15 === (21)) {
			return $parseInt($keys(v.object()).length);
		} else {
			$panic(new ValueError.ptr("reflect.Value.Len", k));
		}
	};
	Value.prototype.Len = function() { return this.$val.Len(); };
	Value.ptr.prototype.Pointer = function() {
		var $ptr, _16, k, v;
		v = this;
		k = new flag(v.flag).kind();
		_16 = k;
		if ((_16 === (18)) || (_16 === (21)) || (_16 === (22)) || (_16 === (26))) {
			if (v.IsNil()) {
				return 0;
			}
			return v.object();
		} else if (_16 === (19)) {
			if (v.IsNil()) {
				return 0;
			}
			return 1;
		} else if (_16 === (23)) {
			if (v.IsNil()) {
				return 0;
			}
			return v.object().$array;
		} else {
			$panic(new ValueError.ptr("reflect.Value.Pointer", k));
		}
	};
	Value.prototype.Pointer = function() { return this.$val.Pointer(); };
	Value.ptr.prototype.Set = function(x) {
		var $ptr, _17, _r, _r$1, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _17 = $f._17; _r = $f._r; _r$1 = $f._r$1; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		x = x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(x.flag).mustBeExported();
		_r = x.assignTo("reflect.Set", v.typ, 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		x = _r;
		/* */ if (!((((v.flag & 128) >>> 0) === 0))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!((((v.flag & 128) >>> 0) === 0))) { */ case 2:
				_17 = v.typ.Kind();
				/* */ if (_17 === (17)) { $s = 5; continue; }
				/* */ if (_17 === (20)) { $s = 6; continue; }
				/* */ if (_17 === (25)) { $s = 7; continue; }
				/* */ $s = 8; continue;
				/* if (_17 === (17)) { */ case 5:
					jsType(v.typ).copy(v.ptr, x.ptr);
					$s = 9; continue;
				/* } else if (_17 === (20)) { */ case 6:
					_r$1 = valueInterface(x, false); /* */ $s = 10; case 10: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
					v.ptr.$set(_r$1);
					$s = 9; continue;
				/* } else if (_17 === (25)) { */ case 7:
					copyStruct(v.ptr, x.ptr, v.typ);
					$s = 9; continue;
				/* } else { */ case 8:
					v.ptr.$set(x.object());
				/* } */ case 9:
			case 4:
			return;
		/* } */ case 3:
		v.ptr = x.ptr;
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Set }; } $f.$ptr = $ptr; $f._17 = _17; $f._r = _r; $f._r$1 = _r$1; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Set = function(x) { return this.$val.Set(x); };
	Value.ptr.prototype.SetBytes = function(x) {
		var $ptr, _r, _r$1, _v, slice, typedSlice, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _v = $f._v; slice = $f.slice; typedSlice = $f.typedSlice; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(23);
		_r = v.typ.Elem().Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (!((_r === 8))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((_r === 8))) { */ case 1:
			$panic(new $String("reflect.Value.SetBytes of non-byte slice"));
		/* } */ case 2:
		slice = x;
		if (!(v.typ.Name() === "")) { _v = true; $s = 6; continue s; }
		_r$1 = v.typ.Elem().Name(); /* */ $s = 7; case 7: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_v = !(_r$1 === ""); case 6:
		/* */ if (_v) { $s = 4; continue; }
		/* */ $s = 5; continue;
		/* if (_v) { */ case 4:
			typedSlice = new (jsType(v.typ))(slice.$array);
			typedSlice.$offset = slice.$offset;
			typedSlice.$length = slice.$length;
			typedSlice.$capacity = slice.$capacity;
			slice = typedSlice;
		/* } */ case 5:
		v.ptr.$set(slice);
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.SetBytes }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._v = _v; $f.slice = slice; $f.typedSlice = typedSlice; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.SetBytes = function(x) { return this.$val.SetBytes(x); };
	Value.ptr.prototype.SetCap = function(n) {
		var $ptr, n, newSlice, s, v;
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(23);
		s = v.ptr.$get();
		if (n < ($parseInt(s.$length) >> 0) || n > ($parseInt(s.$capacity) >> 0)) {
			$panic(new $String("reflect: slice capacity out of range in SetCap"));
		}
		newSlice = new (jsType(v.typ))(s.$array);
		newSlice.$offset = s.$offset;
		newSlice.$length = s.$length;
		newSlice.$capacity = n;
		v.ptr.$set(newSlice);
	};
	Value.prototype.SetCap = function(n) { return this.$val.SetCap(n); };
	Value.ptr.prototype.SetLen = function(n) {
		var $ptr, n, newSlice, s, v;
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(23);
		s = v.ptr.$get();
		if (n < 0 || n > ($parseInt(s.$capacity) >> 0)) {
			$panic(new $String("reflect: slice length out of range in SetLen"));
		}
		newSlice = new (jsType(v.typ))(s.$array);
		newSlice.$offset = s.$offset;
		newSlice.$length = n;
		newSlice.$capacity = s.$capacity;
		v.ptr.$set(newSlice);
	};
	Value.prototype.SetLen = function(n) { return this.$val.SetLen(n); };
	Value.ptr.prototype.Slice = function(i, j) {
		var $ptr, _18, _r, _r$1, cap, i, j, kind, s, str, tt, typ, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _18 = $f._18; _r = $f._r; _r$1 = $f._r$1; cap = $f.cap; i = $f.i; j = $f.j; kind = $f.kind; s = $f.s; str = $f.str; tt = $f.tt; typ = $f.typ; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		cap = 0;
		typ = $ifaceNil;
		s = null;
			kind = new flag(v.flag).kind();
			_18 = kind;
			/* */ if (_18 === (17)) { $s = 2; continue; }
			/* */ if (_18 === (23)) { $s = 3; continue; }
			/* */ if (_18 === (24)) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if (_18 === (17)) { */ case 2:
				if (((v.flag & 256) >>> 0) === 0) {
					$panic(new $String("reflect.Value.Slice: slice of unaddressable array"));
				}
				tt = v.typ.kindType;
				cap = (tt.len >> 0);
				typ = SliceOf(tt.elem);
				s = new (jsType(typ))(v.object());
				$s = 6; continue;
			/* } else if (_18 === (23)) { */ case 3:
				typ = v.typ;
				s = v.object();
				cap = $parseInt(s.$capacity) >> 0;
				$s = 6; continue;
			/* } else if (_18 === (24)) { */ case 4:
				str = v.ptr.$get();
				if (i < 0 || j < i || j > str.length) {
					$panic(new $String("reflect.Value.Slice: string slice index out of bounds"));
				}
				_r = ValueOf(new $String(str.substring(i, j))); /* */ $s = 7; case 7: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				/* */ $s = 8; case 8:
				return _r;
			/* } else { */ case 5:
				$panic(new ValueError.ptr("reflect.Value.Slice", kind));
			/* } */ case 6:
		case 1:
		if (i < 0 || j < i || j > cap) {
			$panic(new $String("reflect.Value.Slice: slice index out of bounds"));
		}
		_r$1 = makeValue(typ, $subslice(s, i, j), (v.flag & 96) >>> 0); /* */ $s = 9; case 9: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 10; case 10:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Slice }; } $f.$ptr = $ptr; $f._18 = _18; $f._r = _r; $f._r$1 = _r$1; $f.cap = cap; $f.i = i; $f.j = j; $f.kind = kind; $f.s = s; $f.str = str; $f.tt = tt; $f.typ = typ; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Slice = function(i, j) { return this.$val.Slice(i, j); };
	Value.ptr.prototype.Slice3 = function(i, j, k) {
		var $ptr, _19, _r, cap, i, j, k, kind, s, tt, typ, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _19 = $f._19; _r = $f._r; cap = $f.cap; i = $f.i; j = $f.j; k = $f.k; kind = $f.kind; s = $f.s; tt = $f.tt; typ = $f.typ; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		cap = 0;
		typ = $ifaceNil;
		s = null;
		kind = new flag(v.flag).kind();
		_19 = kind;
		if (_19 === (17)) {
			if (((v.flag & 256) >>> 0) === 0) {
				$panic(new $String("reflect.Value.Slice: slice of unaddressable array"));
			}
			tt = v.typ.kindType;
			cap = (tt.len >> 0);
			typ = SliceOf(tt.elem);
			s = new (jsType(typ))(v.object());
		} else if (_19 === (23)) {
			typ = v.typ;
			s = v.object();
			cap = $parseInt(s.$capacity) >> 0;
		} else {
			$panic(new ValueError.ptr("reflect.Value.Slice3", kind));
		}
		if (i < 0 || j < i || k < j || k > cap) {
			$panic(new $String("reflect.Value.Slice3: slice index out of bounds"));
		}
		_r = makeValue(typ, $subslice(s, i, j, k), (v.flag & 96) >>> 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Slice3 }; } $f.$ptr = $ptr; $f._19 = _19; $f._r = _r; $f.cap = cap; $f.i = i; $f.j = j; $f.k = k; $f.kind = kind; $f.s = s; $f.tt = tt; $f.typ = typ; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Slice3 = function(i, j, k) { return this.$val.Slice3(i, j, k); };
	Value.ptr.prototype.Close = function() {
		var $ptr, v;
		v = this;
		new flag(v.flag).mustBe(18);
		new flag(v.flag).mustBeExported();
		$close(v.object());
	};
	Value.prototype.Close = function() { return this.$val.Close(); };
	chanrecv = function(t, ch, nb, val) {
		var $ptr, _r, _tmp, _tmp$1, _tmp$2, _tmp$3, ch, comms, nb, received, recvRes, selectRes, selected, t, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; ch = $f.ch; comms = $f.comms; nb = $f.nb; received = $f.received; recvRes = $f.recvRes; selectRes = $f.selectRes; selected = $f.selected; t = $f.t; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		selected = false;
		received = false;
		comms = new sliceType$9([new sliceType$7([ch])]);
		if (nb) {
			comms = $append(comms, new sliceType$7([]));
		}
		_r = selectHelper(new sliceType$2([comms])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		selectRes = _r;
		if (nb && (($parseInt(selectRes[0]) >> 0) === 1)) {
			_tmp = false;
			_tmp$1 = false;
			selected = _tmp;
			received = _tmp$1;
			return [selected, received];
		}
		recvRes = selectRes[1];
		val.$set(recvRes[0]);
		_tmp$2 = true;
		_tmp$3 = !!(recvRes[1]);
		selected = _tmp$2;
		received = _tmp$3;
		return [selected, received];
		/* */ } return; } if ($f === undefined) { $f = { $blk: chanrecv }; } $f.$ptr = $ptr; $f._r = _r; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f.ch = ch; $f.comms = comms; $f.nb = nb; $f.received = received; $f.recvRes = recvRes; $f.selectRes = selectRes; $f.selected = selected; $f.t = t; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	chansend = function(t, ch, val, nb) {
		var $ptr, _r, ch, comms, nb, selectRes, t, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; ch = $f.ch; comms = $f.comms; nb = $f.nb; selectRes = $f.selectRes; t = $f.t; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		comms = new sliceType$9([new sliceType$7([ch, val.$get()])]);
		if (nb) {
			comms = $append(comms, new sliceType$7([]));
		}
		_r = selectHelper(new sliceType$2([comms])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		selectRes = _r;
		if (nb && (($parseInt(selectRes[0]) >> 0) === 1)) {
			return false;
		}
		return true;
		/* */ } return; } if ($f === undefined) { $f = { $blk: chansend }; } $f.$ptr = $ptr; $f._r = _r; $f.ch = ch; $f.comms = comms; $f.nb = nb; $f.selectRes = selectRes; $f.t = t; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	Kind.prototype.String = function() {
		var $ptr, k;
		k = this.$val;
		if ((k >> 0) < kindNames.$length) {
			return ((k < 0 || k >= kindNames.$length) ? $throwRuntimeError("index out of range") : kindNames.$array[kindNames.$offset + k]);
		}
		return "kind" + strconv.Itoa((k >> 0));
	};
	$ptrType(Kind).prototype.String = function() { return new Kind(this.$get()).String(); };
	uncommonType.ptr.prototype.uncommon = function() {
		var $ptr, t;
		t = this;
		return t;
	};
	uncommonType.prototype.uncommon = function() { return this.$val.uncommon(); };
	uncommonType.ptr.prototype.PkgPath = function() {
		var $ptr, t;
		t = this;
		if (t === ptrType$7.nil || t.pkgPath === ptrType$6.nil) {
			return "";
		}
		return t.pkgPath.$get();
	};
	uncommonType.prototype.PkgPath = function() { return this.$val.PkgPath(); };
	uncommonType.ptr.prototype.Name = function() {
		var $ptr, t;
		t = this;
		if (t === ptrType$7.nil || t.name === ptrType$6.nil) {
			return "";
		}
		return t.name.$get();
	};
	uncommonType.prototype.Name = function() { return this.$val.Name(); };
	rtype.ptr.prototype.String = function() {
		var $ptr, t;
		t = this;
		return t.string.$get();
	};
	rtype.prototype.String = function() { return this.$val.String(); };
	rtype.ptr.prototype.Size = function() {
		var $ptr, t;
		t = this;
		return t.size;
	};
	rtype.prototype.Size = function() { return this.$val.Size(); };
	rtype.ptr.prototype.Bits = function() {
		var $ptr, k, t;
		t = this;
		if (t === ptrType$1.nil) {
			$panic(new $String("reflect: Bits of nil Type"));
		}
		k = t.Kind();
		if (k < 2 || k > 16) {
			$panic(new $String("reflect: Bits of non-arithmetic Type " + t.String()));
		}
		return $imul((t.size >> 0), 8);
	};
	rtype.prototype.Bits = function() { return this.$val.Bits(); };
	rtype.ptr.prototype.Align = function() {
		var $ptr, t;
		t = this;
		return (t.align >> 0);
	};
	rtype.prototype.Align = function() { return this.$val.Align(); };
	rtype.ptr.prototype.FieldAlign = function() {
		var $ptr, t;
		t = this;
		return (t.fieldAlign >> 0);
	};
	rtype.prototype.FieldAlign = function() { return this.$val.FieldAlign(); };
	rtype.ptr.prototype.Kind = function() {
		var $ptr, t;
		t = this;
		return (((t.kind & 31) >>> 0) >>> 0);
	};
	rtype.prototype.Kind = function() { return this.$val.Kind(); };
	rtype.ptr.prototype.common = function() {
		var $ptr, t;
		t = this;
		return t;
	};
	rtype.prototype.common = function() { return this.$val.common(); };
	uncommonType.ptr.prototype.NumMethod = function() {
		var $ptr, t;
		t = this;
		if (t === ptrType$7.nil) {
			return 0;
		}
		return t.methods.$length;
	};
	uncommonType.prototype.NumMethod = function() { return this.$val.NumMethod(); };
	uncommonType.ptr.prototype.MethodByName = function(name) {
		var $ptr, _i, _ref, _tmp, _tmp$1, i, m, name, ok, p, t, x;
		m = new Method.ptr("", "", $ifaceNil, new Value.ptr(ptrType$1.nil, 0, 0), 0);
		ok = false;
		t = this;
		if (t === ptrType$7.nil) {
			return [m, ok];
		}
		p = ptrType$10.nil;
		_ref = t.methods;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			p = (x = t.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
			if (!(p.name === ptrType$6.nil) && p.name.$get() === name) {
				_tmp = $clone(t.Method(i), Method);
				_tmp$1 = true;
				Method.copy(m, _tmp);
				ok = _tmp$1;
				return [m, ok];
			}
			_i++;
		}
		return [m, ok];
	};
	uncommonType.prototype.MethodByName = function(name) { return this.$val.MethodByName(name); };
	rtype.ptr.prototype.NumMethod = function() {
		var $ptr, t, tt;
		t = this;
		if (t.Kind() === 20) {
			tt = t.kindType;
			return tt.NumMethod();
		}
		return t.uncommonType.NumMethod();
	};
	rtype.prototype.NumMethod = function() { return this.$val.NumMethod(); };
	rtype.ptr.prototype.Method = function(i) {
		var $ptr, i, m, t, tt;
		m = new Method.ptr("", "", $ifaceNil, new Value.ptr(ptrType$1.nil, 0, 0), 0);
		t = this;
		if (t.Kind() === 20) {
			tt = t.kindType;
			Method.copy(m, tt.Method(i));
			return m;
		}
		Method.copy(m, t.uncommonType.Method(i));
		return m;
	};
	rtype.prototype.Method = function(i) { return this.$val.Method(i); };
	rtype.ptr.prototype.MethodByName = function(name) {
		var $ptr, _tuple, _tuple$1, m, name, ok, t, tt;
		m = new Method.ptr("", "", $ifaceNil, new Value.ptr(ptrType$1.nil, 0, 0), 0);
		ok = false;
		t = this;
		if (t.Kind() === 20) {
			tt = t.kindType;
			_tuple = tt.MethodByName(name);
			Method.copy(m, _tuple[0]);
			ok = _tuple[1];
			return [m, ok];
		}
		_tuple$1 = t.uncommonType.MethodByName(name);
		Method.copy(m, _tuple$1[0]);
		ok = _tuple$1[1];
		return [m, ok];
	};
	rtype.prototype.MethodByName = function(name) { return this.$val.MethodByName(name); };
	rtype.ptr.prototype.PkgPath = function() {
		var $ptr, t;
		t = this;
		return t.uncommonType.PkgPath();
	};
	rtype.prototype.PkgPath = function() { return this.$val.PkgPath(); };
	rtype.ptr.prototype.Name = function() {
		var $ptr, t;
		t = this;
		return t.uncommonType.Name();
	};
	rtype.prototype.Name = function() { return this.$val.Name(); };
	rtype.ptr.prototype.ChanDir = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 18))) {
			$panic(new $String("reflect: ChanDir of non-chan type"));
		}
		tt = t.kindType;
		return (tt.dir >> 0);
	};
	rtype.prototype.ChanDir = function() { return this.$val.ChanDir(); };
	rtype.ptr.prototype.IsVariadic = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 19))) {
			$panic(new $String("reflect: IsVariadic of non-func type"));
		}
		tt = t.kindType;
		return tt.dotdotdot;
	};
	rtype.prototype.IsVariadic = function() { return this.$val.IsVariadic(); };
	rtype.ptr.prototype.Elem = function() {
		var $ptr, _1, t, tt, tt$1, tt$2, tt$3, tt$4;
		t = this;
		_1 = t.Kind();
		if (_1 === (17)) {
			tt = t.kindType;
			return toType(tt.elem);
		} else if (_1 === (18)) {
			tt$1 = t.kindType;
			return toType(tt$1.elem);
		} else if (_1 === (21)) {
			tt$2 = t.kindType;
			return toType(tt$2.elem);
		} else if (_1 === (22)) {
			tt$3 = t.kindType;
			return toType(tt$3.elem);
		} else if (_1 === (23)) {
			tt$4 = t.kindType;
			return toType(tt$4.elem);
		}
		$panic(new $String("reflect: Elem of invalid type"));
	};
	rtype.prototype.Elem = function() { return this.$val.Elem(); };
	rtype.ptr.prototype.Field = function(i) {
		var $ptr, _r, i, t, tt, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; i = $f.i; t = $f.t; tt = $f.tt; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		if (!((t.Kind() === 25))) {
			$panic(new $String("reflect: Field of non-struct type"));
		}
		tt = t.kindType;
		_r = tt.Field(i); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.Field }; } $f.$ptr = $ptr; $f._r = _r; $f.i = i; $f.t = t; $f.tt = tt; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.Field = function(i) { return this.$val.Field(i); };
	rtype.ptr.prototype.FieldByIndex = function(index) {
		var $ptr, _r, index, t, tt, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; index = $f.index; t = $f.t; tt = $f.tt; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		if (!((t.Kind() === 25))) {
			$panic(new $String("reflect: FieldByIndex of non-struct type"));
		}
		tt = t.kindType;
		_r = tt.FieldByIndex(index); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.FieldByIndex }; } $f.$ptr = $ptr; $f._r = _r; $f.index = index; $f.t = t; $f.tt = tt; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.FieldByIndex = function(index) { return this.$val.FieldByIndex(index); };
	rtype.ptr.prototype.FieldByName = function(name) {
		var $ptr, _r, name, t, tt, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; name = $f.name; t = $f.t; tt = $f.tt; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		if (!((t.Kind() === 25))) {
			$panic(new $String("reflect: FieldByName of non-struct type"));
		}
		tt = t.kindType;
		_r = tt.FieldByName(name); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.FieldByName }; } $f.$ptr = $ptr; $f._r = _r; $f.name = name; $f.t = t; $f.tt = tt; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.FieldByName = function(name) { return this.$val.FieldByName(name); };
	rtype.ptr.prototype.FieldByNameFunc = function(match) {
		var $ptr, _r, match, t, tt, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; match = $f.match; t = $f.t; tt = $f.tt; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		if (!((t.Kind() === 25))) {
			$panic(new $String("reflect: FieldByNameFunc of non-struct type"));
		}
		tt = t.kindType;
		_r = tt.FieldByNameFunc(match); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.FieldByNameFunc }; } $f.$ptr = $ptr; $f._r = _r; $f.match = match; $f.t = t; $f.tt = tt; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.FieldByNameFunc = function(match) { return this.$val.FieldByNameFunc(match); };
	rtype.ptr.prototype.In = function(i) {
		var $ptr, i, t, tt, x;
		t = this;
		if (!((t.Kind() === 19))) {
			$panic(new $String("reflect: In of non-func type"));
		}
		tt = t.kindType;
		return toType((x = tt.in$2, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i])));
	};
	rtype.prototype.In = function(i) { return this.$val.In(i); };
	rtype.ptr.prototype.Key = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 21))) {
			$panic(new $String("reflect: Key of non-map type"));
		}
		tt = t.kindType;
		return toType(tt.key);
	};
	rtype.prototype.Key = function() { return this.$val.Key(); };
	rtype.ptr.prototype.Len = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 17))) {
			$panic(new $String("reflect: Len of non-array type"));
		}
		tt = t.kindType;
		return (tt.len >> 0);
	};
	rtype.prototype.Len = function() { return this.$val.Len(); };
	rtype.ptr.prototype.NumField = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 25))) {
			$panic(new $String("reflect: NumField of non-struct type"));
		}
		tt = t.kindType;
		return tt.fields.$length;
	};
	rtype.prototype.NumField = function() { return this.$val.NumField(); };
	rtype.ptr.prototype.NumIn = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 19))) {
			$panic(new $String("reflect: NumIn of non-func type"));
		}
		tt = t.kindType;
		return tt.in$2.$length;
	};
	rtype.prototype.NumIn = function() { return this.$val.NumIn(); };
	rtype.ptr.prototype.NumOut = function() {
		var $ptr, t, tt;
		t = this;
		if (!((t.Kind() === 19))) {
			$panic(new $String("reflect: NumOut of non-func type"));
		}
		tt = t.kindType;
		return tt.out.$length;
	};
	rtype.prototype.NumOut = function() { return this.$val.NumOut(); };
	rtype.ptr.prototype.Out = function(i) {
		var $ptr, i, t, tt, x;
		t = this;
		if (!((t.Kind() === 19))) {
			$panic(new $String("reflect: Out of non-func type"));
		}
		tt = t.kindType;
		return toType((x = tt.out, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i])));
	};
	rtype.prototype.Out = function(i) { return this.$val.Out(i); };
	ChanDir.prototype.String = function() {
		var $ptr, _2, d;
		d = this.$val;
		_2 = d;
		if (_2 === (2)) {
			return "chan<-";
		} else if (_2 === (1)) {
			return "<-chan";
		} else if (_2 === (3)) {
			return "chan";
		}
		return "ChanDir" + strconv.Itoa((d >> 0));
	};
	$ptrType(ChanDir).prototype.String = function() { return new ChanDir(this.$get()).String(); };
	interfaceType.ptr.prototype.Method = function(i) {
		var $ptr, i, m, p, t, x;
		m = new Method.ptr("", "", $ifaceNil, new Value.ptr(ptrType$1.nil, 0, 0), 0);
		t = this;
		if (i < 0 || i >= t.methods.$length) {
			return m;
		}
		p = (x = t.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
		m.Name = p.name.$get();
		if (!(p.pkgPath === ptrType$6.nil)) {
			m.PkgPath = p.pkgPath.$get();
		}
		m.Type = toType(p.typ);
		m.Index = i;
		return m;
	};
	interfaceType.prototype.Method = function(i) { return this.$val.Method(i); };
	interfaceType.ptr.prototype.NumMethod = function() {
		var $ptr, t;
		t = this;
		return t.methods.$length;
	};
	interfaceType.prototype.NumMethod = function() { return this.$val.NumMethod(); };
	interfaceType.ptr.prototype.MethodByName = function(name) {
		var $ptr, _i, _ref, _tmp, _tmp$1, i, m, name, ok, p, t, x;
		m = new Method.ptr("", "", $ifaceNil, new Value.ptr(ptrType$1.nil, 0, 0), 0);
		ok = false;
		t = this;
		if (t === ptrType$11.nil) {
			return [m, ok];
		}
		p = ptrType$12.nil;
		_ref = t.methods;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			p = (x = t.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
			if (p.name.$get() === name) {
				_tmp = $clone(t.Method(i), Method);
				_tmp$1 = true;
				Method.copy(m, _tmp);
				ok = _tmp$1;
				return [m, ok];
			}
			_i++;
		}
		return [m, ok];
	};
	interfaceType.prototype.MethodByName = function(name) { return this.$val.MethodByName(name); };
	StructTag.prototype.Get = function(key) {
		var $ptr, _tuple, err, i, key, name, qvalue, tag, value;
		tag = this.$val;
		while (true) {
			if (!(!(tag === ""))) { break; }
			i = 0;
			while (true) {
				if (!(i < tag.length && (tag.charCodeAt(i) === 32))) { break; }
				i = i + (1) >> 0;
			}
			tag = tag.substring(i);
			if (tag === "") {
				break;
			}
			i = 0;
			while (true) {
				if (!(i < tag.length && tag.charCodeAt(i) > 32 && !((tag.charCodeAt(i) === 58)) && !((tag.charCodeAt(i) === 34)) && !((tag.charCodeAt(i) === 127)))) { break; }
				i = i + (1) >> 0;
			}
			if ((i === 0) || (i + 1 >> 0) >= tag.length || !((tag.charCodeAt(i) === 58)) || !((tag.charCodeAt((i + 1 >> 0)) === 34))) {
				break;
			}
			name = tag.substring(0, i);
			tag = tag.substring((i + 1 >> 0));
			i = 1;
			while (true) {
				if (!(i < tag.length && !((tag.charCodeAt(i) === 34)))) { break; }
				if (tag.charCodeAt(i) === 92) {
					i = i + (1) >> 0;
				}
				i = i + (1) >> 0;
			}
			if (i >= tag.length) {
				break;
			}
			qvalue = tag.substring(0, (i + 1 >> 0));
			tag = tag.substring((i + 1 >> 0));
			if (key === name) {
				_tuple = strconv.Unquote(qvalue);
				value = _tuple[0];
				err = _tuple[1];
				if (!($interfaceIsEqual(err, $ifaceNil))) {
					break;
				}
				return value;
			}
		}
		return "";
	};
	$ptrType(StructTag).prototype.Get = function(key) { return new StructTag(this.$get()).Get(key); };
	structType.ptr.prototype.Field = function(i) {
		var $ptr, _r, _r$1, _r$2, f, i, p, t, t$1, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; f = $f.f; i = $f.i; p = $f.p; t = $f.t; t$1 = $f.t$1; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		f = new StructField.ptr("", "", $ifaceNil, "", 0, sliceType$11.nil, false);
		t = this;
		if (i < 0 || i >= t.fields.$length) {
			return f;
		}
		p = (x = t.fields, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
		f.Type = toType(p.typ);
		/* */ if (!(p.name === ptrType$6.nil)) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!(p.name === ptrType$6.nil)) { */ case 1:
			f.Name = p.name.$get();
			$s = 3; continue;
		/* } else { */ case 2:
			t$1 = f.Type;
			_r = t$1.Kind(); /* */ $s = 6; case 6: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			/* */ if (_r === 22) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if (_r === 22) { */ case 4:
				_r$1 = t$1.Elem(); /* */ $s = 7; case 7: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				t$1 = _r$1;
			/* } */ case 5:
			_r$2 = t$1.Name(); /* */ $s = 8; case 8: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			f.Name = _r$2;
			f.Anonymous = true;
		/* } */ case 3:
		if (!(p.pkgPath === ptrType$6.nil)) {
			f.PkgPath = p.pkgPath.$get();
		}
		if (!(p.tag === ptrType$6.nil)) {
			f.Tag = p.tag.$get();
		}
		f.Offset = p.offset;
		f.Index = new sliceType$11([i]);
		return f;
		/* */ } return; } if ($f === undefined) { $f = { $blk: structType.ptr.prototype.Field }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.f = f; $f.i = i; $f.p = p; $f.t = t; $f.t$1 = t$1; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	structType.prototype.Field = function(i) { return this.$val.Field(i); };
	structType.ptr.prototype.FieldByIndex = function(index) {
		var $ptr, _i, _r, _r$1, _r$2, _r$3, _r$4, _ref, _v, f, ft, i, index, t, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _i = $f._i; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _ref = $f._ref; _v = $f._v; f = $f.f; ft = $f.ft; i = $f.i; index = $f.index; t = $f.t; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		f = new StructField.ptr("", "", $ifaceNil, "", 0, sliceType$11.nil, false);
		t = this;
		f.Type = toType(t.rtype);
		_ref = index;
		_i = 0;
		/* while (true) { */ case 1:
			/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 2; continue; }
			i = _i;
			x = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			/* */ if (i > 0) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (i > 0) { */ case 3:
				ft = f.Type;
				_r = ft.Kind(); /* */ $s = 8; case 8: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				if (!(_r === 22)) { _v = false; $s = 7; continue s; }
				_r$1 = ft.Elem(); /* */ $s = 9; case 9: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				_r$2 = _r$1.Kind(); /* */ $s = 10; case 10: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_v = _r$2 === 25; case 7:
				/* */ if (_v) { $s = 5; continue; }
				/* */ $s = 6; continue;
				/* if (_v) { */ case 5:
					_r$3 = ft.Elem(); /* */ $s = 11; case 11: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
					ft = _r$3;
				/* } */ case 6:
				f.Type = ft;
			/* } */ case 4:
			_r$4 = f.Type.Field(x); /* */ $s = 12; case 12: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			StructField.copy(f, _r$4);
			_i++;
		/* } */ $s = 1; continue; case 2:
		return f;
		/* */ } return; } if ($f === undefined) { $f = { $blk: structType.ptr.prototype.FieldByIndex }; } $f.$ptr = $ptr; $f._i = _i; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._ref = _ref; $f._v = _v; $f.f = f; $f.ft = ft; $f.i = i; $f.index = index; $f.t = t; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	structType.prototype.FieldByIndex = function(index) { return this.$val.FieldByIndex(index); };
	structType.ptr.prototype.FieldByNameFunc = function(match) {
		var $ptr, _entry, _entry$1, _entry$2, _entry$3, _i, _i$1, _key, _key$1, _key$2, _key$3, _r, _r$1, _r$2, _ref, _ref$1, _tmp, _tmp$1, _tmp$2, _tmp$3, count, current, f, fname, i, index, match, next, nextCount, ntyp, ok, result, scan, styp, t, t$1, visited, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _entry = $f._entry; _entry$1 = $f._entry$1; _entry$2 = $f._entry$2; _entry$3 = $f._entry$3; _i = $f._i; _i$1 = $f._i$1; _key = $f._key; _key$1 = $f._key$1; _key$2 = $f._key$2; _key$3 = $f._key$3; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _ref = $f._ref; _ref$1 = $f._ref$1; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; count = $f.count; current = $f.current; f = $f.f; fname = $f.fname; i = $f.i; index = $f.index; match = $f.match; next = $f.next; nextCount = $f.nextCount; ntyp = $f.ntyp; ok = $f.ok; result = $f.result; scan = $f.scan; styp = $f.styp; t = $f.t; t$1 = $f.t$1; visited = $f.visited; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		result = new StructField.ptr("", "", $ifaceNil, "", 0, sliceType$11.nil, false);
		ok = false;
		t = this;
		current = new sliceType$12([]);
		next = new sliceType$12([new fieldScan.ptr(t, sliceType$11.nil)]);
		nextCount = false;
		visited = $makeMap(ptrType$13.keyFor, []);
		/* while (true) { */ case 1:
			/* if (!(next.$length > 0)) { break; } */ if(!(next.$length > 0)) { $s = 2; continue; }
			_tmp = next;
			_tmp$1 = $subslice(current, 0, 0);
			current = _tmp;
			next = _tmp$1;
			count = nextCount;
			nextCount = false;
			_ref = current;
			_i = 0;
			/* while (true) { */ case 3:
				/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 4; continue; }
				scan = $clone(((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]), fieldScan);
				t$1 = scan.typ;
				/* */ if ((_entry = visited[ptrType$13.keyFor(t$1)], _entry !== undefined ? _entry.v : false)) { $s = 5; continue; }
				/* */ $s = 6; continue;
				/* if ((_entry = visited[ptrType$13.keyFor(t$1)], _entry !== undefined ? _entry.v : false)) { */ case 5:
					_i++;
					/* continue; */ $s = 3; continue;
				/* } */ case 6:
				_key = t$1; (visited || $throwRuntimeError("assignment to entry in nil map"))[ptrType$13.keyFor(_key)] = { k: _key, v: true };
				_ref$1 = t$1.fields;
				_i$1 = 0;
				/* while (true) { */ case 7:
					/* if (!(_i$1 < _ref$1.$length)) { break; } */ if(!(_i$1 < _ref$1.$length)) { $s = 8; continue; }
					i = _i$1;
					f = (x = t$1.fields, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
					fname = "";
					ntyp = ptrType$1.nil;
					/* */ if (!(f.name === ptrType$6.nil)) { $s = 9; continue; }
					/* */ $s = 10; continue;
					/* if (!(f.name === ptrType$6.nil)) { */ case 9:
						fname = f.name.$get();
						$s = 11; continue;
					/* } else { */ case 10:
						ntyp = f.typ;
						/* */ if (ntyp.Kind() === 22) { $s = 12; continue; }
						/* */ $s = 13; continue;
						/* if (ntyp.Kind() === 22) { */ case 12:
							_r = ntyp.Elem().common(); /* */ $s = 14; case 14: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
							ntyp = _r;
						/* } */ case 13:
						fname = ntyp.Name();
					/* } */ case 11:
					_r$1 = match(fname); /* */ $s = 17; case 17: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
					/* */ if (_r$1) { $s = 15; continue; }
					/* */ $s = 16; continue;
					/* if (_r$1) { */ case 15:
						if ((_entry$1 = count[ptrType$13.keyFor(t$1)], _entry$1 !== undefined ? _entry$1.v : 0) > 1 || ok) {
							_tmp$2 = new StructField.ptr("", "", $ifaceNil, "", 0, sliceType$11.nil, false);
							_tmp$3 = false;
							StructField.copy(result, _tmp$2);
							ok = _tmp$3;
							return [result, ok];
						}
						_r$2 = t$1.Field(i); /* */ $s = 18; case 18: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
						StructField.copy(result, _r$2);
						result.Index = sliceType$11.nil;
						result.Index = $appendSlice(result.Index, scan.index);
						result.Index = $append(result.Index, i);
						ok = true;
						_i$1++;
						/* continue; */ $s = 7; continue;
					/* } */ case 16:
					if (ok || ntyp === ptrType$1.nil || !((ntyp.Kind() === 25))) {
						_i$1++;
						/* continue; */ $s = 7; continue;
					}
					styp = ntyp.kindType;
					if ((_entry$2 = nextCount[ptrType$13.keyFor(styp)], _entry$2 !== undefined ? _entry$2.v : 0) > 0) {
						_key$1 = styp; (nextCount || $throwRuntimeError("assignment to entry in nil map"))[ptrType$13.keyFor(_key$1)] = { k: _key$1, v: 2 };
						_i$1++;
						/* continue; */ $s = 7; continue;
					}
					if (nextCount === false) {
						nextCount = $makeMap(ptrType$13.keyFor, []);
					}
					_key$2 = styp; (nextCount || $throwRuntimeError("assignment to entry in nil map"))[ptrType$13.keyFor(_key$2)] = { k: _key$2, v: 1 };
					if ((_entry$3 = count[ptrType$13.keyFor(t$1)], _entry$3 !== undefined ? _entry$3.v : 0) > 1) {
						_key$3 = styp; (nextCount || $throwRuntimeError("assignment to entry in nil map"))[ptrType$13.keyFor(_key$3)] = { k: _key$3, v: 2 };
					}
					index = sliceType$11.nil;
					index = $appendSlice(index, scan.index);
					index = $append(index, i);
					next = $append(next, new fieldScan.ptr(styp, index));
					_i$1++;
				/* } */ $s = 7; continue; case 8:
				_i++;
			/* } */ $s = 3; continue; case 4:
			if (ok) {
				/* break; */ $s = 2; continue;
			}
		/* } */ $s = 1; continue; case 2:
		return [result, ok];
		/* */ } return; } if ($f === undefined) { $f = { $blk: structType.ptr.prototype.FieldByNameFunc }; } $f.$ptr = $ptr; $f._entry = _entry; $f._entry$1 = _entry$1; $f._entry$2 = _entry$2; $f._entry$3 = _entry$3; $f._i = _i; $f._i$1 = _i$1; $f._key = _key; $f._key$1 = _key$1; $f._key$2 = _key$2; $f._key$3 = _key$3; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._ref = _ref; $f._ref$1 = _ref$1; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f.count = count; $f.current = current; $f.f = f; $f.fname = fname; $f.i = i; $f.index = index; $f.match = match; $f.next = next; $f.nextCount = nextCount; $f.ntyp = ntyp; $f.ok = ok; $f.result = result; $f.scan = scan; $f.styp = styp; $f.t = t; $f.t$1 = t$1; $f.visited = visited; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	structType.prototype.FieldByNameFunc = function(match) { return this.$val.FieldByNameFunc(match); };
	structType.ptr.prototype.FieldByName = function(name) {
		var $ptr, _i, _r, _r$1, _ref, _tmp, _tmp$1, _tuple, f, hasAnon, i, name, present, t, tf, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _i = $f._i; _r = $f._r; _r$1 = $f._r$1; _ref = $f._ref; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tuple = $f._tuple; f = $f.f; hasAnon = $f.hasAnon; i = $f.i; name = $f.name; present = $f.present; t = $f.t; tf = $f.tf; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		name = [name];
		f = new StructField.ptr("", "", $ifaceNil, "", 0, sliceType$11.nil, false);
		present = false;
		t = this;
		hasAnon = false;
		/* */ if (!(name[0] === "")) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!(name[0] === "")) { */ case 1:
			_ref = t.fields;
			_i = 0;
			/* while (true) { */ case 3:
				/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 4; continue; }
				i = _i;
				tf = (x = t.fields, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
				/* */ if (tf.name === ptrType$6.nil) { $s = 5; continue; }
				/* */ $s = 6; continue;
				/* if (tf.name === ptrType$6.nil) { */ case 5:
					hasAnon = true;
					_i++;
					/* continue; */ $s = 3; continue;
				/* } */ case 6:
				/* */ if (tf.name.$get() === name[0]) { $s = 7; continue; }
				/* */ $s = 8; continue;
				/* if (tf.name.$get() === name[0]) { */ case 7:
					_r = t.Field(i); /* */ $s = 9; case 9: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
					_tmp = $clone(_r, StructField);
					_tmp$1 = true;
					StructField.copy(f, _tmp);
					present = _tmp$1;
					/* */ $s = 10; case 10:
					return [f, present];
				/* } */ case 8:
				_i++;
			/* } */ $s = 3; continue; case 4:
		/* } */ case 2:
		if (!hasAnon) {
			return [f, present];
		}
		_r$1 = t.FieldByNameFunc((function(name) { return function(s) {
			var $ptr, s;
			return s === name[0];
		}; })(name)); /* */ $s = 11; case 11: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_tuple = _r$1;
		StructField.copy(f, _tuple[0]);
		present = _tuple[1];
		/* */ $s = 12; case 12:
		return [f, present];
		/* */ } return; } if ($f === undefined) { $f = { $blk: structType.ptr.prototype.FieldByName }; } $f.$ptr = $ptr; $f._i = _i; $f._r = _r; $f._r$1 = _r$1; $f._ref = _ref; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tuple = _tuple; $f.f = f; $f.hasAnon = hasAnon; $f.i = i; $f.name = name; $f.present = present; $f.t = t; $f.tf = tf; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	structType.prototype.FieldByName = function(name) { return this.$val.FieldByName(name); };
	PtrTo = function(t) {
		var $ptr, t;
		return $assertType(t, ptrType$1).ptrTo();
	};
	$pkg.PtrTo = PtrTo;
	rtype.ptr.prototype.Implements = function(u) {
		var $ptr, _r, t, u, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; u = $f.u; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		if ($interfaceIsEqual(u, $ifaceNil)) {
			$panic(new $String("reflect: nil type passed to Type.Implements"));
		}
		_r = u.Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (!((_r === 20))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((_r === 20))) { */ case 1:
			$panic(new $String("reflect: non-interface type passed to Type.Implements"));
		/* } */ case 2:
		return implements$1($assertType(u, ptrType$1), t);
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.Implements }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.u = u; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.Implements = function(u) { return this.$val.Implements(u); };
	rtype.ptr.prototype.AssignableTo = function(u) {
		var $ptr, t, u, uu;
		t = this;
		if ($interfaceIsEqual(u, $ifaceNil)) {
			$panic(new $String("reflect: nil type passed to Type.AssignableTo"));
		}
		uu = $assertType(u, ptrType$1);
		return directlyAssignable(uu, t) || implements$1(uu, t);
	};
	rtype.prototype.AssignableTo = function(u) { return this.$val.AssignableTo(u); };
	rtype.ptr.prototype.ConvertibleTo = function(u) {
		var $ptr, _r, t, u, uu, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; u = $f.u; uu = $f.uu; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		t = this;
		if ($interfaceIsEqual(u, $ifaceNil)) {
			$panic(new $String("reflect: nil type passed to Type.ConvertibleTo"));
		}
		uu = $assertType(u, ptrType$1);
		_r = convertOp(uu, t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return !(_r === $throwNilPointerError);
		/* */ } return; } if ($f === undefined) { $f = { $blk: rtype.ptr.prototype.ConvertibleTo }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.u = u; $f.uu = uu; $f.$s = $s; $f.$r = $r; return $f;
	};
	rtype.prototype.ConvertibleTo = function(u) { return this.$val.ConvertibleTo(u); };
	implements$1 = function(T, V) {
		var $ptr, T, V, i, i$1, j, j$1, t, tm, tm$1, v, v$1, vm, vm$1, x, x$1, x$2, x$3;
		if (!((T.Kind() === 20))) {
			return false;
		}
		t = T.kindType;
		if (t.methods.$length === 0) {
			return true;
		}
		if (V.Kind() === 20) {
			v = V.kindType;
			i = 0;
			j = 0;
			while (true) {
				if (!(j < v.methods.$length)) { break; }
				tm = (x = t.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
				vm = (x$1 = v.methods, ((j < 0 || j >= x$1.$length) ? $throwRuntimeError("index out of range") : x$1.$array[x$1.$offset + j]));
				if (vm.name.$get() === tm.name.$get() && vm.pkgPath === tm.pkgPath && vm.typ === tm.typ) {
					i = i + (1) >> 0;
					if (i >= t.methods.$length) {
						return true;
					}
				}
				j = j + (1) >> 0;
			}
			return false;
		}
		v$1 = V.uncommonType.uncommon();
		if (v$1 === ptrType$7.nil) {
			return false;
		}
		i$1 = 0;
		j$1 = 0;
		while (true) {
			if (!(j$1 < v$1.methods.$length)) { break; }
			tm$1 = (x$2 = t.methods, ((i$1 < 0 || i$1 >= x$2.$length) ? $throwRuntimeError("index out of range") : x$2.$array[x$2.$offset + i$1]));
			vm$1 = (x$3 = v$1.methods, ((j$1 < 0 || j$1 >= x$3.$length) ? $throwRuntimeError("index out of range") : x$3.$array[x$3.$offset + j$1]));
			if (vm$1.name.$get() === tm$1.name.$get() && vm$1.pkgPath === tm$1.pkgPath && vm$1.mtyp === tm$1.typ) {
				i$1 = i$1 + (1) >> 0;
				if (i$1 >= t.methods.$length) {
					return true;
				}
			}
			j$1 = j$1 + (1) >> 0;
		}
		return false;
	};
	directlyAssignable = function(T, V) {
		var $ptr, T, V;
		if (T === V) {
			return true;
		}
		if (!(T.Name() === "") && !(V.Name() === "") || !((T.Kind() === V.Kind()))) {
			return false;
		}
		return haveIdenticalUnderlyingType(T, V);
	};
	haveIdenticalUnderlyingType = function(T, V) {
		var $ptr, T, V, _3, _i, _i$1, _i$2, _ref, _ref$1, _ref$2, i, i$1, i$2, kind, t, t$1, t$2, tf, typ, typ$1, v, v$1, v$2, vf, x, x$1, x$2, x$3;
		if (T === V) {
			return true;
		}
		kind = T.Kind();
		if (!((kind === V.Kind()))) {
			return false;
		}
		if (1 <= kind && kind <= 16 || (kind === 24) || (kind === 26)) {
			return true;
		}
		_3 = kind;
		if (_3 === (17)) {
			return $interfaceIsEqual(T.Elem(), V.Elem()) && (T.Len() === V.Len());
		} else if (_3 === (18)) {
			if ((V.ChanDir() === 3) && $interfaceIsEqual(T.Elem(), V.Elem())) {
				return true;
			}
			return (V.ChanDir() === T.ChanDir()) && $interfaceIsEqual(T.Elem(), V.Elem());
		} else if (_3 === (19)) {
			t = T.kindType;
			v = V.kindType;
			if (!(t.dotdotdot === v.dotdotdot) || !((t.in$2.$length === v.in$2.$length)) || !((t.out.$length === v.out.$length))) {
				return false;
			}
			_ref = t.in$2;
			_i = 0;
			while (true) {
				if (!(_i < _ref.$length)) { break; }
				i = _i;
				typ = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
				if (!(typ === (x = v.in$2, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i])))) {
					return false;
				}
				_i++;
			}
			_ref$1 = t.out;
			_i$1 = 0;
			while (true) {
				if (!(_i$1 < _ref$1.$length)) { break; }
				i$1 = _i$1;
				typ$1 = ((_i$1 < 0 || _i$1 >= _ref$1.$length) ? $throwRuntimeError("index out of range") : _ref$1.$array[_ref$1.$offset + _i$1]);
				if (!(typ$1 === (x$1 = v.out, ((i$1 < 0 || i$1 >= x$1.$length) ? $throwRuntimeError("index out of range") : x$1.$array[x$1.$offset + i$1])))) {
					return false;
				}
				_i$1++;
			}
			return true;
		} else if (_3 === (20)) {
			t$1 = T.kindType;
			v$1 = V.kindType;
			if ((t$1.methods.$length === 0) && (v$1.methods.$length === 0)) {
				return true;
			}
			return false;
		} else if (_3 === (21)) {
			return $interfaceIsEqual(T.Key(), V.Key()) && $interfaceIsEqual(T.Elem(), V.Elem());
		} else if ((_3 === (22)) || (_3 === (23))) {
			return $interfaceIsEqual(T.Elem(), V.Elem());
		} else if (_3 === (25)) {
			t$2 = T.kindType;
			v$2 = V.kindType;
			if (!((t$2.fields.$length === v$2.fields.$length))) {
				return false;
			}
			_ref$2 = t$2.fields;
			_i$2 = 0;
			while (true) {
				if (!(_i$2 < _ref$2.$length)) { break; }
				i$2 = _i$2;
				tf = (x$2 = t$2.fields, ((i$2 < 0 || i$2 >= x$2.$length) ? $throwRuntimeError("index out of range") : x$2.$array[x$2.$offset + i$2]));
				vf = (x$3 = v$2.fields, ((i$2 < 0 || i$2 >= x$3.$length) ? $throwRuntimeError("index out of range") : x$3.$array[x$3.$offset + i$2]));
				if (!(tf.name === vf.name) && (tf.name === ptrType$6.nil || vf.name === ptrType$6.nil || !(tf.name.$get() === vf.name.$get()))) {
					return false;
				}
				if (!(tf.pkgPath === vf.pkgPath) && (tf.pkgPath === ptrType$6.nil || vf.pkgPath === ptrType$6.nil || !(tf.pkgPath.$get() === vf.pkgPath.$get()))) {
					return false;
				}
				if (!(tf.typ === vf.typ)) {
					return false;
				}
				if (!(tf.tag === vf.tag) && (tf.tag === ptrType$6.nil || vf.tag === ptrType$6.nil || !(tf.tag.$get() === vf.tag.$get()))) {
					return false;
				}
				if (!((tf.offset === vf.offset))) {
					return false;
				}
				_i$2++;
			}
			return true;
		}
		return false;
	};
	toType = function(t) {
		var $ptr, t;
		if (t === ptrType$1.nil) {
			return $ifaceNil;
		}
		return t;
	};
	ifaceIndir = function(t) {
		var $ptr, t;
		return ((t.kind & 32) >>> 0) === 0;
	};
	flag.prototype.kind = function() {
		var $ptr, f;
		f = this.$val;
		return (((f & 31) >>> 0) >>> 0);
	};
	$ptrType(flag).prototype.kind = function() { return new flag(this.$get()).kind(); };
	Value.ptr.prototype.pointer = function() {
		var $ptr, v;
		v = this;
		if (!((v.typ.size === 4)) || !v.typ.pointers()) {
			$panic(new $String("can't call pointer on a non-pointer Value"));
		}
		if (!((((v.flag & 128) >>> 0) === 0))) {
			return v.ptr.$get();
		}
		return v.ptr;
	};
	Value.prototype.pointer = function() { return this.$val.pointer(); };
	ValueError.ptr.prototype.Error = function() {
		var $ptr, e;
		e = this;
		if (e.Kind === 0) {
			return "reflect: call of " + e.Method + " on zero Value";
		}
		return "reflect: call of " + e.Method + " on " + new Kind(e.Kind).String() + " Value";
	};
	ValueError.prototype.Error = function() { return this.$val.Error(); };
	flag.prototype.mustBe = function(expected) {
		var $ptr, expected, f;
		f = this.$val;
		if (!((new flag(f).kind() === expected))) {
			$panic(new ValueError.ptr(methodName(), new flag(f).kind()));
		}
	};
	$ptrType(flag).prototype.mustBe = function(expected) { return new flag(this.$get()).mustBe(expected); };
	flag.prototype.mustBeExported = function() {
		var $ptr, f;
		f = this.$val;
		if (f === 0) {
			$panic(new ValueError.ptr(methodName(), 0));
		}
		if (!((((f & 96) >>> 0) === 0))) {
			$panic(new $String("reflect: " + methodName() + " using value obtained using unexported field"));
		}
	};
	$ptrType(flag).prototype.mustBeExported = function() { return new flag(this.$get()).mustBeExported(); };
	flag.prototype.mustBeAssignable = function() {
		var $ptr, f;
		f = this.$val;
		if (f === 0) {
			$panic(new ValueError.ptr(methodName(), 0));
		}
		if (!((((f & 96) >>> 0) === 0))) {
			$panic(new $String("reflect: " + methodName() + " using value obtained using unexported field"));
		}
		if (((f & 256) >>> 0) === 0) {
			$panic(new $String("reflect: " + methodName() + " using unaddressable value"));
		}
	};
	$ptrType(flag).prototype.mustBeAssignable = function() { return new flag(this.$get()).mustBeAssignable(); };
	Value.ptr.prototype.Addr = function() {
		var $ptr, v;
		v = this;
		if (((v.flag & 256) >>> 0) === 0) {
			$panic(new $String("reflect.Value.Addr of unaddressable value"));
		}
		return new Value.ptr(v.typ.ptrTo(), v.ptr, ((((v.flag & 96) >>> 0)) | 22) >>> 0);
	};
	Value.prototype.Addr = function() { return this.$val.Addr(); };
	Value.ptr.prototype.Bool = function() {
		var $ptr, v;
		v = this;
		new flag(v.flag).mustBe(1);
		return v.ptr.$get();
	};
	Value.prototype.Bool = function() { return this.$val.Bool(); };
	Value.ptr.prototype.Bytes = function() {
		var $ptr, _r, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBe(23);
		_r = v.typ.Elem().Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (!((_r === 8))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((_r === 8))) { */ case 1:
			$panic(new $String("reflect.Value.Bytes of non-byte slice"));
		/* } */ case 2:
		return v.ptr.$get();
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Bytes }; } $f.$ptr = $ptr; $f._r = _r; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Bytes = function() { return this.$val.Bytes(); };
	Value.ptr.prototype.runes = function() {
		var $ptr, _r, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBe(23);
		_r = v.typ.Elem().Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (!((_r === 5))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((_r === 5))) { */ case 1:
			$panic(new $String("reflect.Value.Bytes of non-rune slice"));
		/* } */ case 2:
		return v.ptr.$get();
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.runes }; } $f.$ptr = $ptr; $f._r = _r; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.runes = function() { return this.$val.runes(); };
	Value.ptr.prototype.CanAddr = function() {
		var $ptr, v;
		v = this;
		return !((((v.flag & 256) >>> 0) === 0));
	};
	Value.prototype.CanAddr = function() { return this.$val.CanAddr(); };
	Value.ptr.prototype.CanSet = function() {
		var $ptr, v;
		v = this;
		return ((v.flag & 352) >>> 0) === 256;
	};
	Value.prototype.CanSet = function() { return this.$val.CanSet(); };
	Value.ptr.prototype.Call = function(in$1) {
		var $ptr, _r, in$1, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; in$1 = $f.in$1; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBe(19);
		new flag(v.flag).mustBeExported();
		_r = v.call("Call", in$1); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Call }; } $f.$ptr = $ptr; $f._r = _r; $f.in$1 = in$1; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Call = function(in$1) { return this.$val.Call(in$1); };
	Value.ptr.prototype.CallSlice = function(in$1) {
		var $ptr, _r, in$1, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; in$1 = $f.in$1; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBe(19);
		new flag(v.flag).mustBeExported();
		_r = v.call("CallSlice", in$1); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.CallSlice }; } $f.$ptr = $ptr; $f._r = _r; $f.in$1 = in$1; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.CallSlice = function(in$1) { return this.$val.CallSlice(in$1); };
	Value.ptr.prototype.Complex = function() {
		var $ptr, _2, k, v, x;
		v = this;
		k = new flag(v.flag).kind();
		_2 = k;
		if (_2 === (15)) {
			return (x = v.ptr.$get(), new $Complex128(x.$real, x.$imag));
		} else if (_2 === (16)) {
			return v.ptr.$get();
		}
		$panic(new ValueError.ptr("reflect.Value.Complex", new flag(v.flag).kind()));
	};
	Value.prototype.Complex = function() { return this.$val.Complex(); };
	Value.ptr.prototype.FieldByIndex = function(index) {
		var $ptr, _i, _r, _r$1, _r$2, _r$3, _ref, _v, i, index, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _i = $f._i; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _ref = $f._ref; _v = $f._v; i = $f.i; index = $f.index; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		/* */ if (index.$length === 1) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (index.$length === 1) { */ case 1:
			_r = v.Field((0 >= index.$length ? $throwRuntimeError("index out of range") : index.$array[index.$offset + 0])); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			/* */ $s = 4; case 4:
			return _r;
		/* } */ case 2:
		new flag(v.flag).mustBe(25);
		_ref = index;
		_i = 0;
		/* while (true) { */ case 5:
			/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 6; continue; }
			i = _i;
			x = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			/* */ if (i > 0) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (i > 0) { */ case 7:
				if (!(v.Kind() === 22)) { _v = false; $s = 11; continue s; }
				_r$1 = v.typ.Elem().Kind(); /* */ $s = 12; case 12: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				_v = _r$1 === 25; case 11:
				/* */ if (_v) { $s = 9; continue; }
				/* */ $s = 10; continue;
				/* if (_v) { */ case 9:
					if (v.IsNil()) {
						$panic(new $String("reflect: indirection through nil pointer to embedded struct"));
					}
					_r$2 = v.Elem(); /* */ $s = 13; case 13: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					v = _r$2;
				/* } */ case 10:
			/* } */ case 8:
			_r$3 = v.Field(x); /* */ $s = 14; case 14: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			v = _r$3;
			_i++;
		/* } */ $s = 5; continue; case 6:
		return v;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.FieldByIndex }; } $f.$ptr = $ptr; $f._i = _i; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._ref = _ref; $f._v = _v; $f.i = i; $f.index = index; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.FieldByIndex = function(index) { return this.$val.FieldByIndex(index); };
	Value.ptr.prototype.FieldByName = function(name) {
		var $ptr, _r, _r$1, _tuple, f, name, ok, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _tuple = $f._tuple; f = $f.f; name = $f.name; ok = $f.ok; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBe(25);
		_r = v.typ.FieldByName(name); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		f = $clone(_tuple[0], StructField);
		ok = _tuple[1];
		/* */ if (ok) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (ok) { */ case 2:
			_r$1 = v.FieldByIndex(f.Index); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			/* */ $s = 5; case 5:
			return _r$1;
		/* } */ case 3:
		return new Value.ptr(ptrType$1.nil, 0, 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.FieldByName }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._tuple = _tuple; $f.f = f; $f.name = name; $f.ok = ok; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.FieldByName = function(name) { return this.$val.FieldByName(name); };
	Value.ptr.prototype.FieldByNameFunc = function(match) {
		var $ptr, _r, _r$1, _tuple, f, match, ok, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _tuple = $f._tuple; f = $f.f; match = $f.match; ok = $f.ok; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		_r = v.typ.FieldByNameFunc(match); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		f = $clone(_tuple[0], StructField);
		ok = _tuple[1];
		/* */ if (ok) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (ok) { */ case 2:
			_r$1 = v.FieldByIndex(f.Index); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			/* */ $s = 5; case 5:
			return _r$1;
		/* } */ case 3:
		return new Value.ptr(ptrType$1.nil, 0, 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.FieldByNameFunc }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._tuple = _tuple; $f.f = f; $f.match = match; $f.ok = ok; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.FieldByNameFunc = function(match) { return this.$val.FieldByNameFunc(match); };
	Value.ptr.prototype.Float = function() {
		var $ptr, _4, k, v;
		v = this;
		k = new flag(v.flag).kind();
		_4 = k;
		if (_4 === (13)) {
			return v.ptr.$get();
		} else if (_4 === (14)) {
			return v.ptr.$get();
		}
		$panic(new ValueError.ptr("reflect.Value.Float", new flag(v.flag).kind()));
	};
	Value.prototype.Float = function() { return this.$val.Float(); };
	Value.ptr.prototype.Int = function() {
		var $ptr, _6, k, p, v;
		v = this;
		k = new flag(v.flag).kind();
		p = v.ptr;
		_6 = k;
		if (_6 === (2)) {
			return new $Int64(0, p.$get());
		} else if (_6 === (3)) {
			return new $Int64(0, p.$get());
		} else if (_6 === (4)) {
			return new $Int64(0, p.$get());
		} else if (_6 === (5)) {
			return new $Int64(0, p.$get());
		} else if (_6 === (6)) {
			return p.$get();
		}
		$panic(new ValueError.ptr("reflect.Value.Int", new flag(v.flag).kind()));
	};
	Value.prototype.Int = function() { return this.$val.Int(); };
	Value.ptr.prototype.CanInterface = function() {
		var $ptr, v;
		v = this;
		if (v.flag === 0) {
			$panic(new ValueError.ptr("reflect.Value.CanInterface", 0));
		}
		return ((v.flag & 96) >>> 0) === 0;
	};
	Value.prototype.CanInterface = function() { return this.$val.CanInterface(); };
	Value.ptr.prototype.Interface = function() {
		var $ptr, _r, i, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; i = $f.i; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		i = $ifaceNil;
		v = this;
		_r = valueInterface(v, true); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		i = _r;
		/* */ $s = 2; case 2:
		return i;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Interface }; } $f.$ptr = $ptr; $f._r = _r; $f.i = i; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Interface = function() { return this.$val.Interface(); };
	Value.ptr.prototype.IsValid = function() {
		var $ptr, v;
		v = this;
		return !((v.flag === 0));
	};
	Value.prototype.IsValid = function() { return this.$val.IsValid(); };
	Value.ptr.prototype.Kind = function() {
		var $ptr, v;
		v = this;
		return new flag(v.flag).kind();
	};
	Value.prototype.Kind = function() { return this.$val.Kind(); };
	Value.ptr.prototype.MapIndex = function(key) {
		var $ptr, _r, c, e, fl, k, key, tt, typ, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; c = $f.c; e = $f.e; fl = $f.fl; k = $f.k; key = $f.key; tt = $f.tt; typ = $f.typ; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		key = key;
		v = this;
		new flag(v.flag).mustBe(21);
		tt = v.typ.kindType;
		_r = key.assignTo("reflect.Value.MapIndex", tt.key, 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		key = _r;
		k = 0;
		if (!((((key.flag & 128) >>> 0) === 0))) {
			k = key.ptr;
		} else {
			k = (key.$ptr_ptr || (key.$ptr_ptr = new ptrType$18(function() { return this.$target.ptr; }, function($v) { this.$target.ptr = $v; }, key)));
		}
		e = mapaccess(v.typ, v.pointer(), k);
		if (e === 0) {
			return new Value.ptr(ptrType$1.nil, 0, 0);
		}
		typ = tt.elem;
		fl = ((((v.flag | key.flag) >>> 0)) & 96) >>> 0;
		fl = (fl | ((typ.Kind() >>> 0))) >>> 0;
		if (ifaceIndir(typ)) {
			c = unsafe_New(typ);
			typedmemmove(typ, c, e);
			return new Value.ptr(typ, c, (fl | 128) >>> 0);
		} else {
			return new Value.ptr(typ, e.$get(), fl);
		}
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.MapIndex }; } $f.$ptr = $ptr; $f._r = _r; $f.c = c; $f.e = e; $f.fl = fl; $f.k = k; $f.key = key; $f.tt = tt; $f.typ = typ; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.MapIndex = function(key) { return this.$val.MapIndex(key); };
	Value.ptr.prototype.MapKeys = function() {
		var $ptr, _r, a, c, fl, i, it, key, keyType, m, mlen, tt, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; a = $f.a; c = $f.c; fl = $f.fl; i = $f.i; it = $f.it; key = $f.key; keyType = $f.keyType; m = $f.m; mlen = $f.mlen; tt = $f.tt; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBe(21);
		tt = v.typ.kindType;
		keyType = tt.key;
		fl = (((v.flag & 96) >>> 0) | (keyType.Kind() >>> 0)) >>> 0;
		m = v.pointer();
		mlen = 0;
		if (!(m === 0)) {
			mlen = maplen(m);
		}
		it = mapiterinit(v.typ, m);
		a = $makeSlice(sliceType$8, mlen);
		i = 0;
		i = 0;
		/* while (true) { */ case 1:
			/* if (!(i < a.$length)) { break; } */ if(!(i < a.$length)) { $s = 2; continue; }
			_r = mapiterkey(it); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			key = _r;
			if (key === 0) {
				/* break; */ $s = 2; continue;
			}
			if (ifaceIndir(keyType)) {
				c = unsafe_New(keyType);
				typedmemmove(keyType, c, key);
				((i < 0 || i >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + i] = new Value.ptr(keyType, c, (fl | 128) >>> 0));
			} else {
				((i < 0 || i >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + i] = new Value.ptr(keyType, key.$get(), fl));
			}
			mapiternext(it);
			i = i + (1) >> 0;
		/* } */ $s = 1; continue; case 2:
		return $subslice(a, 0, i);
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.MapKeys }; } $f.$ptr = $ptr; $f._r = _r; $f.a = a; $f.c = c; $f.fl = fl; $f.i = i; $f.it = it; $f.key = key; $f.keyType = keyType; $f.m = m; $f.mlen = mlen; $f.tt = tt; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.MapKeys = function() { return this.$val.MapKeys(); };
	Value.ptr.prototype.Method = function(i) {
		var $ptr, fl, i, v;
		v = this;
		if (v.typ === ptrType$1.nil) {
			$panic(new ValueError.ptr("reflect.Value.Method", 0));
		}
		if (!((((v.flag & 512) >>> 0) === 0)) || (i >>> 0) >= (v.typ.NumMethod() >>> 0)) {
			$panic(new $String("reflect: Method index out of range"));
		}
		if ((v.typ.Kind() === 20) && v.IsNil()) {
			$panic(new $String("reflect: Method on nil interface value"));
		}
		fl = (v.flag & 160) >>> 0;
		fl = (fl | (19)) >>> 0;
		fl = (fl | (((((i >>> 0) << 10 >>> 0) | 512) >>> 0))) >>> 0;
		return new Value.ptr(v.typ, v.ptr, fl);
	};
	Value.prototype.Method = function(i) { return this.$val.Method(i); };
	Value.ptr.prototype.NumMethod = function() {
		var $ptr, v;
		v = this;
		if (v.typ === ptrType$1.nil) {
			$panic(new ValueError.ptr("reflect.Value.NumMethod", 0));
		}
		if (!((((v.flag & 512) >>> 0) === 0))) {
			return 0;
		}
		return v.typ.NumMethod();
	};
	Value.prototype.NumMethod = function() { return this.$val.NumMethod(); };
	Value.ptr.prototype.MethodByName = function(name) {
		var $ptr, _tuple, m, name, ok, v;
		v = this;
		if (v.typ === ptrType$1.nil) {
			$panic(new ValueError.ptr("reflect.Value.MethodByName", 0));
		}
		if (!((((v.flag & 512) >>> 0) === 0))) {
			return new Value.ptr(ptrType$1.nil, 0, 0);
		}
		_tuple = v.typ.MethodByName(name);
		m = $clone(_tuple[0], Method);
		ok = _tuple[1];
		if (!ok) {
			return new Value.ptr(ptrType$1.nil, 0, 0);
		}
		return v.Method(m.Index);
	};
	Value.prototype.MethodByName = function(name) { return this.$val.MethodByName(name); };
	Value.ptr.prototype.NumField = function() {
		var $ptr, tt, v;
		v = this;
		new flag(v.flag).mustBe(25);
		tt = v.typ.kindType;
		return tt.fields.$length;
	};
	Value.prototype.NumField = function() { return this.$val.NumField(); };
	Value.ptr.prototype.OverflowComplex = function(x) {
		var $ptr, _9, k, v, x;
		v = this;
		k = new flag(v.flag).kind();
		_9 = k;
		if (_9 === (15)) {
			return overflowFloat32(x.$real) || overflowFloat32(x.$imag);
		} else if (_9 === (16)) {
			return false;
		}
		$panic(new ValueError.ptr("reflect.Value.OverflowComplex", new flag(v.flag).kind()));
	};
	Value.prototype.OverflowComplex = function(x) { return this.$val.OverflowComplex(x); };
	Value.ptr.prototype.OverflowFloat = function(x) {
		var $ptr, _10, k, v, x;
		v = this;
		k = new flag(v.flag).kind();
		_10 = k;
		if (_10 === (13)) {
			return overflowFloat32(x);
		} else if (_10 === (14)) {
			return false;
		}
		$panic(new ValueError.ptr("reflect.Value.OverflowFloat", new flag(v.flag).kind()));
	};
	Value.prototype.OverflowFloat = function(x) { return this.$val.OverflowFloat(x); };
	overflowFloat32 = function(x) {
		var $ptr, x;
		if (x < 0) {
			x = -x;
		}
		return 3.4028234663852886e+38 < x && x <= 1.7976931348623157e+308;
	};
	Value.ptr.prototype.OverflowInt = function(x) {
		var $ptr, _11, bitSize, k, trunc, v, x;
		v = this;
		k = new flag(v.flag).kind();
		_11 = k;
		if ((_11 === (2)) || (_11 === (3)) || (_11 === (4)) || (_11 === (5)) || (_11 === (6))) {
			bitSize = $imul(v.typ.size, 8) >>> 0;
			trunc = $shiftRightInt64(($shiftLeft64(x, ((64 - bitSize >>> 0)))), ((64 - bitSize >>> 0)));
			return !((x.$high === trunc.$high && x.$low === trunc.$low));
		}
		$panic(new ValueError.ptr("reflect.Value.OverflowInt", new flag(v.flag).kind()));
	};
	Value.prototype.OverflowInt = function(x) { return this.$val.OverflowInt(x); };
	Value.ptr.prototype.OverflowUint = function(x) {
		var $ptr, _12, bitSize, k, trunc, v, x;
		v = this;
		k = new flag(v.flag).kind();
		_12 = k;
		if ((_12 === (7)) || (_12 === (12)) || (_12 === (8)) || (_12 === (9)) || (_12 === (10)) || (_12 === (11))) {
			bitSize = $imul(v.typ.size, 8) >>> 0;
			trunc = $shiftRightUint64(($shiftLeft64(x, ((64 - bitSize >>> 0)))), ((64 - bitSize >>> 0)));
			return !((x.$high === trunc.$high && x.$low === trunc.$low));
		}
		$panic(new ValueError.ptr("reflect.Value.OverflowUint", new flag(v.flag).kind()));
	};
	Value.prototype.OverflowUint = function(x) { return this.$val.OverflowUint(x); };
	Value.ptr.prototype.Recv = function() {
		var $ptr, _r, _tuple, ok, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tuple = $f._tuple; ok = $f.ok; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		x = new Value.ptr(ptrType$1.nil, 0, 0);
		ok = false;
		v = this;
		new flag(v.flag).mustBe(18);
		new flag(v.flag).mustBeExported();
		_r = v.recv(false); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		x = _tuple[0];
		ok = _tuple[1];
		/* */ $s = 2; case 2:
		return [x, ok];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Recv }; } $f.$ptr = $ptr; $f._r = _r; $f._tuple = _tuple; $f.ok = ok; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Recv = function() { return this.$val.Recv(); };
	Value.ptr.prototype.recv = function(nb) {
		var $ptr, _r, _tuple, nb, ok, p, selected, t, tt, v, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tuple = $f._tuple; nb = $f.nb; ok = $f.ok; p = $f.p; selected = $f.selected; t = $f.t; tt = $f.tt; v = $f.v; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		val = new Value.ptr(ptrType$1.nil, 0, 0);
		ok = false;
		v = this;
		tt = v.typ.kindType;
		if (((tt.dir >> 0) & 1) === 0) {
			$panic(new $String("reflect: recv on send-only channel"));
		}
		t = tt.elem;
		val = new Value.ptr(t, 0, (t.Kind() >>> 0));
		p = 0;
		if (ifaceIndir(t)) {
			p = unsafe_New(t);
			val.ptr = p;
			val.flag = (val.flag | (128)) >>> 0;
		} else {
			p = (val.$ptr_ptr || (val.$ptr_ptr = new ptrType$18(function() { return this.$target.ptr; }, function($v) { this.$target.ptr = $v; }, val)));
		}
		_r = chanrecv(v.typ, v.pointer(), nb, p); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		selected = _tuple[0];
		ok = _tuple[1];
		if (!selected) {
			val = new Value.ptr(ptrType$1.nil, 0, 0);
		}
		return [val, ok];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.recv }; } $f.$ptr = $ptr; $f._r = _r; $f._tuple = _tuple; $f.nb = nb; $f.ok = ok; $f.p = p; $f.selected = selected; $f.t = t; $f.tt = tt; $f.v = v; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.recv = function(nb) { return this.$val.recv(nb); };
	Value.ptr.prototype.Send = function(x) {
		var $ptr, _r, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		x = x;
		v = this;
		new flag(v.flag).mustBe(18);
		new flag(v.flag).mustBeExported();
		_r = v.send(x, false); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r;
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Send }; } $f.$ptr = $ptr; $f._r = _r; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Send = function(x) { return this.$val.Send(x); };
	Value.ptr.prototype.send = function(x, nb) {
		var $ptr, _r, _r$1, nb, p, selected, tt, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; nb = $f.nb; p = $f.p; selected = $f.selected; tt = $f.tt; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		selected = false;
		x = x;
		v = this;
		tt = v.typ.kindType;
		if (((tt.dir >> 0) & 2) === 0) {
			$panic(new $String("reflect: send on recv-only channel"));
		}
		new flag(x.flag).mustBeExported();
		_r = x.assignTo("reflect.Value.Send", tt.elem, 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		x = _r;
		p = 0;
		if (!((((x.flag & 128) >>> 0) === 0))) {
			p = x.ptr;
		} else {
			p = (x.$ptr_ptr || (x.$ptr_ptr = new ptrType$18(function() { return this.$target.ptr; }, function($v) { this.$target.ptr = $v; }, x)));
		}
		_r$1 = chansend(v.typ, v.pointer(), p, nb); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		selected = _r$1;
		/* */ $s = 3; case 3:
		return selected;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.send }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.nb = nb; $f.p = p; $f.selected = selected; $f.tt = tt; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.send = function(x, nb) { return this.$val.send(x, nb); };
	Value.ptr.prototype.SetBool = function(x) {
		var $ptr, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(1);
		v.ptr.$set(x);
	};
	Value.prototype.SetBool = function(x) { return this.$val.SetBool(x); };
	Value.ptr.prototype.setRunes = function(x) {
		var $ptr, _r, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(23);
		_r = v.typ.Elem().Kind(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ if (!((_r === 5))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((_r === 5))) { */ case 1:
			$panic(new $String("reflect.Value.setRunes of non-rune slice"));
		/* } */ case 2:
		v.ptr.$set(x);
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.setRunes }; } $f.$ptr = $ptr; $f._r = _r; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.setRunes = function(x) { return this.$val.setRunes(x); };
	Value.ptr.prototype.SetComplex = function(x) {
		var $ptr, _14, k, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		k = new flag(v.flag).kind();
		_14 = k;
		if (_14 === (15)) {
			v.ptr.$set(new $Complex64(x.$real, x.$imag));
		} else if (_14 === (16)) {
			v.ptr.$set(x);
		} else {
			$panic(new ValueError.ptr("reflect.Value.SetComplex", new flag(v.flag).kind()));
		}
	};
	Value.prototype.SetComplex = function(x) { return this.$val.SetComplex(x); };
	Value.ptr.prototype.SetFloat = function(x) {
		var $ptr, _15, k, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		k = new flag(v.flag).kind();
		_15 = k;
		if (_15 === (13)) {
			v.ptr.$set($fround(x));
		} else if (_15 === (14)) {
			v.ptr.$set(x);
		} else {
			$panic(new ValueError.ptr("reflect.Value.SetFloat", new flag(v.flag).kind()));
		}
	};
	Value.prototype.SetFloat = function(x) { return this.$val.SetFloat(x); };
	Value.ptr.prototype.SetInt = function(x) {
		var $ptr, _16, k, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		k = new flag(v.flag).kind();
		_16 = k;
		if (_16 === (2)) {
			v.ptr.$set(((x.$low + ((x.$high >> 31) * 4294967296)) >> 0));
		} else if (_16 === (3)) {
			v.ptr.$set(((x.$low + ((x.$high >> 31) * 4294967296)) << 24 >> 24));
		} else if (_16 === (4)) {
			v.ptr.$set(((x.$low + ((x.$high >> 31) * 4294967296)) << 16 >> 16));
		} else if (_16 === (5)) {
			v.ptr.$set(((x.$low + ((x.$high >> 31) * 4294967296)) >> 0));
		} else if (_16 === (6)) {
			v.ptr.$set(x);
		} else {
			$panic(new ValueError.ptr("reflect.Value.SetInt", new flag(v.flag).kind()));
		}
	};
	Value.prototype.SetInt = function(x) { return this.$val.SetInt(x); };
	Value.ptr.prototype.SetMapIndex = function(key, val) {
		var $ptr, _r, _r$1, e, k, key, tt, v, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; e = $f.e; k = $f.k; key = $f.key; tt = $f.tt; v = $f.v; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		val = val;
		key = key;
		v = this;
		new flag(v.flag).mustBe(21);
		new flag(v.flag).mustBeExported();
		new flag(key.flag).mustBeExported();
		tt = v.typ.kindType;
		_r = key.assignTo("reflect.Value.SetMapIndex", tt.key, 0); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		key = _r;
		k = 0;
		if (!((((key.flag & 128) >>> 0) === 0))) {
			k = key.ptr;
		} else {
			k = (key.$ptr_ptr || (key.$ptr_ptr = new ptrType$18(function() { return this.$target.ptr; }, function($v) { this.$target.ptr = $v; }, key)));
		}
		if (val.typ === ptrType$1.nil) {
			mapdelete(v.typ, v.pointer(), k);
			return;
		}
		new flag(val.flag).mustBeExported();
		_r$1 = val.assignTo("reflect.Value.SetMapIndex", tt.elem, 0); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		val = _r$1;
		e = 0;
		if (!((((val.flag & 128) >>> 0) === 0))) {
			e = val.ptr;
		} else {
			e = (val.$ptr_ptr || (val.$ptr_ptr = new ptrType$18(function() { return this.$target.ptr; }, function($v) { this.$target.ptr = $v; }, val)));
		}
		$r = mapassign(v.typ, v.pointer(), k, e); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.SetMapIndex }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.e = e; $f.k = k; $f.key = key; $f.tt = tt; $f.v = v; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.SetMapIndex = function(key, val) { return this.$val.SetMapIndex(key, val); };
	Value.ptr.prototype.SetUint = function(x) {
		var $ptr, _17, k, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		k = new flag(v.flag).kind();
		_17 = k;
		if (_17 === (7)) {
			v.ptr.$set((x.$low >>> 0));
		} else if (_17 === (8)) {
			v.ptr.$set((x.$low << 24 >>> 24));
		} else if (_17 === (9)) {
			v.ptr.$set((x.$low << 16 >>> 16));
		} else if (_17 === (10)) {
			v.ptr.$set((x.$low >>> 0));
		} else if (_17 === (11)) {
			v.ptr.$set(x);
		} else if (_17 === (12)) {
			v.ptr.$set((x.$low >>> 0));
		} else {
			$panic(new ValueError.ptr("reflect.Value.SetUint", new flag(v.flag).kind()));
		}
	};
	Value.prototype.SetUint = function(x) { return this.$val.SetUint(x); };
	Value.ptr.prototype.SetPointer = function(x) {
		var $ptr, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(26);
		v.ptr.$set(x);
	};
	Value.prototype.SetPointer = function(x) { return this.$val.SetPointer(x); };
	Value.ptr.prototype.SetString = function(x) {
		var $ptr, v, x;
		v = this;
		new flag(v.flag).mustBeAssignable();
		new flag(v.flag).mustBe(24);
		v.ptr.$set(x);
	};
	Value.prototype.SetString = function(x) { return this.$val.SetString(x); };
	Value.ptr.prototype.String = function() {
		var $ptr, _20, _r, k, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _20 = $f._20; _r = $f._r; k = $f.k; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		k = new flag(v.flag).kind();
		_20 = k;
		if (_20 === (0)) {
			return "<invalid Value>";
		} else if (_20 === (24)) {
			return v.ptr.$get();
		}
		_r = v.Type().String(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return "<" + _r + " Value>";
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.String }; } $f.$ptr = $ptr; $f._20 = _20; $f._r = _r; $f.k = k; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.String = function() { return this.$val.String(); };
	Value.ptr.prototype.TryRecv = function() {
		var $ptr, _r, _tuple, ok, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tuple = $f._tuple; ok = $f.ok; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		x = new Value.ptr(ptrType$1.nil, 0, 0);
		ok = false;
		v = this;
		new flag(v.flag).mustBe(18);
		new flag(v.flag).mustBeExported();
		_r = v.recv(true); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		x = _tuple[0];
		ok = _tuple[1];
		/* */ $s = 2; case 2:
		return [x, ok];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.TryRecv }; } $f.$ptr = $ptr; $f._r = _r; $f._tuple = _tuple; $f.ok = ok; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.TryRecv = function() { return this.$val.TryRecv(); };
	Value.ptr.prototype.TrySend = function(x) {
		var $ptr, _r, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		x = x;
		v = this;
		new flag(v.flag).mustBe(18);
		new flag(v.flag).mustBeExported();
		_r = v.send(x, true); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.TrySend }; } $f.$ptr = $ptr; $f._r = _r; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.TrySend = function(x) { return this.$val.TrySend(x); };
	Value.ptr.prototype.Type = function() {
		var $ptr, f, i, m, m$1, tt, ut, v, x, x$1;
		v = this;
		f = v.flag;
		if (f === 0) {
			$panic(new ValueError.ptr("reflect.Value.Type", 0));
		}
		if (((f & 512) >>> 0) === 0) {
			return v.typ;
		}
		i = (v.flag >> 0) >> 10 >> 0;
		if (v.typ.Kind() === 20) {
			tt = v.typ.kindType;
			if ((i >>> 0) >= (tt.methods.$length >>> 0)) {
				$panic(new $String("reflect: internal error: invalid method index"));
			}
			m = (x = tt.methods, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i]));
			return m.typ;
		}
		ut = v.typ.uncommonType.uncommon();
		if (ut === ptrType$7.nil || (i >>> 0) >= (ut.methods.$length >>> 0)) {
			$panic(new $String("reflect: internal error: invalid method index"));
		}
		m$1 = (x$1 = ut.methods, ((i < 0 || i >= x$1.$length) ? $throwRuntimeError("index out of range") : x$1.$array[x$1.$offset + i]));
		return m$1.mtyp;
	};
	Value.prototype.Type = function() { return this.$val.Type(); };
	Value.ptr.prototype.Uint = function() {
		var $ptr, _21, k, p, v, x;
		v = this;
		k = new flag(v.flag).kind();
		p = v.ptr;
		_21 = k;
		if (_21 === (7)) {
			return new $Uint64(0, p.$get());
		} else if (_21 === (8)) {
			return new $Uint64(0, p.$get());
		} else if (_21 === (9)) {
			return new $Uint64(0, p.$get());
		} else if (_21 === (10)) {
			return new $Uint64(0, p.$get());
		} else if (_21 === (11)) {
			return p.$get();
		} else if (_21 === (12)) {
			return (x = p.$get(), new $Uint64(0, x.constructor === Number ? x : 1));
		}
		$panic(new ValueError.ptr("reflect.Value.Uint", new flag(v.flag).kind()));
	};
	Value.prototype.Uint = function() { return this.$val.Uint(); };
	Value.ptr.prototype.UnsafeAddr = function() {
		var $ptr, v;
		v = this;
		if (v.typ === ptrType$1.nil) {
			$panic(new ValueError.ptr("reflect.Value.UnsafeAddr", 0));
		}
		if (((v.flag & 256) >>> 0) === 0) {
			$panic(new $String("reflect.Value.UnsafeAddr of unaddressable value"));
		}
		return v.ptr;
	};
	Value.prototype.UnsafeAddr = function() { return this.$val.UnsafeAddr(); };
	New = function(typ) {
		var $ptr, _r, _r$1, fl, ptr, typ, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; fl = $f.fl; ptr = $f.ptr; typ = $f.typ; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		if ($interfaceIsEqual(typ, $ifaceNil)) {
			$panic(new $String("reflect: New(nil)"));
		}
		ptr = unsafe_New($assertType(typ, ptrType$1));
		fl = 22;
		_r = typ.common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r$1 = _r.ptrTo(); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return new Value.ptr(_r$1, ptr, fl);
		/* */ } return; } if ($f === undefined) { $f = { $blk: New }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.fl = fl; $f.ptr = ptr; $f.typ = typ; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.New = New;
	Value.ptr.prototype.assignTo = function(context, dst, target) {
		var $ptr, _r, _r$1, context, dst, fl, target, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; context = $f.context; dst = $f.dst; fl = $f.fl; target = $f.target; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		/* */ if (!((((v.flag & 512) >>> 0) === 0))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((((v.flag & 512) >>> 0) === 0))) { */ case 1:
			_r = makeMethodValue(context, v); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			v = _r;
		/* } */ case 2:
			/* */ if (directlyAssignable(dst, v.typ)) { $s = 5; continue; }
			/* */ if (implements$1(dst, v.typ)) { $s = 6; continue; }
			/* */ $s = 7; continue;
			/* if (directlyAssignable(dst, v.typ)) { */ case 5:
				v.typ = dst;
				fl = (v.flag & 480) >>> 0;
				fl = (fl | ((dst.Kind() >>> 0))) >>> 0;
				return new Value.ptr(dst, v.ptr, fl);
			/* } else if (implements$1(dst, v.typ)) { */ case 6:
				if (target === 0) {
					target = unsafe_New(dst);
				}
				_r$1 = valueInterface(v, false); /* */ $s = 8; case 8: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				x = _r$1;
				if (dst.NumMethod() === 0) {
					target.$set(x);
				} else {
					ifaceE2I(dst, x, target);
				}
				return new Value.ptr(dst, target, 148);
			/* } */ case 7:
		case 4:
		$panic(new $String(context + ": value of type " + v.typ.String() + " is not assignable to type " + dst.String()));
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.assignTo }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.context = context; $f.dst = dst; $f.fl = fl; $f.target = target; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.assignTo = function(context, dst, target) { return this.$val.assignTo(context, dst, target); };
	Value.ptr.prototype.Convert = function(t) {
		var $ptr, _r, _r$1, _r$2, _r$3, _r$4, op, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; op = $f.op; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = this;
		/* */ if (!((((v.flag & 512) >>> 0) === 0))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!((((v.flag & 512) >>> 0) === 0))) { */ case 1:
			_r = makeMethodValue("Convert", v); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			v = _r;
		/* } */ case 2:
		_r$1 = t.common(); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_r$2 = convertOp(_r$1, v.typ); /* */ $s = 5; case 5: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		op = _r$2;
		/* */ if (op === $throwNilPointerError) { $s = 6; continue; }
		/* */ $s = 7; continue;
		/* if (op === $throwNilPointerError) { */ case 6:
			_r$3 = t.String(); /* */ $s = 8; case 8: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			$panic(new $String("reflect.Value.Convert: value of type " + v.typ.String() + " cannot be converted to type " + _r$3));
		/* } */ case 7:
		_r$4 = op(v, t); /* */ $s = 9; case 9: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
		/* */ $s = 10; case 10:
		return _r$4;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Value.ptr.prototype.Convert }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f.op = op; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Value.prototype.Convert = function(t) { return this.$val.Convert(t); };
	convertOp = function(dst, src) {
		var $ptr, _23, _24, _25, _26, _27, _28, _29, _arg, _arg$1, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _v, _v$1, _v$2, dst, src, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _23 = $f._23; _24 = $f._24; _25 = $f._25; _26 = $f._26; _27 = $f._27; _28 = $f._28; _29 = $f._29; _arg = $f._arg; _arg$1 = $f._arg$1; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _v = $f._v; _v$1 = $f._v$1; _v$2 = $f._v$2; dst = $f.dst; src = $f.src; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
			_23 = src.Kind();
			/* */ if ((_23 === (2)) || (_23 === (3)) || (_23 === (4)) || (_23 === (5)) || (_23 === (6))) { $s = 2; continue; }
			/* */ if ((_23 === (7)) || (_23 === (8)) || (_23 === (9)) || (_23 === (10)) || (_23 === (11)) || (_23 === (12))) { $s = 3; continue; }
			/* */ if ((_23 === (13)) || (_23 === (14))) { $s = 4; continue; }
			/* */ if ((_23 === (15)) || (_23 === (16))) { $s = 5; continue; }
			/* */ if (_23 === (24)) { $s = 6; continue; }
			/* */ if (_23 === (23)) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if ((_23 === (2)) || (_23 === (3)) || (_23 === (4)) || (_23 === (5)) || (_23 === (6))) { */ case 2:
				_24 = dst.Kind();
				if ((_24 === (2)) || (_24 === (3)) || (_24 === (4)) || (_24 === (5)) || (_24 === (6)) || (_24 === (7)) || (_24 === (8)) || (_24 === (9)) || (_24 === (10)) || (_24 === (11)) || (_24 === (12))) {
					return cvtInt;
				} else if ((_24 === (13)) || (_24 === (14))) {
					return cvtIntFloat;
				} else if (_24 === (24)) {
					return cvtIntString;
				}
				$s = 8; continue;
			/* } else if ((_23 === (7)) || (_23 === (8)) || (_23 === (9)) || (_23 === (10)) || (_23 === (11)) || (_23 === (12))) { */ case 3:
				_25 = dst.Kind();
				if ((_25 === (2)) || (_25 === (3)) || (_25 === (4)) || (_25 === (5)) || (_25 === (6)) || (_25 === (7)) || (_25 === (8)) || (_25 === (9)) || (_25 === (10)) || (_25 === (11)) || (_25 === (12))) {
					return cvtUint;
				} else if ((_25 === (13)) || (_25 === (14))) {
					return cvtUintFloat;
				} else if (_25 === (24)) {
					return cvtUintString;
				}
				$s = 8; continue;
			/* } else if ((_23 === (13)) || (_23 === (14))) { */ case 4:
				_26 = dst.Kind();
				if ((_26 === (2)) || (_26 === (3)) || (_26 === (4)) || (_26 === (5)) || (_26 === (6))) {
					return cvtFloatInt;
				} else if ((_26 === (7)) || (_26 === (8)) || (_26 === (9)) || (_26 === (10)) || (_26 === (11)) || (_26 === (12))) {
					return cvtFloatUint;
				} else if ((_26 === (13)) || (_26 === (14))) {
					return cvtFloat;
				}
				$s = 8; continue;
			/* } else if ((_23 === (15)) || (_23 === (16))) { */ case 5:
				_27 = dst.Kind();
				if ((_27 === (15)) || (_27 === (16))) {
					return cvtComplex;
				}
				$s = 8; continue;
			/* } else if (_23 === (24)) { */ case 6:
				if (!(dst.Kind() === 23)) { _v = false; $s = 11; continue s; }
				_r = dst.Elem().PkgPath(); /* */ $s = 12; case 12: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				_v = _r === ""; case 11:
				/* */ if (_v) { $s = 9; continue; }
				/* */ $s = 10; continue;
				/* if (_v) { */ case 9:
						_r$1 = dst.Elem().Kind(); /* */ $s = 14; case 14: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
						_28 = _r$1;
						if (_28 === (8)) {
							return cvtStringBytes;
						} else if (_28 === (5)) {
							return cvtStringRunes;
						}
					case 13:
				/* } */ case 10:
				$s = 8; continue;
			/* } else if (_23 === (23)) { */ case 7:
				if (!(dst.Kind() === 24)) { _v$1 = false; $s = 17; continue s; }
				_r$2 = src.Elem().PkgPath(); /* */ $s = 18; case 18: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_v$1 = _r$2 === ""; case 17:
				/* */ if (_v$1) { $s = 15; continue; }
				/* */ $s = 16; continue;
				/* if (_v$1) { */ case 15:
						_r$3 = src.Elem().Kind(); /* */ $s = 20; case 20: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
						_29 = _r$3;
						if (_29 === (8)) {
							return cvtBytesString;
						} else if (_29 === (5)) {
							return cvtRunesString;
						}
					case 19:
				/* } */ case 16:
			/* } */ case 8:
		case 1:
		if (haveIdenticalUnderlyingType(dst, src)) {
			return cvtDirect;
		}
		if (!((dst.Kind() === 22) && dst.Name() === "" && (src.Kind() === 22) && src.Name() === "")) { _v$2 = false; $s = 23; continue s; }
		_r$4 = dst.Elem().common(); /* */ $s = 24; case 24: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
		_arg = _r$4;
		_r$5 = src.Elem().common(); /* */ $s = 25; case 25: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
		_arg$1 = _r$5;
		_r$6 = haveIdenticalUnderlyingType(_arg, _arg$1); /* */ $s = 26; case 26: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
		_v$2 = _r$6; case 23:
		/* */ if (_v$2) { $s = 21; continue; }
		/* */ $s = 22; continue;
		/* if (_v$2) { */ case 21:
			return cvtDirect;
		/* } */ case 22:
		if (implements$1(dst, src)) {
			if (src.Kind() === 20) {
				return cvtI2I;
			}
			return cvtT2I;
		}
		return $throwNilPointerError;
		/* */ } return; } if ($f === undefined) { $f = { $blk: convertOp }; } $f.$ptr = $ptr; $f._23 = _23; $f._24 = _24; $f._25 = _25; $f._26 = _26; $f._27 = _27; $f._28 = _28; $f._29 = _29; $f._arg = _arg; $f._arg$1 = _arg$1; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._v = _v; $f._v$1 = _v$1; $f._v$2 = _v$2; $f.dst = dst; $f.src = src; $f.$s = $s; $f.$r = $r; return $f;
	};
	makeFloat = function(f, v, t) {
		var $ptr, _31, _r, f, ptr, t, typ, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _31 = $f._31; _r = $f._r; f = $f.f; ptr = $f.ptr; t = $f.t; typ = $f.typ; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = t.common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		typ = _r;
		ptr = unsafe_New(typ);
		_31 = typ.size;
		if (_31 === (4)) {
			ptr.$set($fround(v));
		} else if (_31 === (8)) {
			ptr.$set(v);
		}
		return new Value.ptr(typ, ptr, (((f | 128) >>> 0) | (typ.Kind() >>> 0)) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeFloat }; } $f.$ptr = $ptr; $f._31 = _31; $f._r = _r; $f.f = f; $f.ptr = ptr; $f.t = t; $f.typ = typ; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	makeComplex = function(f, v, t) {
		var $ptr, _32, _r, f, ptr, t, typ, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _32 = $f._32; _r = $f._r; f = $f.f; ptr = $f.ptr; t = $f.t; typ = $f.typ; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = t.common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		typ = _r;
		ptr = unsafe_New(typ);
		_32 = typ.size;
		if (_32 === (8)) {
			ptr.$set(new $Complex64(v.$real, v.$imag));
		} else if (_32 === (16)) {
			ptr.$set(v);
		}
		return new Value.ptr(typ, ptr, (((f | 128) >>> 0) | (typ.Kind() >>> 0)) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeComplex }; } $f.$ptr = $ptr; $f._32 = _32; $f._r = _r; $f.f = f; $f.ptr = ptr; $f.t = t; $f.typ = typ; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	makeString = function(f, v, t) {
		var $ptr, _r, _r$1, f, ret, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; f = $f.f; ret = $f.ret; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = New(t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r$1 = _r.Elem(); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		ret = _r$1;
		ret.SetString(v);
		ret.flag = (((ret.flag & ~256) >>> 0) | f) >>> 0;
		return ret;
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeString }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.f = f; $f.ret = ret; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	makeBytes = function(f, v, t) {
		var $ptr, _r, _r$1, f, ret, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; f = $f.f; ret = $f.ret; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = New(t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r$1 = _r.Elem(); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		ret = _r$1;
		$r = ret.SetBytes(v); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		ret.flag = (((ret.flag & ~256) >>> 0) | f) >>> 0;
		return ret;
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeBytes }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.f = f; $f.ret = ret; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	makeRunes = function(f, v, t) {
		var $ptr, _r, _r$1, f, ret, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; f = $f.f; ret = $f.ret; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = New(t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r$1 = _r.Elem(); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		ret = _r$1;
		$r = ret.setRunes(v); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		ret.flag = (((ret.flag & ~256) >>> 0) | f) >>> 0;
		return ret;
		/* */ } return; } if ($f === undefined) { $f = { $blk: makeRunes }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f.f = f; $f.ret = ret; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtInt = function(v, t) {
		var $ptr, _r, t, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeInt((v.flag & 96) >>> 0, (x = v.Int(), new $Uint64(x.$high, x.$low)), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtInt }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtUint = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeInt((v.flag & 96) >>> 0, v.Uint(), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtUint }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtFloatInt = function(v, t) {
		var $ptr, _r, t, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeInt((v.flag & 96) >>> 0, (x = new $Int64(0, v.Float()), new $Uint64(x.$high, x.$low)), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtFloatInt }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtFloatUint = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeInt((v.flag & 96) >>> 0, new $Uint64(0, v.Float()), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtFloatUint }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtIntFloat = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeFloat((v.flag & 96) >>> 0, $flatten64(v.Int()), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtIntFloat }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtUintFloat = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeFloat((v.flag & 96) >>> 0, $flatten64(v.Uint()), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtUintFloat }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtFloat = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeFloat((v.flag & 96) >>> 0, v.Float(), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtFloat }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtComplex = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeComplex((v.flag & 96) >>> 0, v.Complex(), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtComplex }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtIntString = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeString((v.flag & 96) >>> 0, $encodeRune(v.Int().$low), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtIntString }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtUintString = function(v, t) {
		var $ptr, _r, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = makeString((v.flag & 96) >>> 0, $encodeRune(v.Uint().$low), t); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtUintString }; } $f.$ptr = $ptr; $f._r = _r; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtBytesString = function(v, t) {
		var $ptr, _arg, _arg$1, _arg$2, _r, _r$1, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _r = $f._r; _r$1 = $f._r$1; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_arg = (v.flag & 96) >>> 0;
		_r = v.Bytes(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_arg$1 = $bytesToString(_r);
		_arg$2 = t;
		_r$1 = makeString(_arg, _arg$1, _arg$2); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtBytesString }; } $f.$ptr = $ptr; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._r = _r; $f._r$1 = _r$1; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtStringBytes = function(v, t) {
		var $ptr, _arg, _arg$1, _arg$2, _r, _r$1, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _r = $f._r; _r$1 = $f._r$1; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_arg = (v.flag & 96) >>> 0;
		_r = v.String(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_arg$1 = new sliceType$13($stringToBytes(_r));
		_arg$2 = t;
		_r$1 = makeBytes(_arg, _arg$1, _arg$2); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtStringBytes }; } $f.$ptr = $ptr; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._r = _r; $f._r$1 = _r$1; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtRunesString = function(v, t) {
		var $ptr, _arg, _arg$1, _arg$2, _r, _r$1, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _r = $f._r; _r$1 = $f._r$1; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_arg = (v.flag & 96) >>> 0;
		_r = v.runes(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_arg$1 = $runesToString(_r);
		_arg$2 = t;
		_r$1 = makeString(_arg, _arg$1, _arg$2); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtRunesString }; } $f.$ptr = $ptr; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._r = _r; $f._r$1 = _r$1; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtStringRunes = function(v, t) {
		var $ptr, _arg, _arg$1, _arg$2, _r, _r$1, t, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _r = $f._r; _r$1 = $f._r$1; t = $f.t; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_arg = (v.flag & 96) >>> 0;
		_r = v.String(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_arg$1 = new sliceType$15($stringToRunes(_r));
		_arg$2 = t;
		_r$1 = makeRunes(_arg, _arg$1, _arg$2); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtStringRunes }; } $f.$ptr = $ptr; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._r = _r; $f._r$1 = _r$1; $f.t = t; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtT2I = function(v, typ) {
		var $ptr, _r, _r$1, _r$2, _r$3, _r$4, target, typ, v, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; target = $f.target; typ = $f.typ; v = $f.v; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r = typ.common(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r$1 = unsafe_New(_r); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		target = _r$1;
		_r$2 = valueInterface(v, false); /* */ $s = 3; case 3: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		x = _r$2;
		_r$3 = typ.NumMethod(); /* */ $s = 7; case 7: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		/* */ if (_r$3 === 0) { $s = 4; continue; }
		/* */ $s = 5; continue;
		/* if (_r$3 === 0) { */ case 4:
			target.$set(x);
			$s = 6; continue;
		/* } else { */ case 5:
			ifaceE2I($assertType(typ, ptrType$1), x, target);
		/* } */ case 6:
		_r$4 = typ.common(); /* */ $s = 8; case 8: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
		/* */ $s = 9; case 9:
		return new Value.ptr(_r$4, target, (((((v.flag & 96) >>> 0) | 128) >>> 0) | 20) >>> 0);
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtT2I }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f.target = target; $f.typ = typ; $f.v = v; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	cvtI2I = function(v, typ) {
		var $ptr, _r, _r$1, _r$2, ret, typ, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; ret = $f.ret; typ = $f.typ; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		/* */ if (v.IsNil()) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (v.IsNil()) { */ case 1:
			_r = Zero(typ); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			ret = _r;
			ret.flag = (ret.flag | (((v.flag & 96) >>> 0))) >>> 0;
			return ret;
		/* } */ case 2:
		_r$1 = v.Elem(); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_r$2 = cvtT2I(_r$1, typ); /* */ $s = 5; case 5: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		/* */ $s = 6; case 6:
		return _r$2;
		/* */ } return; } if ($f === undefined) { $f = { $blk: cvtI2I }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.ret = ret; $f.typ = typ; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	Kind.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$1.methods = [{prop: "ptrTo", name: "ptrTo", pkg: "reflect", typ: $funcType([], [ptrType$1], false)}, {prop: "pointers", name: "pointers", pkg: "reflect", typ: $funcType([], [$Bool], false)}, {prop: "Comparable", name: "Comparable", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Size", name: "Size", pkg: "", typ: $funcType([], [$Uintptr], false)}, {prop: "Bits", name: "Bits", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Align", name: "Align", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "FieldAlign", name: "FieldAlign", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Kind", name: "Kind", pkg: "", typ: $funcType([], [Kind], false)}, {prop: "common", name: "common", pkg: "reflect", typ: $funcType([], [ptrType$1], false)}, {prop: "NumMethod", name: "NumMethod", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Method", name: "Method", pkg: "", typ: $funcType([$Int], [Method], false)}, {prop: "MethodByName", name: "MethodByName", pkg: "", typ: $funcType([$String], [Method, $Bool], false)}, {prop: "PkgPath", name: "PkgPath", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Name", name: "Name", pkg: "", typ: $funcType([], [$String], false)}, {prop: "ChanDir", name: "ChanDir", pkg: "", typ: $funcType([], [ChanDir], false)}, {prop: "IsVariadic", name: "IsVariadic", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Elem", name: "Elem", pkg: "", typ: $funcType([], [Type], false)}, {prop: "Field", name: "Field", pkg: "", typ: $funcType([$Int], [StructField], false)}, {prop: "FieldByIndex", name: "FieldByIndex", pkg: "", typ: $funcType([sliceType$11], [StructField], false)}, {prop: "FieldByName", name: "FieldByName", pkg: "", typ: $funcType([$String], [StructField, $Bool], false)}, {prop: "FieldByNameFunc", name: "FieldByNameFunc", pkg: "", typ: $funcType([funcType$3], [StructField, $Bool], false)}, {prop: "In", name: "In", pkg: "", typ: $funcType([$Int], [Type], false)}, {prop: "Key", name: "Key", pkg: "", typ: $funcType([], [Type], false)}, {prop: "Len", name: "Len", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "NumField", name: "NumField", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "NumIn", name: "NumIn", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "NumOut", name: "NumOut", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Out", name: "Out", pkg: "", typ: $funcType([$Int], [Type], false)}, {prop: "Implements", name: "Implements", pkg: "", typ: $funcType([Type], [$Bool], false)}, {prop: "AssignableTo", name: "AssignableTo", pkg: "", typ: $funcType([Type], [$Bool], false)}, {prop: "ConvertibleTo", name: "ConvertibleTo", pkg: "", typ: $funcType([Type], [$Bool], false)}];
	ptrType$7.methods = [{prop: "Method", name: "Method", pkg: "", typ: $funcType([$Int], [Method], false)}, {prop: "uncommon", name: "uncommon", pkg: "reflect", typ: $funcType([], [ptrType$7], false)}, {prop: "PkgPath", name: "PkgPath", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Name", name: "Name", pkg: "", typ: $funcType([], [$String], false)}, {prop: "NumMethod", name: "NumMethod", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "MethodByName", name: "MethodByName", pkg: "", typ: $funcType([$String], [Method, $Bool], false)}];
	ChanDir.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$11.methods = [{prop: "Method", name: "Method", pkg: "", typ: $funcType([$Int], [Method], false)}, {prop: "NumMethod", name: "NumMethod", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "MethodByName", name: "MethodByName", pkg: "", typ: $funcType([$String], [Method, $Bool], false)}];
	ptrType$13.methods = [{prop: "Field", name: "Field", pkg: "", typ: $funcType([$Int], [StructField], false)}, {prop: "FieldByIndex", name: "FieldByIndex", pkg: "", typ: $funcType([sliceType$11], [StructField], false)}, {prop: "FieldByNameFunc", name: "FieldByNameFunc", pkg: "", typ: $funcType([funcType$3], [StructField, $Bool], false)}, {prop: "FieldByName", name: "FieldByName", pkg: "", typ: $funcType([$String], [StructField, $Bool], false)}];
	StructTag.methods = [{prop: "Get", name: "Get", pkg: "", typ: $funcType([$String], [$String], false)}];
	Value.methods = [{prop: "object", name: "object", pkg: "reflect", typ: $funcType([], [ptrType$3], false)}, {prop: "call", name: "call", pkg: "reflect", typ: $funcType([$String, sliceType$8], [sliceType$8], false)}, {prop: "Cap", name: "Cap", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Elem", name: "Elem", pkg: "", typ: $funcType([], [Value], false)}, {prop: "Field", name: "Field", pkg: "", typ: $funcType([$Int], [Value], false)}, {prop: "Index", name: "Index", pkg: "", typ: $funcType([$Int], [Value], false)}, {prop: "InterfaceData", name: "InterfaceData", pkg: "", typ: $funcType([], [arrayType$3], false)}, {prop: "IsNil", name: "IsNil", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Len", name: "Len", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Pointer", name: "Pointer", pkg: "", typ: $funcType([], [$Uintptr], false)}, {prop: "Set", name: "Set", pkg: "", typ: $funcType([Value], [], false)}, {prop: "SetBytes", name: "SetBytes", pkg: "", typ: $funcType([sliceType$13], [], false)}, {prop: "SetCap", name: "SetCap", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "SetLen", name: "SetLen", pkg: "", typ: $funcType([$Int], [], false)}, {prop: "Slice", name: "Slice", pkg: "", typ: $funcType([$Int, $Int], [Value], false)}, {prop: "Slice3", name: "Slice3", pkg: "", typ: $funcType([$Int, $Int, $Int], [Value], false)}, {prop: "Close", name: "Close", pkg: "", typ: $funcType([], [], false)}, {prop: "pointer", name: "pointer", pkg: "reflect", typ: $funcType([], [$UnsafePointer], false)}, {prop: "Addr", name: "Addr", pkg: "", typ: $funcType([], [Value], false)}, {prop: "Bool", name: "Bool", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Bytes", name: "Bytes", pkg: "", typ: $funcType([], [sliceType$13], false)}, {prop: "runes", name: "runes", pkg: "reflect", typ: $funcType([], [sliceType$15], false)}, {prop: "CanAddr", name: "CanAddr", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "CanSet", name: "CanSet", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Call", name: "Call", pkg: "", typ: $funcType([sliceType$8], [sliceType$8], false)}, {prop: "CallSlice", name: "CallSlice", pkg: "", typ: $funcType([sliceType$8], [sliceType$8], false)}, {prop: "Complex", name: "Complex", pkg: "", typ: $funcType([], [$Complex128], false)}, {prop: "FieldByIndex", name: "FieldByIndex", pkg: "", typ: $funcType([sliceType$11], [Value], false)}, {prop: "FieldByName", name: "FieldByName", pkg: "", typ: $funcType([$String], [Value], false)}, {prop: "FieldByNameFunc", name: "FieldByNameFunc", pkg: "", typ: $funcType([funcType$3], [Value], false)}, {prop: "Float", name: "Float", pkg: "", typ: $funcType([], [$Float64], false)}, {prop: "Int", name: "Int", pkg: "", typ: $funcType([], [$Int64], false)}, {prop: "CanInterface", name: "CanInterface", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Interface", name: "Interface", pkg: "", typ: $funcType([], [$emptyInterface], false)}, {prop: "IsValid", name: "IsValid", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Kind", name: "Kind", pkg: "", typ: $funcType([], [Kind], false)}, {prop: "MapIndex", name: "MapIndex", pkg: "", typ: $funcType([Value], [Value], false)}, {prop: "MapKeys", name: "MapKeys", pkg: "", typ: $funcType([], [sliceType$8], false)}, {prop: "Method", name: "Method", pkg: "", typ: $funcType([$Int], [Value], false)}, {prop: "NumMethod", name: "NumMethod", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "MethodByName", name: "MethodByName", pkg: "", typ: $funcType([$String], [Value], false)}, {prop: "NumField", name: "NumField", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "OverflowComplex", name: "OverflowComplex", pkg: "", typ: $funcType([$Complex128], [$Bool], false)}, {prop: "OverflowFloat", name: "OverflowFloat", pkg: "", typ: $funcType([$Float64], [$Bool], false)}, {prop: "OverflowInt", name: "OverflowInt", pkg: "", typ: $funcType([$Int64], [$Bool], false)}, {prop: "OverflowUint", name: "OverflowUint", pkg: "", typ: $funcType([$Uint64], [$Bool], false)}, {prop: "Recv", name: "Recv", pkg: "", typ: $funcType([], [Value, $Bool], false)}, {prop: "recv", name: "recv", pkg: "reflect", typ: $funcType([$Bool], [Value, $Bool], false)}, {prop: "Send", name: "Send", pkg: "", typ: $funcType([Value], [], false)}, {prop: "send", name: "send", pkg: "reflect", typ: $funcType([Value, $Bool], [$Bool], false)}, {prop: "SetBool", name: "SetBool", pkg: "", typ: $funcType([$Bool], [], false)}, {prop: "setRunes", name: "setRunes", pkg: "reflect", typ: $funcType([sliceType$15], [], false)}, {prop: "SetComplex", name: "SetComplex", pkg: "", typ: $funcType([$Complex128], [], false)}, {prop: "SetFloat", name: "SetFloat", pkg: "", typ: $funcType([$Float64], [], false)}, {prop: "SetInt", name: "SetInt", pkg: "", typ: $funcType([$Int64], [], false)}, {prop: "SetMapIndex", name: "SetMapIndex", pkg: "", typ: $funcType([Value, Value], [], false)}, {prop: "SetUint", name: "SetUint", pkg: "", typ: $funcType([$Uint64], [], false)}, {prop: "SetPointer", name: "SetPointer", pkg: "", typ: $funcType([$UnsafePointer], [], false)}, {prop: "SetString", name: "SetString", pkg: "", typ: $funcType([$String], [], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "TryRecv", name: "TryRecv", pkg: "", typ: $funcType([], [Value, $Bool], false)}, {prop: "TrySend", name: "TrySend", pkg: "", typ: $funcType([Value], [$Bool], false)}, {prop: "Type", name: "Type", pkg: "", typ: $funcType([], [Type], false)}, {prop: "Uint", name: "Uint", pkg: "", typ: $funcType([], [$Uint64], false)}, {prop: "UnsafeAddr", name: "UnsafeAddr", pkg: "", typ: $funcType([], [$Uintptr], false)}, {prop: "assignTo", name: "assignTo", pkg: "reflect", typ: $funcType([$String, ptrType$1, $UnsafePointer], [Value], false)}, {prop: "Convert", name: "Convert", pkg: "", typ: $funcType([Type], [Value], false)}];
	flag.methods = [{prop: "kind", name: "kind", pkg: "reflect", typ: $funcType([], [Kind], false)}, {prop: "mustBe", name: "mustBe", pkg: "reflect", typ: $funcType([Kind], [], false)}, {prop: "mustBeExported", name: "mustBeExported", pkg: "reflect", typ: $funcType([], [], false)}, {prop: "mustBeAssignable", name: "mustBeAssignable", pkg: "reflect", typ: $funcType([], [], false)}];
	ptrType$20.methods = [{prop: "Error", name: "Error", pkg: "", typ: $funcType([], [$String], false)}];
	mapIter.init([{prop: "t", name: "t", pkg: "reflect", typ: Type, tag: ""}, {prop: "m", name: "m", pkg: "reflect", typ: ptrType$3, tag: ""}, {prop: "keys", name: "keys", pkg: "reflect", typ: ptrType$3, tag: ""}, {prop: "i", name: "i", pkg: "reflect", typ: $Int, tag: ""}]);
	Type.init([{prop: "Align", name: "Align", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "AssignableTo", name: "AssignableTo", pkg: "", typ: $funcType([Type], [$Bool], false)}, {prop: "Bits", name: "Bits", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "ChanDir", name: "ChanDir", pkg: "", typ: $funcType([], [ChanDir], false)}, {prop: "Comparable", name: "Comparable", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "ConvertibleTo", name: "ConvertibleTo", pkg: "", typ: $funcType([Type], [$Bool], false)}, {prop: "Elem", name: "Elem", pkg: "", typ: $funcType([], [Type], false)}, {prop: "Field", name: "Field", pkg: "", typ: $funcType([$Int], [StructField], false)}, {prop: "FieldAlign", name: "FieldAlign", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "FieldByIndex", name: "FieldByIndex", pkg: "", typ: $funcType([sliceType$11], [StructField], false)}, {prop: "FieldByName", name: "FieldByName", pkg: "", typ: $funcType([$String], [StructField, $Bool], false)}, {prop: "FieldByNameFunc", name: "FieldByNameFunc", pkg: "", typ: $funcType([funcType$3], [StructField, $Bool], false)}, {prop: "Implements", name: "Implements", pkg: "", typ: $funcType([Type], [$Bool], false)}, {prop: "In", name: "In", pkg: "", typ: $funcType([$Int], [Type], false)}, {prop: "IsVariadic", name: "IsVariadic", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Key", name: "Key", pkg: "", typ: $funcType([], [Type], false)}, {prop: "Kind", name: "Kind", pkg: "", typ: $funcType([], [Kind], false)}, {prop: "Len", name: "Len", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Method", name: "Method", pkg: "", typ: $funcType([$Int], [Method], false)}, {prop: "MethodByName", name: "MethodByName", pkg: "", typ: $funcType([$String], [Method, $Bool], false)}, {prop: "Name", name: "Name", pkg: "", typ: $funcType([], [$String], false)}, {prop: "NumField", name: "NumField", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "NumIn", name: "NumIn", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "NumMethod", name: "NumMethod", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "NumOut", name: "NumOut", pkg: "", typ: $funcType([], [$Int], false)}, {prop: "Out", name: "Out", pkg: "", typ: $funcType([$Int], [Type], false)}, {prop: "PkgPath", name: "PkgPath", pkg: "", typ: $funcType([], [$String], false)}, {prop: "Size", name: "Size", pkg: "", typ: $funcType([], [$Uintptr], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "common", name: "common", pkg: "reflect", typ: $funcType([], [ptrType$1], false)}, {prop: "uncommon", name: "uncommon", pkg: "reflect", typ: $funcType([], [ptrType$7], false)}]);
	rtype.init([{prop: "size", name: "size", pkg: "reflect", typ: $Uintptr, tag: ""}, {prop: "ptrdata", name: "ptrdata", pkg: "reflect", typ: $Uintptr, tag: ""}, {prop: "hash", name: "hash", pkg: "reflect", typ: $Uint32, tag: ""}, {prop: "_$3", name: "_", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "align", name: "align", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "fieldAlign", name: "fieldAlign", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "kind", name: "kind", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "alg", name: "alg", pkg: "reflect", typ: ptrType$4, tag: ""}, {prop: "gcdata", name: "gcdata", pkg: "reflect", typ: ptrType$5, tag: ""}, {prop: "string", name: "string", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "uncommonType", name: "", pkg: "reflect", typ: ptrType$7, tag: ""}, {prop: "ptrToThis", name: "ptrToThis", pkg: "reflect", typ: ptrType$1, tag: ""}]);
	typeAlg.init([{prop: "hash", name: "hash", pkg: "reflect", typ: funcType$4, tag: ""}, {prop: "equal", name: "equal", pkg: "reflect", typ: funcType$5, tag: ""}]);
	method.init([{prop: "name", name: "name", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "pkgPath", name: "pkgPath", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "mtyp", name: "mtyp", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "typ", name: "typ", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "ifn", name: "ifn", pkg: "reflect", typ: $UnsafePointer, tag: ""}, {prop: "tfn", name: "tfn", pkg: "reflect", typ: $UnsafePointer, tag: ""}]);
	uncommonType.init([{prop: "name", name: "name", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "pkgPath", name: "pkgPath", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "methods", name: "methods", pkg: "reflect", typ: sliceType$4, tag: ""}]);
	arrayType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"array\""}, {prop: "elem", name: "elem", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "slice", name: "slice", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "len", name: "len", pkg: "reflect", typ: $Uintptr, tag: ""}]);
	chanType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"chan\""}, {prop: "elem", name: "elem", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "dir", name: "dir", pkg: "reflect", typ: $Uintptr, tag: ""}]);
	funcType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"func\""}, {prop: "dotdotdot", name: "dotdotdot", pkg: "reflect", typ: $Bool, tag: ""}, {prop: "in$2", name: "in", pkg: "reflect", typ: sliceType$1, tag: ""}, {prop: "out", name: "out", pkg: "reflect", typ: sliceType$1, tag: ""}]);
	imethod.init([{prop: "name", name: "name", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "pkgPath", name: "pkgPath", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "typ", name: "typ", pkg: "reflect", typ: ptrType$1, tag: ""}]);
	interfaceType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"interface\""}, {prop: "methods", name: "methods", pkg: "reflect", typ: sliceType$5, tag: ""}]);
	mapType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"map\""}, {prop: "key", name: "key", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "elem", name: "elem", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "bucket", name: "bucket", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "hmap", name: "hmap", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "keysize", name: "keysize", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "indirectkey", name: "indirectkey", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "valuesize", name: "valuesize", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "indirectvalue", name: "indirectvalue", pkg: "reflect", typ: $Uint8, tag: ""}, {prop: "bucketsize", name: "bucketsize", pkg: "reflect", typ: $Uint16, tag: ""}, {prop: "reflexivekey", name: "reflexivekey", pkg: "reflect", typ: $Bool, tag: ""}, {prop: "needkeyupdate", name: "needkeyupdate", pkg: "reflect", typ: $Bool, tag: ""}]);
	ptrType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"ptr\""}, {prop: "elem", name: "elem", pkg: "reflect", typ: ptrType$1, tag: ""}]);
	sliceType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"slice\""}, {prop: "elem", name: "elem", pkg: "reflect", typ: ptrType$1, tag: ""}]);
	structField.init([{prop: "name", name: "name", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "pkgPath", name: "pkgPath", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "typ", name: "typ", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "tag", name: "tag", pkg: "reflect", typ: ptrType$6, tag: ""}, {prop: "offset", name: "offset", pkg: "reflect", typ: $Uintptr, tag: ""}]);
	structType.init([{prop: "rtype", name: "", pkg: "reflect", typ: rtype, tag: "reflect:\"struct\""}, {prop: "fields", name: "fields", pkg: "reflect", typ: sliceType$6, tag: ""}]);
	Method.init([{prop: "Name", name: "Name", pkg: "", typ: $String, tag: ""}, {prop: "PkgPath", name: "PkgPath", pkg: "", typ: $String, tag: ""}, {prop: "Type", name: "Type", pkg: "", typ: Type, tag: ""}, {prop: "Func", name: "Func", pkg: "", typ: Value, tag: ""}, {prop: "Index", name: "Index", pkg: "", typ: $Int, tag: ""}]);
	StructField.init([{prop: "Name", name: "Name", pkg: "", typ: $String, tag: ""}, {prop: "PkgPath", name: "PkgPath", pkg: "", typ: $String, tag: ""}, {prop: "Type", name: "Type", pkg: "", typ: Type, tag: ""}, {prop: "Tag", name: "Tag", pkg: "", typ: StructTag, tag: ""}, {prop: "Offset", name: "Offset", pkg: "", typ: $Uintptr, tag: ""}, {prop: "Index", name: "Index", pkg: "", typ: sliceType$11, tag: ""}, {prop: "Anonymous", name: "Anonymous", pkg: "", typ: $Bool, tag: ""}]);
	fieldScan.init([{prop: "typ", name: "typ", pkg: "reflect", typ: ptrType$13, tag: ""}, {prop: "index", name: "index", pkg: "reflect", typ: sliceType$11, tag: ""}]);
	Value.init([{prop: "typ", name: "typ", pkg: "reflect", typ: ptrType$1, tag: ""}, {prop: "ptr", name: "ptr", pkg: "reflect", typ: $UnsafePointer, tag: ""}, {prop: "flag", name: "", pkg: "reflect", typ: flag, tag: ""}]);
	ValueError.init([{prop: "Method", name: "Method", pkg: "", typ: $String, tag: ""}, {prop: "Kind", name: "Kind", pkg: "", typ: Kind, tag: ""}]);
	nonEmptyInterface.init([{prop: "itab", name: "itab", pkg: "reflect", typ: ptrType$9, tag: ""}, {prop: "word", name: "word", pkg: "reflect", typ: $UnsafePointer, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = js.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = math.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = runtime.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = strconv.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sync.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		initialized = false;
		stringPtrMap = {};
		callHelper = $assertType($internalize($call, $emptyInterface), funcType$1);
		selectHelper = $assertType($internalize($select, $emptyInterface), funcType$1);
		kindNames = new sliceType$3(["invalid", "bool", "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "uintptr", "float32", "float64", "complex64", "complex128", "array", "chan", "func", "interface", "map", "ptr", "slice", "string", "struct", "unsafe.Pointer"]);
		jsObjectPtr = reflectType($jsObjectPtr);
		uint8Type = $assertType(TypeOf(new $Uint8(0)), ptrType$1);
		$r = init(); /* */ $s = 7; case 7: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["fmt"] = (function() {
	var $pkg = {}, $init, errors, io, math, os, reflect, strconv, sync, utf8, fmtFlags, fmt, State, Formatter, Stringer, GoStringer, buffer, pp, runeUnreader, scanError, ss, ssave, sliceType, sliceType$1, ptrType, arrayType, arrayType$1, ptrType$1, arrayType$2, sliceType$2, ptrType$2, ptrType$5, ptrType$25, funcType, padZeroBytes, padSpaceBytes, trueBytes, falseBytes, commaSpaceBytes, nilAngleBytes, nilParenBytes, nilBytes, mapBytes, percentBangBytes, missingBytes, badIndexBytes, panicBytes, extraBytes, irparenBytes, bytesBytes, badWidthBytes, badPrecBytes, noVerbBytes, ppFree, intBits, uintptrBits, byteType, space, ssFree, complexError, boolError, _r, _r$1, init, doPrec, newPrinter, Fprintf, Printf, Sprintf, getField, tooLarge, parsenum, intFromArg, parseArgNumber, isSpace, notSpace, indexRune;
	errors = $packages["errors"];
	io = $packages["io"];
	math = $packages["math"];
	os = $packages["os"];
	reflect = $packages["reflect"];
	strconv = $packages["strconv"];
	sync = $packages["sync"];
	utf8 = $packages["unicode/utf8"];
	fmtFlags = $pkg.fmtFlags = $newType(0, $kindStruct, "fmt.fmtFlags", "fmtFlags", "fmt", function(widPresent_, precPresent_, minus_, plus_, sharp_, space_, unicode_, uniQuote_, zero_, plusV_, sharpV_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.widPresent = false;
			this.precPresent = false;
			this.minus = false;
			this.plus = false;
			this.sharp = false;
			this.space = false;
			this.unicode = false;
			this.uniQuote = false;
			this.zero = false;
			this.plusV = false;
			this.sharpV = false;
			return;
		}
		this.widPresent = widPresent_;
		this.precPresent = precPresent_;
		this.minus = minus_;
		this.plus = plus_;
		this.sharp = sharp_;
		this.space = space_;
		this.unicode = unicode_;
		this.uniQuote = uniQuote_;
		this.zero = zero_;
		this.plusV = plusV_;
		this.sharpV = sharpV_;
	});
	fmt = $pkg.fmt = $newType(0, $kindStruct, "fmt.fmt", "fmt", "fmt", function(intbuf_, buf_, wid_, prec_, fmtFlags_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.intbuf = arrayType$1.zero();
			this.buf = ptrType$1.nil;
			this.wid = 0;
			this.prec = 0;
			this.fmtFlags = new fmtFlags.ptr(false, false, false, false, false, false, false, false, false, false, false);
			return;
		}
		this.intbuf = intbuf_;
		this.buf = buf_;
		this.wid = wid_;
		this.prec = prec_;
		this.fmtFlags = fmtFlags_;
	});
	State = $pkg.State = $newType(8, $kindInterface, "fmt.State", "State", "fmt", null);
	Formatter = $pkg.Formatter = $newType(8, $kindInterface, "fmt.Formatter", "Formatter", "fmt", null);
	Stringer = $pkg.Stringer = $newType(8, $kindInterface, "fmt.Stringer", "Stringer", "fmt", null);
	GoStringer = $pkg.GoStringer = $newType(8, $kindInterface, "fmt.GoStringer", "GoStringer", "fmt", null);
	buffer = $pkg.buffer = $newType(12, $kindSlice, "fmt.buffer", "buffer", "fmt", null);
	pp = $pkg.pp = $newType(0, $kindStruct, "fmt.pp", "pp", "fmt", function(n_, panicking_, erroring_, buf_, arg_, value_, reordered_, goodArgNum_, runeBuf_, fmt_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.n = 0;
			this.panicking = false;
			this.erroring = false;
			this.buf = buffer.nil;
			this.arg = $ifaceNil;
			this.value = new reflect.Value.ptr(ptrType.nil, 0, 0);
			this.reordered = false;
			this.goodArgNum = false;
			this.runeBuf = arrayType.zero();
			this.fmt = new fmt.ptr(arrayType$1.zero(), ptrType$1.nil, 0, 0, new fmtFlags.ptr(false, false, false, false, false, false, false, false, false, false, false));
			return;
		}
		this.n = n_;
		this.panicking = panicking_;
		this.erroring = erroring_;
		this.buf = buf_;
		this.arg = arg_;
		this.value = value_;
		this.reordered = reordered_;
		this.goodArgNum = goodArgNum_;
		this.runeBuf = runeBuf_;
		this.fmt = fmt_;
	});
	runeUnreader = $pkg.runeUnreader = $newType(8, $kindInterface, "fmt.runeUnreader", "runeUnreader", "fmt", null);
	scanError = $pkg.scanError = $newType(0, $kindStruct, "fmt.scanError", "scanError", "fmt", function(err_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.err = $ifaceNil;
			return;
		}
		this.err = err_;
	});
	ss = $pkg.ss = $newType(0, $kindStruct, "fmt.ss", "ss", "fmt", function(rr_, buf_, peekRune_, prevRune_, count_, atEOF_, ssave_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.rr = $ifaceNil;
			this.buf = buffer.nil;
			this.peekRune = 0;
			this.prevRune = 0;
			this.count = 0;
			this.atEOF = false;
			this.ssave = new ssave.ptr(false, false, false, 0, 0, 0);
			return;
		}
		this.rr = rr_;
		this.buf = buf_;
		this.peekRune = peekRune_;
		this.prevRune = prevRune_;
		this.count = count_;
		this.atEOF = atEOF_;
		this.ssave = ssave_;
	});
	ssave = $pkg.ssave = $newType(0, $kindStruct, "fmt.ssave", "ssave", "fmt", function(validSave_, nlIsEnd_, nlIsSpace_, argLimit_, limit_, maxWid_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.validSave = false;
			this.nlIsEnd = false;
			this.nlIsSpace = false;
			this.argLimit = 0;
			this.limit = 0;
			this.maxWid = 0;
			return;
		}
		this.validSave = validSave_;
		this.nlIsEnd = nlIsEnd_;
		this.nlIsSpace = nlIsSpace_;
		this.argLimit = argLimit_;
		this.limit = limit_;
		this.maxWid = maxWid_;
	});
	sliceType = $sliceType($Uint8);
	sliceType$1 = $sliceType($emptyInterface);
	ptrType = $ptrType(reflect.rtype);
	arrayType = $arrayType($Uint8, 4);
	arrayType$1 = $arrayType($Uint8, 65);
	ptrType$1 = $ptrType(buffer);
	arrayType$2 = $arrayType($Uint16, 2);
	sliceType$2 = $sliceType(arrayType$2);
	ptrType$2 = $ptrType(pp);
	ptrType$5 = $ptrType(ss);
	ptrType$25 = $ptrType(fmt);
	funcType = $funcType([$Int32], [$Bool], false);
	init = function() {
		var $ptr, i;
		i = 0;
		while (true) {
			if (!(i < 65)) { break; }
			((i < 0 || i >= padZeroBytes.$length) ? $throwRuntimeError("index out of range") : padZeroBytes.$array[padZeroBytes.$offset + i] = 48);
			((i < 0 || i >= padSpaceBytes.$length) ? $throwRuntimeError("index out of range") : padSpaceBytes.$array[padSpaceBytes.$offset + i] = 32);
			i = i + (1) >> 0;
		}
	};
	fmt.ptr.prototype.clearflags = function() {
		var $ptr, f;
		f = this;
		fmtFlags.copy(f.fmtFlags, new fmtFlags.ptr(false, false, false, false, false, false, false, false, false, false, false));
	};
	fmt.prototype.clearflags = function() { return this.$val.clearflags(); };
	fmt.ptr.prototype.init = function(buf) {
		var $ptr, buf, f;
		f = this;
		f.buf = buf;
		f.clearflags();
	};
	fmt.prototype.init = function(buf) { return this.$val.init(buf); };
	fmt.ptr.prototype.computePadding = function(width) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, f, left, leftWidth, padding, rightWidth, w, width;
		padding = sliceType.nil;
		leftWidth = 0;
		rightWidth = 0;
		f = this;
		left = !f.fmtFlags.minus;
		w = f.wid;
		if (w < 0) {
			left = false;
			w = -w;
		}
		w = w - (width) >> 0;
		if (w > 0) {
			if (left && f.fmtFlags.zero) {
				_tmp = padZeroBytes;
				_tmp$1 = w;
				_tmp$2 = 0;
				padding = _tmp;
				leftWidth = _tmp$1;
				rightWidth = _tmp$2;
				return [padding, leftWidth, rightWidth];
			}
			if (left) {
				_tmp$3 = padSpaceBytes;
				_tmp$4 = w;
				_tmp$5 = 0;
				padding = _tmp$3;
				leftWidth = _tmp$4;
				rightWidth = _tmp$5;
				return [padding, leftWidth, rightWidth];
			} else {
				_tmp$6 = padSpaceBytes;
				_tmp$7 = 0;
				_tmp$8 = w;
				padding = _tmp$6;
				leftWidth = _tmp$7;
				rightWidth = _tmp$8;
				return [padding, leftWidth, rightWidth];
			}
		}
		return [padding, leftWidth, rightWidth];
	};
	fmt.prototype.computePadding = function(width) { return this.$val.computePadding(width); };
	fmt.ptr.prototype.writePadding = function(n, padding) {
		var $ptr, f, m, n, padding;
		f = this;
		while (true) {
			if (!(n > 0)) { break; }
			m = n;
			if (m > 65) {
				m = 65;
			}
			f.buf.Write($subslice(padding, 0, m));
			n = n - (m) >> 0;
		}
	};
	fmt.prototype.writePadding = function(n, padding) { return this.$val.writePadding(n, padding); };
	fmt.ptr.prototype.pad = function(b) {
		var $ptr, _tuple, b, f, left, padding, right;
		f = this;
		if (!f.fmtFlags.widPresent || (f.wid === 0)) {
			f.buf.Write(b);
			return;
		}
		_tuple = f.computePadding(utf8.RuneCount(b));
		padding = _tuple[0];
		left = _tuple[1];
		right = _tuple[2];
		if (left > 0) {
			f.writePadding(left, padding);
		}
		f.buf.Write(b);
		if (right > 0) {
			f.writePadding(right, padding);
		}
	};
	fmt.prototype.pad = function(b) { return this.$val.pad(b); };
	fmt.ptr.prototype.padString = function(s) {
		var $ptr, _tuple, f, left, padding, right, s;
		f = this;
		if (!f.fmtFlags.widPresent || (f.wid === 0)) {
			f.buf.WriteString(s);
			return;
		}
		_tuple = f.computePadding(utf8.RuneCountInString(s));
		padding = _tuple[0];
		left = _tuple[1];
		right = _tuple[2];
		if (left > 0) {
			f.writePadding(left, padding);
		}
		f.buf.WriteString(s);
		if (right > 0) {
			f.writePadding(right, padding);
		}
	};
	fmt.prototype.padString = function(s) { return this.$val.padString(s); };
	fmt.ptr.prototype.fmt_boolean = function(v) {
		var $ptr, f, v;
		f = this;
		if (v) {
			f.pad(trueBytes);
		} else {
			f.pad(falseBytes);
		}
	};
	fmt.prototype.fmt_boolean = function(v) { return this.$val.fmt_boolean(v); };
	fmt.ptr.prototype.integer = function(a, base, signedness, digits) {
		var $ptr, _1, _2, a, base, buf, digits, f, i, j, negative, next, prec, runeWidth, signedness, ua, width, width$1, x, x$1, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		f = this;
		if (f.fmtFlags.precPresent && (f.prec === 0) && (a.$high === 0 && a.$low === 0)) {
			return;
		}
		negative = signedness && (a.$high < 0 || (a.$high === 0 && a.$low < 0));
		if (negative) {
			a = new $Int64(-a.$high, -a.$low);
		}
		buf = $subslice(new sliceType(f.intbuf), 0);
		if (f.fmtFlags.widPresent || f.fmtFlags.precPresent || f.fmtFlags.plus || f.fmtFlags.space) {
			width = f.wid + f.prec >> 0;
			if ((base.$high === 0 && base.$low === 16) && f.fmtFlags.sharp) {
				width = width + (2) >> 0;
			}
			if (f.fmtFlags.unicode) {
				width = width + (2) >> 0;
				if (f.fmtFlags.uniQuote) {
					width = width + (7) >> 0;
				}
			}
			if (negative || f.fmtFlags.plus || f.fmtFlags.space) {
				width = width + (1) >> 0;
			}
			if (width > 65) {
				buf = $makeSlice(sliceType, width);
			}
		}
		prec = 0;
		if (f.fmtFlags.precPresent) {
			prec = f.prec;
			f.fmtFlags.zero = false;
		} else if (f.fmtFlags.zero && f.fmtFlags.widPresent && !f.fmtFlags.minus && f.wid > 0) {
			prec = f.wid;
			if (negative || f.fmtFlags.plus || f.fmtFlags.space) {
				prec = prec - (1) >> 0;
			}
		}
		i = buf.$length;
		ua = new $Uint64(a.$high, a.$low);
		_1 = base;
		if ((x = new $Uint64(0, 10), (_1.$high === x.$high && _1.$low === x.$low))) {
			while (true) {
				if (!((ua.$high > 0 || (ua.$high === 0 && ua.$low >= 10)))) { break; }
				i = i - (1) >> 0;
				next = $div64(ua, new $Uint64(0, 10), false);
				((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = ((x$4 = new $Uint64(0 + ua.$high, 48 + ua.$low), x$5 = $mul64(next, new $Uint64(0, 10)), new $Uint64(x$4.$high - x$5.$high, x$4.$low - x$5.$low)).$low << 24 >>> 24));
				ua = next;
			}
		} else if ((x$1 = new $Uint64(0, 16), (_1.$high === x$1.$high && _1.$low === x$1.$low))) {
			while (true) {
				if (!((ua.$high > 0 || (ua.$high === 0 && ua.$low >= 16)))) { break; }
				i = i - (1) >> 0;
				((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = digits.charCodeAt($flatten64(new $Uint64(ua.$high & 0, (ua.$low & 15) >>> 0))));
				ua = $shiftRightUint64(ua, (4));
			}
		} else if ((x$2 = new $Uint64(0, 8), (_1.$high === x$2.$high && _1.$low === x$2.$low))) {
			while (true) {
				if (!((ua.$high > 0 || (ua.$high === 0 && ua.$low >= 8)))) { break; }
				i = i - (1) >> 0;
				((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = ((x$6 = new $Uint64(ua.$high & 0, (ua.$low & 7) >>> 0), new $Uint64(0 + x$6.$high, 48 + x$6.$low)).$low << 24 >>> 24));
				ua = $shiftRightUint64(ua, (3));
			}
		} else if ((x$3 = new $Uint64(0, 2), (_1.$high === x$3.$high && _1.$low === x$3.$low))) {
			while (true) {
				if (!((ua.$high > 0 || (ua.$high === 0 && ua.$low >= 2)))) { break; }
				i = i - (1) >> 0;
				((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = ((x$7 = new $Uint64(ua.$high & 0, (ua.$low & 1) >>> 0), new $Uint64(0 + x$7.$high, 48 + x$7.$low)).$low << 24 >>> 24));
				ua = $shiftRightUint64(ua, (1));
			}
		} else {
			$panic(new $String("fmt: unknown base; can't happen"));
		}
		i = i - (1) >> 0;
		((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = digits.charCodeAt($flatten64(ua)));
		while (true) {
			if (!(i > 0 && prec > (buf.$length - i >> 0))) { break; }
			i = i - (1) >> 0;
			((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 48);
		}
		if (f.fmtFlags.sharp) {
			_2 = base;
			if ((x$8 = new $Uint64(0, 8), (_2.$high === x$8.$high && _2.$low === x$8.$low))) {
				if (!((((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i]) === 48))) {
					i = i - (1) >> 0;
					((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 48);
				}
			} else if ((x$9 = new $Uint64(0, 16), (_2.$high === x$9.$high && _2.$low === x$9.$low))) {
				i = i - (1) >> 0;
				((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = ((120 + digits.charCodeAt(10) << 24 >>> 24) - 97 << 24 >>> 24));
				i = i - (1) >> 0;
				((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 48);
			}
		}
		if (f.fmtFlags.unicode) {
			i = i - (1) >> 0;
			((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 43);
			i = i - (1) >> 0;
			((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 85);
		}
		if (negative) {
			i = i - (1) >> 0;
			((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 45);
		} else if (f.fmtFlags.plus) {
			i = i - (1) >> 0;
			((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 43);
		} else if (f.fmtFlags.space) {
			i = i - (1) >> 0;
			((i < 0 || i >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + i] = 32);
		}
		if (f.fmtFlags.unicode && f.fmtFlags.uniQuote && (a.$high > 0 || (a.$high === 0 && a.$low >= 0)) && (a.$high < 0 || (a.$high === 0 && a.$low <= 1114111)) && strconv.IsPrint(((a.$low + ((a.$high >> 31) * 4294967296)) >> 0))) {
			runeWidth = utf8.RuneLen(((a.$low + ((a.$high >> 31) * 4294967296)) >> 0));
			width$1 = (2 + runeWidth >> 0) + 1 >> 0;
			$copySlice($subslice(buf, (i - width$1 >> 0)), $subslice(buf, i));
			i = i - (width$1) >> 0;
			j = buf.$length - width$1 >> 0;
			((j < 0 || j >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + j] = 32);
			j = j + (1) >> 0;
			((j < 0 || j >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + j] = 39);
			j = j + (1) >> 0;
			utf8.EncodeRune($subslice(buf, j), ((a.$low + ((a.$high >> 31) * 4294967296)) >> 0));
			j = j + (runeWidth) >> 0;
			((j < 0 || j >= buf.$length) ? $throwRuntimeError("index out of range") : buf.$array[buf.$offset + j] = 39);
		}
		f.pad($subslice(buf, i));
	};
	fmt.prototype.integer = function(a, base, signedness, digits) { return this.$val.integer(a, base, signedness, digits); };
	fmt.ptr.prototype.truncate = function(s) {
		var $ptr, _i, _ref, _rune, f, i, n, s;
		f = this;
		if (f.fmtFlags.precPresent && f.prec < utf8.RuneCountInString(s)) {
			n = f.prec;
			_ref = s;
			_i = 0;
			while (true) {
				if (!(_i < _ref.length)) { break; }
				_rune = $decodeRune(_ref, _i);
				i = _i;
				if (n === 0) {
					s = s.substring(0, i);
					break;
				}
				n = n - (1) >> 0;
				_i += _rune[1];
			}
		}
		return s;
	};
	fmt.prototype.truncate = function(s) { return this.$val.truncate(s); };
	fmt.ptr.prototype.fmt_s = function(s) {
		var $ptr, f, s;
		f = this;
		s = f.truncate(s);
		f.padString(s);
	};
	fmt.prototype.fmt_s = function(s) { return this.$val.fmt_s(s); };
	fmt.ptr.prototype.fmt_sbx = function(s, b, digits) {
		var $ptr, b, buf, c, digits, f, i, n, s, x;
		f = this;
		n = b.$length;
		if (b === sliceType.nil) {
			n = s.length;
		}
		x = (digits.charCodeAt(10) - 97 << 24 >>> 24) + 120 << 24 >>> 24;
		buf = sliceType.nil;
		i = 0;
		while (true) {
			if (!(i < n)) { break; }
			if (i > 0 && f.fmtFlags.space) {
				buf = $append(buf, 32);
			}
			if (f.fmtFlags.sharp && (f.fmtFlags.space || (i === 0))) {
				buf = $append(buf, 48, x);
			}
			c = 0;
			if (b === sliceType.nil) {
				c = s.charCodeAt(i);
			} else {
				c = ((i < 0 || i >= b.$length) ? $throwRuntimeError("index out of range") : b.$array[b.$offset + i]);
			}
			buf = $append(buf, digits.charCodeAt((c >>> 4 << 24 >>> 24)), digits.charCodeAt(((c & 15) >>> 0)));
			i = i + (1) >> 0;
		}
		f.pad(buf);
	};
	fmt.prototype.fmt_sbx = function(s, b, digits) { return this.$val.fmt_sbx(s, b, digits); };
	fmt.ptr.prototype.fmt_sx = function(s, digits) {
		var $ptr, digits, f, s;
		f = this;
		if (f.fmtFlags.precPresent && f.prec < s.length) {
			s = s.substring(0, f.prec);
		}
		f.fmt_sbx(s, sliceType.nil, digits);
	};
	fmt.prototype.fmt_sx = function(s, digits) { return this.$val.fmt_sx(s, digits); };
	fmt.ptr.prototype.fmt_bx = function(b, digits) {
		var $ptr, b, digits, f;
		f = this;
		if (f.fmtFlags.precPresent && f.prec < b.$length) {
			b = $subslice(b, 0, f.prec);
		}
		f.fmt_sbx("", b, digits);
	};
	fmt.prototype.fmt_bx = function(b, digits) { return this.$val.fmt_bx(b, digits); };
	fmt.ptr.prototype.fmt_q = function(s) {
		var $ptr, f, quoted, s;
		f = this;
		s = f.truncate(s);
		quoted = "";
		if (f.fmtFlags.sharp && strconv.CanBackquote(s)) {
			quoted = "`" + s + "`";
		} else {
			if (f.fmtFlags.plus) {
				quoted = strconv.QuoteToASCII(s);
			} else {
				quoted = strconv.Quote(s);
			}
		}
		f.padString(quoted);
	};
	fmt.prototype.fmt_q = function(s) { return this.$val.fmt_q(s); };
	fmt.ptr.prototype.fmt_qc = function(c) {
		var $ptr, c, f, quoted;
		f = this;
		quoted = sliceType.nil;
		if (f.fmtFlags.plus) {
			quoted = strconv.AppendQuoteRuneToASCII($subslice(new sliceType(f.intbuf), 0, 0), ((c.$low + ((c.$high >> 31) * 4294967296)) >> 0));
		} else {
			quoted = strconv.AppendQuoteRune($subslice(new sliceType(f.intbuf), 0, 0), ((c.$low + ((c.$high >> 31) * 4294967296)) >> 0));
		}
		f.pad(quoted);
	};
	fmt.prototype.fmt_qc = function(c) { return this.$val.fmt_qc(c); };
	doPrec = function(f, def) {
		var $ptr, def, f;
		if (f.fmtFlags.precPresent) {
			return f.prec;
		}
		return def;
	};
	fmt.ptr.prototype.formatFloat = function(v, verb, prec, n) {
		var $ptr, f, n, num, prec, v, verb, $deferred;
		/* */ var $err = null; try { $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		f = this;
		num = strconv.AppendFloat($subslice(new sliceType(f.intbuf), 0, 1), v, verb, prec, n);
		if (((1 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 1]) === 45) || ((1 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 1]) === 43)) {
			num = $subslice(num, 1);
		} else {
			(0 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 0] = 43);
		}
		if (math.IsInf(v, 0)) {
			if (f.fmtFlags.zero) {
				$deferred.push([(function() {
					var $ptr;
					f.fmtFlags.zero = true;
				}), []]);
				f.fmtFlags.zero = false;
			}
		}
		if (f.fmtFlags.zero && f.fmtFlags.widPresent && f.wid > num.$length) {
			if (f.fmtFlags.space && v >= 0) {
				f.buf.WriteByte(32);
				f.wid = f.wid - (1) >> 0;
			} else if (f.fmtFlags.plus || v < 0) {
				f.buf.WriteByte((0 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 0]));
				f.wid = f.wid - (1) >> 0;
			}
			f.pad($subslice(num, 1));
			return;
		}
		if (f.fmtFlags.space && ((0 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 0]) === 43)) {
			(0 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 0] = 32);
			f.pad(num);
			return;
		}
		if (f.fmtFlags.plus || ((0 >= num.$length ? $throwRuntimeError("index out of range") : num.$array[num.$offset + 0]) === 45) || math.IsInf(v, 0)) {
			f.pad(num);
			return;
		}
		f.pad($subslice(num, 1));
		/* */ } catch(err) { $err = err; } finally { $callDeferred($deferred, $err); }
	};
	fmt.prototype.formatFloat = function(v, verb, prec, n) { return this.$val.formatFloat(v, verb, prec, n); };
	fmt.ptr.prototype.fmt_e64 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 101, doPrec(f, 6), 64);
	};
	fmt.prototype.fmt_e64 = function(v) { return this.$val.fmt_e64(v); };
	fmt.ptr.prototype.fmt_E64 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 69, doPrec(f, 6), 64);
	};
	fmt.prototype.fmt_E64 = function(v) { return this.$val.fmt_E64(v); };
	fmt.ptr.prototype.fmt_f64 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 102, doPrec(f, 6), 64);
	};
	fmt.prototype.fmt_f64 = function(v) { return this.$val.fmt_f64(v); };
	fmt.ptr.prototype.fmt_g64 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 103, doPrec(f, -1), 64);
	};
	fmt.prototype.fmt_g64 = function(v) { return this.$val.fmt_g64(v); };
	fmt.ptr.prototype.fmt_G64 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 71, doPrec(f, -1), 64);
	};
	fmt.prototype.fmt_G64 = function(v) { return this.$val.fmt_G64(v); };
	fmt.ptr.prototype.fmt_fb64 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 98, 0, 64);
	};
	fmt.prototype.fmt_fb64 = function(v) { return this.$val.fmt_fb64(v); };
	fmt.ptr.prototype.fmt_e32 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 101, doPrec(f, 6), 32);
	};
	fmt.prototype.fmt_e32 = function(v) { return this.$val.fmt_e32(v); };
	fmt.ptr.prototype.fmt_E32 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 69, doPrec(f, 6), 32);
	};
	fmt.prototype.fmt_E32 = function(v) { return this.$val.fmt_E32(v); };
	fmt.ptr.prototype.fmt_f32 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 102, doPrec(f, 6), 32);
	};
	fmt.prototype.fmt_f32 = function(v) { return this.$val.fmt_f32(v); };
	fmt.ptr.prototype.fmt_g32 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 103, doPrec(f, -1), 32);
	};
	fmt.prototype.fmt_g32 = function(v) { return this.$val.fmt_g32(v); };
	fmt.ptr.prototype.fmt_G32 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 71, doPrec(f, -1), 32);
	};
	fmt.prototype.fmt_G32 = function(v) { return this.$val.fmt_G32(v); };
	fmt.ptr.prototype.fmt_fb32 = function(v) {
		var $ptr, f, v;
		f = this;
		f.formatFloat(v, 98, 0, 32);
	};
	fmt.prototype.fmt_fb32 = function(v) { return this.$val.fmt_fb32(v); };
	fmt.ptr.prototype.fmt_c64 = function(v, verb) {
		var $ptr, f, v, verb;
		f = this;
		f.fmt_complex(v.$real, v.$imag, 32, verb);
	};
	fmt.prototype.fmt_c64 = function(v, verb) { return this.$val.fmt_c64(v, verb); };
	fmt.ptr.prototype.fmt_c128 = function(v, verb) {
		var $ptr, f, v, verb;
		f = this;
		f.fmt_complex(v.$real, v.$imag, 64, verb);
	};
	fmt.prototype.fmt_c128 = function(v, verb) { return this.$val.fmt_c128(v, verb); };
	fmt.ptr.prototype.fmt_complex = function(r, j, size, verb) {
		var $ptr, _3, f, i, j, oldPlus, oldSpace, oldWid, r, size, verb;
		f = this;
		f.buf.WriteByte(40);
		oldPlus = f.fmtFlags.plus;
		oldSpace = f.fmtFlags.space;
		oldWid = f.wid;
		i = 0;
		while (true) {
			_3 = verb;
			if (_3 === (98)) {
				f.formatFloat(r, 98, 0, size);
			} else if (_3 === (101)) {
				f.formatFloat(r, 101, doPrec(f, 6), size);
			} else if (_3 === (69)) {
				f.formatFloat(r, 69, doPrec(f, 6), size);
			} else if ((_3 === (102)) || (_3 === (70))) {
				f.formatFloat(r, 102, doPrec(f, 6), size);
			} else if (_3 === (103)) {
				f.formatFloat(r, 103, doPrec(f, -1), size);
			} else if (_3 === (71)) {
				f.formatFloat(r, 71, doPrec(f, -1), size);
			}
			if (!((i === 0))) {
				break;
			}
			f.fmtFlags.plus = true;
			f.fmtFlags.space = false;
			f.wid = oldWid;
			r = j;
			i = i + (1) >> 0;
		}
		f.fmtFlags.space = oldSpace;
		f.fmtFlags.plus = oldPlus;
		f.wid = oldWid;
		f.buf.Write(irparenBytes);
	};
	fmt.prototype.fmt_complex = function(r, j, size, verb) { return this.$val.fmt_complex(r, j, size, verb); };
	$ptrType(buffer).prototype.Write = function(p) {
		var $ptr, _tmp, _tmp$1, b, err, n, p;
		n = 0;
		err = $ifaceNil;
		b = this;
		b.$set($appendSlice(b.$get(), p));
		_tmp = p.$length;
		_tmp$1 = $ifaceNil;
		n = _tmp;
		err = _tmp$1;
		return [n, err];
	};
	$ptrType(buffer).prototype.WriteString = function(s) {
		var $ptr, _tmp, _tmp$1, b, err, n, s;
		n = 0;
		err = $ifaceNil;
		b = this;
		b.$set($appendSlice(b.$get(), s));
		_tmp = s.length;
		_tmp$1 = $ifaceNil;
		n = _tmp;
		err = _tmp$1;
		return [n, err];
	};
	$ptrType(buffer).prototype.WriteByte = function(c) {
		var $ptr, b, c;
		b = this;
		b.$set($append(b.$get(), c));
		return $ifaceNil;
	};
	$ptrType(buffer).prototype.WriteRune = function(r) {
		var $ptr, b, bp, n, r, w, x;
		bp = this;
		if (r < 128) {
			bp.$set($append(bp.$get(), (r << 24 >>> 24)));
			return $ifaceNil;
		}
		b = bp.$get();
		n = b.$length;
		while (true) {
			if (!((n + 4 >> 0) > b.$capacity)) { break; }
			b = $append(b, 0);
		}
		w = utf8.EncodeRune((x = $subslice(b, n, (n + 4 >> 0)), $subslice(new sliceType(x.$array), x.$offset, x.$offset + x.$length)), r);
		bp.$set($subslice(b, 0, (n + w >> 0)));
		return $ifaceNil;
	};
	newPrinter = function() {
		var $ptr, _r$2, p, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; p = $f.p; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r$2 = ppFree.Get(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		p = $assertType(_r$2, ptrType$2);
		p.panicking = false;
		p.erroring = false;
		p.fmt.init((p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))));
		return p;
		/* */ } return; } if ($f === undefined) { $f = { $blk: newPrinter }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f.p = p; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.ptr.prototype.free = function() {
		var $ptr, p;
		p = this;
		if (p.buf.$capacity > 1024) {
			return;
		}
		p.buf = $subslice(p.buf, 0, 0);
		p.arg = $ifaceNil;
		p.value = new reflect.Value.ptr(ptrType.nil, 0, 0);
		ppFree.Put(p);
	};
	pp.prototype.free = function() { return this.$val.free(); };
	pp.ptr.prototype.Width = function() {
		var $ptr, _tmp, _tmp$1, ok, p, wid;
		wid = 0;
		ok = false;
		p = this;
		_tmp = p.fmt.wid;
		_tmp$1 = p.fmt.fmtFlags.widPresent;
		wid = _tmp;
		ok = _tmp$1;
		return [wid, ok];
	};
	pp.prototype.Width = function() { return this.$val.Width(); };
	pp.ptr.prototype.Precision = function() {
		var $ptr, _tmp, _tmp$1, ok, p, prec;
		prec = 0;
		ok = false;
		p = this;
		_tmp = p.fmt.prec;
		_tmp$1 = p.fmt.fmtFlags.precPresent;
		prec = _tmp;
		ok = _tmp$1;
		return [prec, ok];
	};
	pp.prototype.Precision = function() { return this.$val.Precision(); };
	pp.ptr.prototype.Flag = function(b) {
		var $ptr, _1, b, p;
		p = this;
		_1 = b;
		if (_1 === (45)) {
			return p.fmt.fmtFlags.minus;
		} else if (_1 === (43)) {
			return p.fmt.fmtFlags.plus;
		} else if (_1 === (35)) {
			return p.fmt.fmtFlags.sharp;
		} else if (_1 === (32)) {
			return p.fmt.fmtFlags.space;
		} else if (_1 === (48)) {
			return p.fmt.fmtFlags.zero;
		}
		return false;
	};
	pp.prototype.Flag = function(b) { return this.$val.Flag(b); };
	pp.ptr.prototype.add = function(c) {
		var $ptr, c, p;
		p = this;
		(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteRune(c);
	};
	pp.prototype.add = function(c) { return this.$val.add(c); };
	pp.ptr.prototype.Write = function(b) {
		var $ptr, _tuple, b, err, p, ret;
		ret = 0;
		err = $ifaceNil;
		p = this;
		_tuple = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(b);
		ret = _tuple[0];
		err = _tuple[1];
		return [ret, err];
	};
	pp.prototype.Write = function(b) { return this.$val.Write(b); };
	Fprintf = function(w, format, a) {
		var $ptr, _r$2, _r$3, _tuple, a, err, format, n, p, w, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; _tuple = $f._tuple; a = $f.a; err = $f.err; format = $f.format; n = $f.n; p = $f.p; w = $f.w; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		n = 0;
		err = $ifaceNil;
		_r$2 = newPrinter(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		p = _r$2;
		$r = p.doPrintf(format, a); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		_r$3 = w.Write((x = p.buf, $subslice(new sliceType(x.$array), x.$offset, x.$offset + x.$length))); /* */ $s = 3; case 3: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		_tuple = _r$3;
		n = _tuple[0];
		err = _tuple[1];
		p.free();
		return [n, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Fprintf }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._tuple = _tuple; $f.a = a; $f.err = err; $f.format = format; $f.n = n; $f.p = p; $f.w = w; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Fprintf = Fprintf;
	Printf = function(format, a) {
		var $ptr, _r$2, _tuple, a, err, format, n, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _tuple = $f._tuple; a = $f.a; err = $f.err; format = $f.format; n = $f.n; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		n = 0;
		err = $ifaceNil;
		_r$2 = Fprintf(os.Stdout, format, a); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_tuple = _r$2;
		n = _tuple[0];
		err = _tuple[1];
		/* */ $s = 2; case 2:
		return [n, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Printf }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._tuple = _tuple; $f.a = a; $f.err = err; $f.format = format; $f.n = n; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Printf = Printf;
	Sprintf = function(format, a) {
		var $ptr, _r$2, a, format, p, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; a = $f.a; format = $f.format; p = $f.p; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r$2 = newPrinter(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		p = _r$2;
		$r = p.doPrintf(format, a); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		s = $bytesToString(p.buf);
		p.free();
		return s;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Sprintf }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f.a = a; $f.format = format; $f.p = p; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Sprintf = Sprintf;
	getField = function(v, i) {
		var $ptr, _r$2, _r$3, i, v, val, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; i = $f.i; v = $f.v; val = $f.val; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		_r$2 = v.Field(i); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		val = _r$2;
		/* */ if ((val.Kind() === 20) && !val.IsNil()) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if ((val.Kind() === 20) && !val.IsNil()) { */ case 2:
			_r$3 = val.Elem(); /* */ $s = 4; case 4: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			val = _r$3;
		/* } */ case 3:
		return val;
		/* */ } return; } if ($f === undefined) { $f = { $blk: getField }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.i = i; $f.v = v; $f.val = val; $f.$s = $s; $f.$r = $r; return $f;
	};
	tooLarge = function(x) {
		var $ptr, x;
		return x > 1000000 || x < -1000000;
	};
	parsenum = function(s, start, end) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, end, isnum, newi, num, s, start;
		num = 0;
		isnum = false;
		newi = 0;
		if (start >= end) {
			_tmp = 0;
			_tmp$1 = false;
			_tmp$2 = end;
			num = _tmp;
			isnum = _tmp$1;
			newi = _tmp$2;
			return [num, isnum, newi];
		}
		newi = start;
		while (true) {
			if (!(newi < end && 48 <= s.charCodeAt(newi) && s.charCodeAt(newi) <= 57)) { break; }
			if (tooLarge(num)) {
				_tmp$3 = 0;
				_tmp$4 = false;
				_tmp$5 = end;
				num = _tmp$3;
				isnum = _tmp$4;
				newi = _tmp$5;
				return [num, isnum, newi];
			}
			num = ($imul(num, 10)) + ((s.charCodeAt(newi) - 48 << 24 >>> 24) >> 0) >> 0;
			isnum = true;
			newi = newi + (1) >> 0;
		}
		return [num, isnum, newi];
	};
	pp.ptr.prototype.unknownType = function(v) {
		var $ptr, _r$2, _r$3, p, v, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; p = $f.p; v = $f.v; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		v = v;
		p = this;
		if (!v.IsValid()) {
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilAngleBytes);
			return;
		}
		(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(63);
		_r$2 = v.Type().String(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_r$3 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$2); /* */ $s = 2; case 2: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		_r$3;
		(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(63);
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.unknownType }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.p = p; $f.v = v; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.unknownType = function(v) { return this.$val.unknownType(v); };
	pp.ptr.prototype.badVerb = function(verb) {
		var $ptr, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, p, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; p = $f.p; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
		p.erroring = true;
		p.add(37);
		p.add(33);
		p.add(verb);
		p.add(40);
			/* */ if (!($interfaceIsEqual(p.arg, $ifaceNil))) { $s = 2; continue; }
			/* */ if (p.value.IsValid()) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!($interfaceIsEqual(p.arg, $ifaceNil))) { */ case 2:
				_r$2 = reflect.TypeOf(p.arg).String(); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_r$3 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$2); /* */ $s = 7; case 7: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				_r$3;
				p.add(61);
				_r$4 = p.printArg(p.arg, 118, 0); /* */ $s = 8; case 8: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				_r$4;
				$s = 5; continue;
			/* } else if (p.value.IsValid()) { */ case 3:
				_r$5 = p.value.Type().String(); /* */ $s = 9; case 9: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
				_r$6 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$5); /* */ $s = 10; case 10: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
				_r$6;
				p.add(61);
				_r$7 = p.printValue(p.value, 118, 0); /* */ $s = 11; case 11: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
				_r$7;
				$s = 5; continue;
			/* } else { */ case 4:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilAngleBytes);
			/* } */ case 5:
		case 1:
		p.add(41);
		p.erroring = false;
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.badVerb }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f.p = p; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.badVerb = function(verb) { return this.$val.badVerb(verb); };
	pp.ptr.prototype.fmtBool = function(v, verb) {
		var $ptr, _2, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _2 = $f._2; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_2 = verb;
			/* */ if ((_2 === (116)) || (_2 === (118))) { $s = 2; continue; }
			/* */ $s = 3; continue;
			/* if ((_2 === (116)) || (_2 === (118))) { */ case 2:
				p.fmt.fmt_boolean(v);
				$s = 4; continue;
			/* } else { */ case 3:
				$r = p.badVerb(verb); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 4:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtBool }; } $f.$ptr = $ptr; $f._2 = _2; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtBool = function(v, verb) { return this.$val.fmtBool(v, verb); };
	pp.ptr.prototype.fmtC = function(c) {
		var $ptr, c, p, r, w, x;
		p = this;
		r = ((c.$low + ((c.$high >> 31) * 4294967296)) >> 0);
		if (!((x = new $Int64(0, r), (x.$high === c.$high && x.$low === c.$low)))) {
			r = 65533;
		}
		w = utf8.EncodeRune($subslice(new sliceType(p.runeBuf), 0, 4), r);
		p.fmt.pad($subslice(new sliceType(p.runeBuf), 0, w));
	};
	pp.prototype.fmtC = function(c) { return this.$val.fmtC(c); };
	pp.ptr.prototype.fmtInt64 = function(v, verb) {
		var $ptr, _3, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _3 = $f._3; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_3 = verb;
			/* */ if (_3 === (98)) { $s = 2; continue; }
			/* */ if (_3 === (99)) { $s = 3; continue; }
			/* */ if ((_3 === (100)) || (_3 === (118))) { $s = 4; continue; }
			/* */ if (_3 === (111)) { $s = 5; continue; }
			/* */ if (_3 === (113)) { $s = 6; continue; }
			/* */ if (_3 === (120)) { $s = 7; continue; }
			/* */ if (_3 === (85)) { $s = 8; continue; }
			/* */ if (_3 === (88)) { $s = 9; continue; }
			/* */ $s = 10; continue;
			/* if (_3 === (98)) { */ case 2:
				p.fmt.integer(v, new $Uint64(0, 2), true, "0123456789abcdef");
				$s = 11; continue;
			/* } else if (_3 === (99)) { */ case 3:
				p.fmtC(v);
				$s = 11; continue;
			/* } else if ((_3 === (100)) || (_3 === (118))) { */ case 4:
				p.fmt.integer(v, new $Uint64(0, 10), true, "0123456789abcdef");
				$s = 11; continue;
			/* } else if (_3 === (111)) { */ case 5:
				p.fmt.integer(v, new $Uint64(0, 8), true, "0123456789abcdef");
				$s = 11; continue;
			/* } else if (_3 === (113)) { */ case 6:
				/* */ if ((0 < v.$high || (0 === v.$high && 0 <= v.$low)) && (v.$high < 0 || (v.$high === 0 && v.$low <= 1114111))) { $s = 12; continue; }
				/* */ $s = 13; continue;
				/* if ((0 < v.$high || (0 === v.$high && 0 <= v.$low)) && (v.$high < 0 || (v.$high === 0 && v.$low <= 1114111))) { */ case 12:
					p.fmt.fmt_qc(v);
					$s = 14; continue;
				/* } else { */ case 13:
					$r = p.badVerb(verb); /* */ $s = 15; case 15: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				/* } */ case 14:
				$s = 11; continue;
			/* } else if (_3 === (120)) { */ case 7:
				p.fmt.integer(v, new $Uint64(0, 16), true, "0123456789abcdef");
				$s = 11; continue;
			/* } else if (_3 === (85)) { */ case 8:
				p.fmtUnicode(v);
				$s = 11; continue;
			/* } else if (_3 === (88)) { */ case 9:
				p.fmt.integer(v, new $Uint64(0, 16), true, "0123456789ABCDEF");
				$s = 11; continue;
			/* } else { */ case 10:
				$r = p.badVerb(verb); /* */ $s = 16; case 16: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 11:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtInt64 }; } $f.$ptr = $ptr; $f._3 = _3; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtInt64 = function(v, verb) { return this.$val.fmtInt64(v, verb); };
	pp.ptr.prototype.fmt0x64 = function(v, leading0x) {
		var $ptr, leading0x, p, sharp, v;
		p = this;
		sharp = p.fmt.fmtFlags.sharp;
		p.fmt.fmtFlags.sharp = leading0x;
		p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 16), false, "0123456789abcdef");
		p.fmt.fmtFlags.sharp = sharp;
	};
	pp.prototype.fmt0x64 = function(v, leading0x) { return this.$val.fmt0x64(v, leading0x); };
	pp.ptr.prototype.fmtUnicode = function(v) {
		var $ptr, p, prec, precPresent, sharp, v;
		p = this;
		precPresent = p.fmt.fmtFlags.precPresent;
		sharp = p.fmt.fmtFlags.sharp;
		p.fmt.fmtFlags.sharp = false;
		prec = p.fmt.prec;
		if (!precPresent) {
			p.fmt.prec = 4;
			p.fmt.fmtFlags.precPresent = true;
		}
		p.fmt.fmtFlags.unicode = true;
		p.fmt.fmtFlags.uniQuote = sharp;
		p.fmt.integer(v, new $Uint64(0, 16), false, "0123456789ABCDEF");
		p.fmt.fmtFlags.unicode = false;
		p.fmt.fmtFlags.uniQuote = false;
		p.fmt.prec = prec;
		p.fmt.fmtFlags.precPresent = precPresent;
		p.fmt.fmtFlags.sharp = sharp;
	};
	pp.prototype.fmtUnicode = function(v) { return this.$val.fmtUnicode(v); };
	pp.ptr.prototype.fmtUint64 = function(v, verb) {
		var $ptr, _4, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _4 = $f._4; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_4 = verb;
			/* */ if (_4 === (98)) { $s = 2; continue; }
			/* */ if (_4 === (99)) { $s = 3; continue; }
			/* */ if (_4 === (100)) { $s = 4; continue; }
			/* */ if (_4 === (118)) { $s = 5; continue; }
			/* */ if (_4 === (111)) { $s = 6; continue; }
			/* */ if (_4 === (113)) { $s = 7; continue; }
			/* */ if (_4 === (120)) { $s = 8; continue; }
			/* */ if (_4 === (88)) { $s = 9; continue; }
			/* */ if (_4 === (85)) { $s = 10; continue; }
			/* */ $s = 11; continue;
			/* if (_4 === (98)) { */ case 2:
				p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 2), false, "0123456789abcdef");
				$s = 12; continue;
			/* } else if (_4 === (99)) { */ case 3:
				p.fmtC(new $Int64(v.$high, v.$low));
				$s = 12; continue;
			/* } else if (_4 === (100)) { */ case 4:
				p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 10), false, "0123456789abcdef");
				$s = 12; continue;
			/* } else if (_4 === (118)) { */ case 5:
				if (p.fmt.fmtFlags.sharpV) {
					p.fmt0x64(v, true);
				} else {
					p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 10), false, "0123456789abcdef");
				}
				$s = 12; continue;
			/* } else if (_4 === (111)) { */ case 6:
				p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 8), false, "0123456789abcdef");
				$s = 12; continue;
			/* } else if (_4 === (113)) { */ case 7:
				/* */ if ((0 < v.$high || (0 === v.$high && 0 <= v.$low)) && (v.$high < 0 || (v.$high === 0 && v.$low <= 1114111))) { $s = 13; continue; }
				/* */ $s = 14; continue;
				/* if ((0 < v.$high || (0 === v.$high && 0 <= v.$low)) && (v.$high < 0 || (v.$high === 0 && v.$low <= 1114111))) { */ case 13:
					p.fmt.fmt_qc(new $Int64(v.$high, v.$low));
					$s = 15; continue;
				/* } else { */ case 14:
					$r = p.badVerb(verb); /* */ $s = 16; case 16: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				/* } */ case 15:
				$s = 12; continue;
			/* } else if (_4 === (120)) { */ case 8:
				p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 16), false, "0123456789abcdef");
				$s = 12; continue;
			/* } else if (_4 === (88)) { */ case 9:
				p.fmt.integer(new $Int64(v.$high, v.$low), new $Uint64(0, 16), false, "0123456789ABCDEF");
				$s = 12; continue;
			/* } else if (_4 === (85)) { */ case 10:
				p.fmtUnicode(new $Int64(v.$high, v.$low));
				$s = 12; continue;
			/* } else { */ case 11:
				$r = p.badVerb(verb); /* */ $s = 17; case 17: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 12:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtUint64 }; } $f.$ptr = $ptr; $f._4 = _4; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtUint64 = function(v, verb) { return this.$val.fmtUint64(v, verb); };
	pp.ptr.prototype.fmtFloat32 = function(v, verb) {
		var $ptr, _5, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _5 = $f._5; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_5 = verb;
			/* */ if (_5 === (98)) { $s = 2; continue; }
			/* */ if (_5 === (101)) { $s = 3; continue; }
			/* */ if (_5 === (69)) { $s = 4; continue; }
			/* */ if ((_5 === (102)) || (_5 === (70))) { $s = 5; continue; }
			/* */ if ((_5 === (103)) || (_5 === (118))) { $s = 6; continue; }
			/* */ if (_5 === (71)) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (_5 === (98)) { */ case 2:
				p.fmt.fmt_fb32(v);
				$s = 9; continue;
			/* } else if (_5 === (101)) { */ case 3:
				p.fmt.fmt_e32(v);
				$s = 9; continue;
			/* } else if (_5 === (69)) { */ case 4:
				p.fmt.fmt_E32(v);
				$s = 9; continue;
			/* } else if ((_5 === (102)) || (_5 === (70))) { */ case 5:
				p.fmt.fmt_f32(v);
				$s = 9; continue;
			/* } else if ((_5 === (103)) || (_5 === (118))) { */ case 6:
				p.fmt.fmt_g32(v);
				$s = 9; continue;
			/* } else if (_5 === (71)) { */ case 7:
				p.fmt.fmt_G32(v);
				$s = 9; continue;
			/* } else { */ case 8:
				$r = p.badVerb(verb); /* */ $s = 10; case 10: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 9:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtFloat32 }; } $f.$ptr = $ptr; $f._5 = _5; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtFloat32 = function(v, verb) { return this.$val.fmtFloat32(v, verb); };
	pp.ptr.prototype.fmtFloat64 = function(v, verb) {
		var $ptr, _6, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _6 = $f._6; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_6 = verb;
			/* */ if (_6 === (98)) { $s = 2; continue; }
			/* */ if (_6 === (101)) { $s = 3; continue; }
			/* */ if (_6 === (69)) { $s = 4; continue; }
			/* */ if ((_6 === (102)) || (_6 === (70))) { $s = 5; continue; }
			/* */ if ((_6 === (103)) || (_6 === (118))) { $s = 6; continue; }
			/* */ if (_6 === (71)) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (_6 === (98)) { */ case 2:
				p.fmt.fmt_fb64(v);
				$s = 9; continue;
			/* } else if (_6 === (101)) { */ case 3:
				p.fmt.fmt_e64(v);
				$s = 9; continue;
			/* } else if (_6 === (69)) { */ case 4:
				p.fmt.fmt_E64(v);
				$s = 9; continue;
			/* } else if ((_6 === (102)) || (_6 === (70))) { */ case 5:
				p.fmt.fmt_f64(v);
				$s = 9; continue;
			/* } else if ((_6 === (103)) || (_6 === (118))) { */ case 6:
				p.fmt.fmt_g64(v);
				$s = 9; continue;
			/* } else if (_6 === (71)) { */ case 7:
				p.fmt.fmt_G64(v);
				$s = 9; continue;
			/* } else { */ case 8:
				$r = p.badVerb(verb); /* */ $s = 10; case 10: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 9:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtFloat64 }; } $f.$ptr = $ptr; $f._6 = _6; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtFloat64 = function(v, verb) { return this.$val.fmtFloat64(v, verb); };
	pp.ptr.prototype.fmtComplex64 = function(v, verb) {
		var $ptr, _7, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _7 = $f._7; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_7 = verb;
			/* */ if ((_7 === (98)) || (_7 === (101)) || (_7 === (69)) || (_7 === (102)) || (_7 === (70)) || (_7 === (103)) || (_7 === (71))) { $s = 2; continue; }
			/* */ if (_7 === (118)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if ((_7 === (98)) || (_7 === (101)) || (_7 === (69)) || (_7 === (102)) || (_7 === (70)) || (_7 === (103)) || (_7 === (71))) { */ case 2:
				p.fmt.fmt_c64(v, verb);
				$s = 5; continue;
			/* } else if (_7 === (118)) { */ case 3:
				p.fmt.fmt_c64(v, 103);
				$s = 5; continue;
			/* } else { */ case 4:
				$r = p.badVerb(verb); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 5:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtComplex64 }; } $f.$ptr = $ptr; $f._7 = _7; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtComplex64 = function(v, verb) { return this.$val.fmtComplex64(v, verb); };
	pp.ptr.prototype.fmtComplex128 = function(v, verb) {
		var $ptr, _8, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _8 = $f._8; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_8 = verb;
			/* */ if ((_8 === (98)) || (_8 === (101)) || (_8 === (69)) || (_8 === (102)) || (_8 === (70)) || (_8 === (103)) || (_8 === (71))) { $s = 2; continue; }
			/* */ if (_8 === (118)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if ((_8 === (98)) || (_8 === (101)) || (_8 === (69)) || (_8 === (102)) || (_8 === (70)) || (_8 === (103)) || (_8 === (71))) { */ case 2:
				p.fmt.fmt_c128(v, verb);
				$s = 5; continue;
			/* } else if (_8 === (118)) { */ case 3:
				p.fmt.fmt_c128(v, 103);
				$s = 5; continue;
			/* } else { */ case 4:
				$r = p.badVerb(verb); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 5:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtComplex128 }; } $f.$ptr = $ptr; $f._8 = _8; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtComplex128 = function(v, verb) { return this.$val.fmtComplex128(v, verb); };
	pp.ptr.prototype.fmtString = function(v, verb) {
		var $ptr, _9, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _9 = $f._9; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
			_9 = verb;
			/* */ if (_9 === (118)) { $s = 2; continue; }
			/* */ if (_9 === (115)) { $s = 3; continue; }
			/* */ if (_9 === (120)) { $s = 4; continue; }
			/* */ if (_9 === (88)) { $s = 5; continue; }
			/* */ if (_9 === (113)) { $s = 6; continue; }
			/* */ $s = 7; continue;
			/* if (_9 === (118)) { */ case 2:
				if (p.fmt.fmtFlags.sharpV) {
					p.fmt.fmt_q(v);
				} else {
					p.fmt.fmt_s(v);
				}
				$s = 8; continue;
			/* } else if (_9 === (115)) { */ case 3:
				p.fmt.fmt_s(v);
				$s = 8; continue;
			/* } else if (_9 === (120)) { */ case 4:
				p.fmt.fmt_sx(v, "0123456789abcdef");
				$s = 8; continue;
			/* } else if (_9 === (88)) { */ case 5:
				p.fmt.fmt_sx(v, "0123456789ABCDEF");
				$s = 8; continue;
			/* } else if (_9 === (113)) { */ case 6:
				p.fmt.fmt_q(v);
				$s = 8; continue;
			/* } else { */ case 7:
				$r = p.badVerb(verb); /* */ $s = 9; case 9: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 8:
		case 1:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtString }; } $f.$ptr = $ptr; $f._9 = _9; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtString = function(v, verb) { return this.$val.fmtString(v, verb); };
	pp.ptr.prototype.fmtBytes = function(v, verb, typ, depth) {
		var $ptr, _10, _i, _r$2, _r$3, _r$4, _r$5, _r$6, _ref, c, depth, i, p, typ, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _10 = $f._10; _i = $f._i; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _ref = $f._ref; c = $f.c; depth = $f.depth; i = $f.i; p = $f.p; typ = $f.typ; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
		/* */ if ((verb === 118) || (verb === 100)) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if ((verb === 118) || (verb === 100)) { */ case 1:
			/* */ if (p.fmt.fmtFlags.sharpV) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (p.fmt.fmtFlags.sharpV) { */ case 3:
				/* */ if (v === sliceType.nil) { $s = 6; continue; }
				/* */ $s = 7; continue;
				/* if (v === sliceType.nil) { */ case 6:
					/* */ if ($interfaceIsEqual(typ, $ifaceNil)) { $s = 8; continue; }
					/* */ $s = 9; continue;
					/* if ($interfaceIsEqual(typ, $ifaceNil)) { */ case 8:
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString("[]byte(nil)");
						$s = 10; continue;
					/* } else { */ case 9:
						_r$2 = typ.String(); /* */ $s = 11; case 11: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
						_r$3 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$2); /* */ $s = 12; case 12: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
						_r$3;
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilParenBytes);
					/* } */ case 10:
					return;
				/* } */ case 7:
				/* */ if ($interfaceIsEqual(typ, $ifaceNil)) { $s = 13; continue; }
				/* */ $s = 14; continue;
				/* if ($interfaceIsEqual(typ, $ifaceNil)) { */ case 13:
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(bytesBytes);
					$s = 15; continue;
				/* } else { */ case 14:
					_r$4 = typ.String(); /* */ $s = 16; case 16: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
					_r$5 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$4); /* */ $s = 17; case 17: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
					_r$5;
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(123);
				/* } */ case 15:
				$s = 5; continue;
			/* } else { */ case 4:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(91);
			/* } */ case 5:
			_ref = v;
			_i = 0;
			/* while (true) { */ case 18:
				/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 19; continue; }
				i = _i;
				c = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
				if (i > 0) {
					if (p.fmt.fmtFlags.sharpV) {
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(commaSpaceBytes);
					} else {
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(32);
					}
				}
				_r$6 = p.printArg(new $Uint8(c), 118, depth + 1 >> 0); /* */ $s = 20; case 20: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
				_r$6;
				_i++;
			/* } */ $s = 18; continue; case 19:
			if (p.fmt.fmtFlags.sharpV) {
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(125);
			} else {
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(93);
			}
			return;
		/* } */ case 2:
			_10 = verb;
			/* */ if (_10 === (115)) { $s = 22; continue; }
			/* */ if (_10 === (120)) { $s = 23; continue; }
			/* */ if (_10 === (88)) { $s = 24; continue; }
			/* */ if (_10 === (113)) { $s = 25; continue; }
			/* */ $s = 26; continue;
			/* if (_10 === (115)) { */ case 22:
				p.fmt.fmt_s($bytesToString(v));
				$s = 27; continue;
			/* } else if (_10 === (120)) { */ case 23:
				p.fmt.fmt_bx(v, "0123456789abcdef");
				$s = 27; continue;
			/* } else if (_10 === (88)) { */ case 24:
				p.fmt.fmt_bx(v, "0123456789ABCDEF");
				$s = 27; continue;
			/* } else if (_10 === (113)) { */ case 25:
				p.fmt.fmt_q($bytesToString(v));
				$s = 27; continue;
			/* } else { */ case 26:
				$r = p.badVerb(verb); /* */ $s = 28; case 28: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 27:
		case 21:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtBytes }; } $f.$ptr = $ptr; $f._10 = _10; $f._i = _i; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._ref = _ref; $f.c = c; $f.depth = depth; $f.i = i; $f.p = p; $f.typ = typ; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtBytes = function(v, verb, typ, depth) { return this.$val.fmtBytes(v, verb, typ, depth); };
	pp.ptr.prototype.fmtPointer = function(value, verb) {
		var $ptr, _11, _12, _r$2, _r$3, p, u, use0x64, value, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _11 = $f._11; _12 = $f._12; _r$2 = $f._r$2; _r$3 = $f._r$3; p = $f.p; u = $f.u; use0x64 = $f.use0x64; value = $f.value; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		value = value;
		p = this;
		use0x64 = true;
			_11 = verb;
			/* */ if ((_11 === (112)) || (_11 === (118))) { $s = 2; continue; }
			/* */ if ((_11 === (98)) || (_11 === (100)) || (_11 === (111)) || (_11 === (120)) || (_11 === (88))) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if ((_11 === (112)) || (_11 === (118))) { */ case 2:
				$s = 5; continue;
			/* } else if ((_11 === (98)) || (_11 === (100)) || (_11 === (111)) || (_11 === (120)) || (_11 === (88))) { */ case 3:
				use0x64 = false;
				$s = 5; continue;
			/* } else { */ case 4:
				$r = p.badVerb(verb); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				return;
			/* } */ case 5:
		case 1:
		u = 0;
			_12 = value.Kind();
			/* */ if ((_12 === (18)) || (_12 === (19)) || (_12 === (21)) || (_12 === (22)) || (_12 === (23)) || (_12 === (26))) { $s = 8; continue; }
			/* */ $s = 9; continue;
			/* if ((_12 === (18)) || (_12 === (19)) || (_12 === (21)) || (_12 === (22)) || (_12 === (23)) || (_12 === (26))) { */ case 8:
				u = value.Pointer();
				$s = 10; continue;
			/* } else { */ case 9:
				$r = p.badVerb(verb); /* */ $s = 11; case 11: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				return;
			/* } */ case 10:
		case 7:
		/* */ if (p.fmt.fmtFlags.sharpV) { $s = 12; continue; }
		/* */ if ((verb === 118) && (u === 0)) { $s = 13; continue; }
		/* */ $s = 14; continue;
		/* if (p.fmt.fmtFlags.sharpV) { */ case 12:
			p.add(40);
			_r$2 = value.Type().String(); /* */ $s = 16; case 16: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			_r$3 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$2); /* */ $s = 17; case 17: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_r$3;
			p.add(41);
			p.add(40);
			if (u === 0) {
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilBytes);
			} else {
				p.fmt0x64(new $Uint64(0, u.constructor === Number ? u : 1), true);
			}
			p.add(41);
			$s = 15; continue;
		/* } else if ((verb === 118) && (u === 0)) { */ case 13:
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilAngleBytes);
			$s = 15; continue;
		/* } else { */ case 14:
			/* */ if (use0x64) { $s = 18; continue; }
			/* */ $s = 19; continue;
			/* if (use0x64) { */ case 18:
				p.fmt0x64(new $Uint64(0, u.constructor === Number ? u : 1), !p.fmt.fmtFlags.sharp);
				$s = 20; continue;
			/* } else { */ case 19:
				$r = p.fmtUint64(new $Uint64(0, u.constructor === Number ? u : 1), verb); /* */ $s = 21; case 21: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 20:
		/* } */ case 15:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.fmtPointer }; } $f.$ptr = $ptr; $f._11 = _11; $f._12 = _12; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.p = p; $f.u = u; $f.use0x64 = use0x64; $f.value = value; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.fmtPointer = function(value, verb) { return this.$val.fmtPointer(value, verb); };
	pp.ptr.prototype.catchPanic = function(arg, verb) {
		var $ptr, _r$2, _r$3, arg, err, p, v, verb, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; arg = $f.arg; err = $f.err; p = $f.p; v = $f.v; verb = $f.verb; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
		err = $recover();
		/* */ if (!($interfaceIsEqual(err, $ifaceNil))) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!($interfaceIsEqual(err, $ifaceNil))) { */ case 1:
			_r$2 = reflect.ValueOf(arg); /* */ $s = 3; case 3: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			v = _r$2;
			if ((v.Kind() === 22) && v.IsNil()) {
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilAngleBytes);
				return;
			}
			if (p.panicking) {
				$panic(err);
			}
			p.fmt.clearflags();
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(percentBangBytes);
			p.add(verb);
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(panicBytes);
			p.panicking = true;
			_r$3 = p.printArg(err, 118, 0); /* */ $s = 4; case 4: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_r$3;
			p.panicking = false;
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(41);
		/* } */ case 2:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.catchPanic }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.arg = arg; $f.err = err; $f.p = p; $f.v = v; $f.verb = verb; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.catchPanic = function(arg, verb) { return this.$val.catchPanic(arg, verb); };
	pp.ptr.prototype.clearSpecialFlags = function() {
		var $ptr, p, plusV, sharpV;
		plusV = false;
		sharpV = false;
		p = this;
		plusV = p.fmt.fmtFlags.plusV;
		if (plusV) {
			p.fmt.fmtFlags.plus = true;
			p.fmt.fmtFlags.plusV = false;
		}
		sharpV = p.fmt.fmtFlags.sharpV;
		if (sharpV) {
			p.fmt.fmtFlags.sharp = true;
			p.fmt.fmtFlags.sharpV = false;
		}
		return [plusV, sharpV];
	};
	pp.prototype.clearSpecialFlags = function() { return this.$val.clearSpecialFlags(); };
	pp.ptr.prototype.restoreSpecialFlags = function(plusV, sharpV) {
		var $ptr, p, plusV, sharpV;
		p = this;
		if (plusV) {
			p.fmt.fmtFlags.plus = false;
			p.fmt.fmtFlags.plusV = true;
		}
		if (sharpV) {
			p.fmt.fmtFlags.sharp = false;
			p.fmt.fmtFlags.sharpV = true;
		}
	};
	pp.prototype.restoreSpecialFlags = function(plusV, sharpV) { return this.$val.restoreSpecialFlags(plusV, sharpV); };
	pp.ptr.prototype.handleMethods = function(verb, depth) {
		var $ptr, _13, _r$2, _r$3, _r$4, _r$5, _r$6, _ref, _tuple, _tuple$1, _tuple$2, depth, formatter, handled, ok, ok$1, p, stringer, v, v$1, verb, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _13 = $f._13; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _ref = $f._ref; _tuple = $f._tuple; _tuple$1 = $f._tuple$1; _tuple$2 = $f._tuple$2; depth = $f.depth; formatter = $f.formatter; handled = $f.handled; ok = $f.ok; ok$1 = $f.ok$1; p = $f.p; stringer = $f.stringer; v = $f.v; v$1 = $f.v$1; verb = $f.verb; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		handled = false;
		p = this;
		if (p.erroring) {
			return handled;
		}
		_tuple = $assertType(p.arg, Formatter, true);
		formatter = _tuple[0];
		ok = _tuple[1];
		/* */ if (ok) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (ok) { */ case 1:
			handled = true;
			_tuple$1 = p.clearSpecialFlags();
			$deferred.push([$methodVal(p, "restoreSpecialFlags"), [_tuple$1[0], _tuple$1[1]]]);
			$deferred.push([$methodVal(p, "catchPanic"), [p.arg, verb]]);
			$r = formatter.Format(p, verb); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			return handled;
		/* } */ case 2:
		/* */ if (p.fmt.fmtFlags.sharpV) { $s = 4; continue; }
		/* */ $s = 5; continue;
		/* if (p.fmt.fmtFlags.sharpV) { */ case 4:
			_tuple$2 = $assertType(p.arg, GoStringer, true);
			stringer = _tuple$2[0];
			ok$1 = _tuple$2[1];
			/* */ if (ok$1) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (ok$1) { */ case 7:
				handled = true;
				$deferred.push([$methodVal(p, "catchPanic"), [p.arg, verb]]);
				_r$2 = stringer.GoString(); /* */ $s = 9; case 9: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				$r = p.fmt.fmt_s(_r$2); /* */ $s = 10; case 10: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				return handled;
			/* } */ case 8:
			$s = 6; continue;
		/* } else { */ case 5:
				_13 = verb;
				/* */ if ((_13 === (118)) || (_13 === (115)) || (_13 === (120)) || (_13 === (88)) || (_13 === (113))) { $s = 12; continue; }
				/* */ $s = 13; continue;
				/* if ((_13 === (118)) || (_13 === (115)) || (_13 === (120)) || (_13 === (88)) || (_13 === (113))) { */ case 12:
					_ref = p.arg;
					/* */ if ($assertType(_ref, $error, true)[1]) { $s = 14; continue; }
					/* */ if ($assertType(_ref, Stringer, true)[1]) { $s = 15; continue; }
					/* */ $s = 16; continue;
					/* if ($assertType(_ref, $error, true)[1]) { */ case 14:
						v = _ref;
						handled = true;
						$deferred.push([$methodVal(p, "catchPanic"), [p.arg, verb]]);
						_r$3 = v.Error(); /* */ $s = 17; case 17: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
						_r$4 = p.printArg(new $String(_r$3), verb, depth); /* */ $s = 18; case 18: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
						_r$4;
						return handled;
					/* } else if ($assertType(_ref, Stringer, true)[1]) { */ case 15:
						v$1 = _ref;
						handled = true;
						$deferred.push([$methodVal(p, "catchPanic"), [p.arg, verb]]);
						_r$5 = v$1.String(); /* */ $s = 19; case 19: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
						_r$6 = p.printArg(new $String(_r$5), verb, depth); /* */ $s = 20; case 20: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
						_r$6;
						return handled;
					/* } */ case 16:
				/* } */ case 13:
			case 11:
		/* } */ case 6:
		handled = false;
		return handled;
		/* */ } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if (!$curGoroutine.asleep) { return  handled; } if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: pp.ptr.prototype.handleMethods }; } $f.$ptr = $ptr; $f._13 = _13; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._ref = _ref; $f._tuple = _tuple; $f._tuple$1 = _tuple$1; $f._tuple$2 = _tuple$2; $f.depth = depth; $f.formatter = formatter; $f.handled = handled; $f.ok = ok; $f.ok$1 = ok$1; $f.p = p; $f.stringer = stringer; $f.v = v; $f.v$1 = v$1; $f.verb = verb; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	pp.prototype.handleMethods = function(verb, depth) { return this.$val.handleMethods(verb, depth); };
	pp.ptr.prototype.printArg = function(arg, verb, depth) {
		var $ptr, _14, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, _r$8, _ref, arg, depth, f, f$1, f$10, f$11, f$12, f$13, f$14, f$15, f$16, f$17, f$18, f$19, f$2, f$3, f$4, f$5, f$6, f$7, f$8, f$9, handled, p, verb, wasString, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _14 = $f._14; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _r$8 = $f._r$8; _ref = $f._ref; arg = $f.arg; depth = $f.depth; f = $f.f; f$1 = $f.f$1; f$10 = $f.f$10; f$11 = $f.f$11; f$12 = $f.f$12; f$13 = $f.f$13; f$14 = $f.f$14; f$15 = $f.f$15; f$16 = $f.f$16; f$17 = $f.f$17; f$18 = $f.f$18; f$19 = $f.f$19; f$2 = $f.f$2; f$3 = $f.f$3; f$4 = $f.f$4; f$5 = $f.f$5; f$6 = $f.f$6; f$7 = $f.f$7; f$8 = $f.f$8; f$9 = $f.f$9; handled = $f.handled; p = $f.p; verb = $f.verb; wasString = $f.wasString; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		wasString = false;
		p = this;
		p.arg = arg;
		p.value = new reflect.Value.ptr(ptrType.nil, 0, 0);
		/* */ if ($interfaceIsEqual(arg, $ifaceNil)) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if ($interfaceIsEqual(arg, $ifaceNil)) { */ case 1:
			/* */ if ((verb === 84) || (verb === 118)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if ((verb === 84) || (verb === 118)) { */ case 3:
				p.fmt.pad(nilAngleBytes);
				$s = 5; continue;
			/* } else { */ case 4:
				$r = p.badVerb(verb); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 5:
			wasString = false;
			return wasString;
		/* } */ case 2:
			_14 = verb;
			/* */ if (_14 === (84)) { $s = 8; continue; }
			/* */ if (_14 === (112)) { $s = 9; continue; }
			/* */ $s = 10; continue;
			/* if (_14 === (84)) { */ case 8:
				_r$2 = reflect.TypeOf(arg).String(); /* */ $s = 11; case 11: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_r$3 = p.printArg(new $String(_r$2), 115, 0); /* */ $s = 12; case 12: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				_r$3;
				wasString = false;
				return wasString;
			/* } else if (_14 === (112)) { */ case 9:
				_r$4 = reflect.ValueOf(arg); /* */ $s = 13; case 13: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				$r = p.fmtPointer(_r$4, verb); /* */ $s = 14; case 14: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				wasString = false;
				return wasString;
			/* } */ case 10:
		case 7:
		_ref = arg;
		/* */ if ($assertType(_ref, $Bool, true)[1]) { $s = 15; continue; }
		/* */ if ($assertType(_ref, $Float32, true)[1]) { $s = 16; continue; }
		/* */ if ($assertType(_ref, $Float64, true)[1]) { $s = 17; continue; }
		/* */ if ($assertType(_ref, $Complex64, true)[1]) { $s = 18; continue; }
		/* */ if ($assertType(_ref, $Complex128, true)[1]) { $s = 19; continue; }
		/* */ if ($assertType(_ref, $Int, true)[1]) { $s = 20; continue; }
		/* */ if ($assertType(_ref, $Int8, true)[1]) { $s = 21; continue; }
		/* */ if ($assertType(_ref, $Int16, true)[1]) { $s = 22; continue; }
		/* */ if ($assertType(_ref, $Int32, true)[1]) { $s = 23; continue; }
		/* */ if ($assertType(_ref, $Int64, true)[1]) { $s = 24; continue; }
		/* */ if ($assertType(_ref, $Uint, true)[1]) { $s = 25; continue; }
		/* */ if ($assertType(_ref, $Uint8, true)[1]) { $s = 26; continue; }
		/* */ if ($assertType(_ref, $Uint16, true)[1]) { $s = 27; continue; }
		/* */ if ($assertType(_ref, $Uint32, true)[1]) { $s = 28; continue; }
		/* */ if ($assertType(_ref, $Uint64, true)[1]) { $s = 29; continue; }
		/* */ if ($assertType(_ref, $Uintptr, true)[1]) { $s = 30; continue; }
		/* */ if ($assertType(_ref, $String, true)[1]) { $s = 31; continue; }
		/* */ if ($assertType(_ref, sliceType, true)[1]) { $s = 32; continue; }
		/* */ if ($assertType(_ref, reflect.Value, true)[1]) { $s = 33; continue; }
		/* */ $s = 34; continue;
		/* if ($assertType(_ref, $Bool, true)[1]) { */ case 15:
			f = _ref.$val;
			$r = p.fmtBool(f, verb); /* */ $s = 36; case 36: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Float32, true)[1]) { */ case 16:
			f$1 = _ref.$val;
			$r = p.fmtFloat32(f$1, verb); /* */ $s = 37; case 37: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Float64, true)[1]) { */ case 17:
			f$2 = _ref.$val;
			$r = p.fmtFloat64(f$2, verb); /* */ $s = 38; case 38: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Complex64, true)[1]) { */ case 18:
			f$3 = _ref.$val;
			$r = p.fmtComplex64(f$3, verb); /* */ $s = 39; case 39: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Complex128, true)[1]) { */ case 19:
			f$4 = _ref.$val;
			$r = p.fmtComplex128(f$4, verb); /* */ $s = 40; case 40: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Int, true)[1]) { */ case 20:
			f$5 = _ref.$val;
			$r = p.fmtInt64(new $Int64(0, f$5), verb); /* */ $s = 41; case 41: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Int8, true)[1]) { */ case 21:
			f$6 = _ref.$val;
			$r = p.fmtInt64(new $Int64(0, f$6), verb); /* */ $s = 42; case 42: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Int16, true)[1]) { */ case 22:
			f$7 = _ref.$val;
			$r = p.fmtInt64(new $Int64(0, f$7), verb); /* */ $s = 43; case 43: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Int32, true)[1]) { */ case 23:
			f$8 = _ref.$val;
			$r = p.fmtInt64(new $Int64(0, f$8), verb); /* */ $s = 44; case 44: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Int64, true)[1]) { */ case 24:
			f$9 = _ref.$val;
			$r = p.fmtInt64(f$9, verb); /* */ $s = 45; case 45: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Uint, true)[1]) { */ case 25:
			f$10 = _ref.$val;
			$r = p.fmtUint64(new $Uint64(0, f$10), verb); /* */ $s = 46; case 46: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Uint8, true)[1]) { */ case 26:
			f$11 = _ref.$val;
			$r = p.fmtUint64(new $Uint64(0, f$11), verb); /* */ $s = 47; case 47: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Uint16, true)[1]) { */ case 27:
			f$12 = _ref.$val;
			$r = p.fmtUint64(new $Uint64(0, f$12), verb); /* */ $s = 48; case 48: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Uint32, true)[1]) { */ case 28:
			f$13 = _ref.$val;
			$r = p.fmtUint64(new $Uint64(0, f$13), verb); /* */ $s = 49; case 49: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Uint64, true)[1]) { */ case 29:
			f$14 = _ref.$val;
			$r = p.fmtUint64(f$14, verb); /* */ $s = 50; case 50: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $Uintptr, true)[1]) { */ case 30:
			f$15 = _ref.$val;
			$r = p.fmtUint64(new $Uint64(0, f$15.constructor === Number ? f$15 : 1), verb); /* */ $s = 51; case 51: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			$s = 35; continue;
		/* } else if ($assertType(_ref, $String, true)[1]) { */ case 31:
			f$16 = _ref.$val;
			$r = p.fmtString(f$16, verb); /* */ $s = 52; case 52: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			wasString = (verb === 115) || (verb === 118);
			$s = 35; continue;
		/* } else if ($assertType(_ref, sliceType, true)[1]) { */ case 32:
			f$17 = _ref.$val;
			$r = p.fmtBytes(f$17, verb, $ifaceNil, depth); /* */ $s = 53; case 53: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			wasString = verb === 115;
			$s = 35; continue;
		/* } else if ($assertType(_ref, reflect.Value, true)[1]) { */ case 33:
			f$18 = _ref.$val;
			_r$5 = p.printReflectValue(f$18, verb, depth); /* */ $s = 54; case 54: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
			wasString = _r$5;
			/* */ $s = 55; case 55:
			return wasString;
		/* } else { */ case 34:
			f$19 = _ref;
			_r$6 = p.handleMethods(verb, depth); /* */ $s = 56; case 56: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
			handled = _r$6;
			if (handled) {
				wasString = false;
				return wasString;
			}
			_r$7 = reflect.ValueOf(arg); /* */ $s = 57; case 57: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
			_r$8 = p.printReflectValue(_r$7, verb, depth); /* */ $s = 58; case 58: if($c) { $c = false; _r$8 = _r$8.$blk(); } if (_r$8 && _r$8.$blk !== undefined) { break s; }
			wasString = _r$8;
			/* */ $s = 59; case 59:
			return wasString;
		/* } */ case 35:
		p.arg = $ifaceNil;
		return wasString;
		/* */ } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.printArg }; } $f.$ptr = $ptr; $f._14 = _14; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._r$8 = _r$8; $f._ref = _ref; $f.arg = arg; $f.depth = depth; $f.f = f; $f.f$1 = f$1; $f.f$10 = f$10; $f.f$11 = f$11; $f.f$12 = f$12; $f.f$13 = f$13; $f.f$14 = f$14; $f.f$15 = f$15; $f.f$16 = f$16; $f.f$17 = f$17; $f.f$18 = f$18; $f.f$19 = f$19; $f.f$2 = f$2; $f.f$3 = f$3; $f.f$4 = f$4; $f.f$5 = f$5; $f.f$6 = f$6; $f.f$7 = f$7; $f.f$8 = f$8; $f.f$9 = f$9; $f.handled = handled; $f.p = p; $f.verb = verb; $f.wasString = wasString; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.printArg = function(arg, verb, depth) { return this.$val.printArg(arg, verb, depth); };
	pp.ptr.prototype.printValue = function(value, verb, depth) {
		var $ptr, _15, _r$2, _r$3, _r$4, _r$5, _r$6, depth, handled, p, value, verb, wasString, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _15 = $f._15; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; depth = $f.depth; handled = $f.handled; p = $f.p; value = $f.value; verb = $f.verb; wasString = $f.wasString; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		wasString = false;
		value = value;
		p = this;
		/* */ if (!value.IsValid()) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (!value.IsValid()) { */ case 1:
			/* */ if ((verb === 84) || (verb === 118)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if ((verb === 84) || (verb === 118)) { */ case 3:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilAngleBytes);
				$s = 5; continue;
			/* } else { */ case 4:
				$r = p.badVerb(verb); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 5:
			wasString = false;
			return wasString;
		/* } */ case 2:
			_15 = verb;
			/* */ if (_15 === (84)) { $s = 8; continue; }
			/* */ if (_15 === (112)) { $s = 9; continue; }
			/* */ $s = 10; continue;
			/* if (_15 === (84)) { */ case 8:
				_r$2 = value.Type().String(); /* */ $s = 11; case 11: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_r$3 = p.printArg(new $String(_r$2), 115, 0); /* */ $s = 12; case 12: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				_r$3;
				wasString = false;
				return wasString;
			/* } else if (_15 === (112)) { */ case 9:
				$r = p.fmtPointer(value, verb); /* */ $s = 13; case 13: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				wasString = false;
				return wasString;
			/* } */ case 10:
		case 7:
		p.arg = $ifaceNil;
		/* */ if (value.CanInterface()) { $s = 14; continue; }
		/* */ $s = 15; continue;
		/* if (value.CanInterface()) { */ case 14:
			_r$4 = value.Interface(); /* */ $s = 16; case 16: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			p.arg = _r$4;
		/* } */ case 15:
		_r$5 = p.handleMethods(verb, depth); /* */ $s = 17; case 17: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
		handled = _r$5;
		if (handled) {
			wasString = false;
			return wasString;
		}
		_r$6 = p.printReflectValue(value, verb, depth); /* */ $s = 18; case 18: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
		wasString = _r$6;
		/* */ $s = 19; case 19:
		return wasString;
		/* */ } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.printValue }; } $f.$ptr = $ptr; $f._15 = _15; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f.depth = depth; $f.handled = handled; $f.p = p; $f.value = value; $f.verb = verb; $f.wasString = wasString; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.printValue = function(value, verb, depth) { return this.$val.printValue(value, verb, depth); };
	pp.ptr.prototype.printReflectValue = function(value, verb, depth) {
		var $ptr, _16, _17, _i, _i$1, _r$10, _r$11, _r$12, _r$13, _r$14, _r$15, _r$16, _r$17, _r$18, _r$19, _r$2, _r$20, _r$21, _r$22, _r$23, _r$24, _r$25, _r$26, _r$27, _r$28, _r$29, _r$3, _r$30, _r$31, _r$32, _r$33, _r$34, _r$35, _r$4, _r$5, _r$6, _r$7, _r$8, _r$9, _ref, _ref$1, _v, a, bytes, depth, f, f$1, i, i$1, i$2, i$3, key, keys, oldValue, p, t, typ, v, v$1, value, value$1, verb, wasString, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _16 = $f._16; _17 = $f._17; _i = $f._i; _i$1 = $f._i$1; _r$10 = $f._r$10; _r$11 = $f._r$11; _r$12 = $f._r$12; _r$13 = $f._r$13; _r$14 = $f._r$14; _r$15 = $f._r$15; _r$16 = $f._r$16; _r$17 = $f._r$17; _r$18 = $f._r$18; _r$19 = $f._r$19; _r$2 = $f._r$2; _r$20 = $f._r$20; _r$21 = $f._r$21; _r$22 = $f._r$22; _r$23 = $f._r$23; _r$24 = $f._r$24; _r$25 = $f._r$25; _r$26 = $f._r$26; _r$27 = $f._r$27; _r$28 = $f._r$28; _r$29 = $f._r$29; _r$3 = $f._r$3; _r$30 = $f._r$30; _r$31 = $f._r$31; _r$32 = $f._r$32; _r$33 = $f._r$33; _r$34 = $f._r$34; _r$35 = $f._r$35; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _r$8 = $f._r$8; _r$9 = $f._r$9; _ref = $f._ref; _ref$1 = $f._ref$1; _v = $f._v; a = $f.a; bytes = $f.bytes; depth = $f.depth; f = $f.f; f$1 = $f.f$1; i = $f.i; i$1 = $f.i$1; i$2 = $f.i$2; i$3 = $f.i$3; key = $f.key; keys = $f.keys; oldValue = $f.oldValue; p = $f.p; t = $f.t; typ = $f.typ; v = $f.v; v$1 = $f.v$1; value = $f.value; value$1 = $f.value$1; verb = $f.verb; wasString = $f.wasString; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		wasString = false;
		value = value;
		p = this;
		oldValue = p.value;
		p.value = value;
			f = value;
			_16 = f.Kind();
			/* */ if (_16 === (0)) { $s = 2; continue; }
			/* */ if (_16 === (1)) { $s = 3; continue; }
			/* */ if ((_16 === (2)) || (_16 === (3)) || (_16 === (4)) || (_16 === (5)) || (_16 === (6))) { $s = 4; continue; }
			/* */ if ((_16 === (7)) || (_16 === (8)) || (_16 === (9)) || (_16 === (10)) || (_16 === (11)) || (_16 === (12))) { $s = 5; continue; }
			/* */ if ((_16 === (13)) || (_16 === (14))) { $s = 6; continue; }
			/* */ if ((_16 === (15)) || (_16 === (16))) { $s = 7; continue; }
			/* */ if (_16 === (24)) { $s = 8; continue; }
			/* */ if (_16 === (21)) { $s = 9; continue; }
			/* */ if (_16 === (25)) { $s = 10; continue; }
			/* */ if (_16 === (20)) { $s = 11; continue; }
			/* */ if ((_16 === (17)) || (_16 === (23))) { $s = 12; continue; }
			/* */ if (_16 === (22)) { $s = 13; continue; }
			/* */ if ((_16 === (18)) || (_16 === (19)) || (_16 === (26))) { $s = 14; continue; }
			/* */ $s = 15; continue;
			/* if (_16 === (0)) { */ case 2:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString("<invalid reflect.Value>");
				$s = 16; continue;
			/* } else if (_16 === (1)) { */ case 3:
				$r = p.fmtBool(f.Bool(), verb); /* */ $s = 17; case 17: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				$s = 16; continue;
			/* } else if ((_16 === (2)) || (_16 === (3)) || (_16 === (4)) || (_16 === (5)) || (_16 === (6))) { */ case 4:
				$r = p.fmtInt64(f.Int(), verb); /* */ $s = 18; case 18: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				$s = 16; continue;
			/* } else if ((_16 === (7)) || (_16 === (8)) || (_16 === (9)) || (_16 === (10)) || (_16 === (11)) || (_16 === (12))) { */ case 5:
				$r = p.fmtUint64(f.Uint(), verb); /* */ $s = 19; case 19: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				$s = 16; continue;
			/* } else if ((_16 === (13)) || (_16 === (14))) { */ case 6:
				_r$2 = f.Type().Size(); /* */ $s = 23; case 23: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				/* */ if (_r$2 === 4) { $s = 20; continue; }
				/* */ $s = 21; continue;
				/* if (_r$2 === 4) { */ case 20:
					$r = p.fmtFloat32($fround(f.Float()), verb); /* */ $s = 24; case 24: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
					$s = 22; continue;
				/* } else { */ case 21:
					$r = p.fmtFloat64(f.Float(), verb); /* */ $s = 25; case 25: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				/* } */ case 22:
				$s = 16; continue;
			/* } else if ((_16 === (15)) || (_16 === (16))) { */ case 7:
				_r$3 = f.Type().Size(); /* */ $s = 29; case 29: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				/* */ if (_r$3 === 8) { $s = 26; continue; }
				/* */ $s = 27; continue;
				/* if (_r$3 === 8) { */ case 26:
					$r = p.fmtComplex64((x = f.Complex(), new $Complex64(x.$real, x.$imag)), verb); /* */ $s = 30; case 30: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
					$s = 28; continue;
				/* } else { */ case 27:
					$r = p.fmtComplex128(f.Complex(), verb); /* */ $s = 31; case 31: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				/* } */ case 28:
				$s = 16; continue;
			/* } else if (_16 === (24)) { */ case 8:
				_r$4 = f.String(); /* */ $s = 32; case 32: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				$r = p.fmtString(_r$4, verb); /* */ $s = 33; case 33: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				$s = 16; continue;
			/* } else if (_16 === (21)) { */ case 9:
				/* */ if (p.fmt.fmtFlags.sharpV) { $s = 34; continue; }
				/* */ $s = 35; continue;
				/* if (p.fmt.fmtFlags.sharpV) { */ case 34:
					_r$5 = f.Type().String(); /* */ $s = 37; case 37: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
					_r$6 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$5); /* */ $s = 38; case 38: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
					_r$6;
					if (f.IsNil()) {
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString("(nil)");
						/* break; */ $s = 1; continue;
					}
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(123);
					$s = 36; continue;
				/* } else { */ case 35:
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(mapBytes);
				/* } */ case 36:
				_r$7 = f.MapKeys(); /* */ $s = 39; case 39: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
				keys = _r$7;
				_ref = keys;
				_i = 0;
				/* while (true) { */ case 40:
					/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 41; continue; }
					i = _i;
					key = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
					if (i > 0) {
						if (p.fmt.fmtFlags.sharpV) {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(commaSpaceBytes);
						} else {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(32);
						}
					}
					_r$8 = p.printValue(key, verb, depth + 1 >> 0); /* */ $s = 42; case 42: if($c) { $c = false; _r$8 = _r$8.$blk(); } if (_r$8 && _r$8.$blk !== undefined) { break s; }
					_r$8;
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(58);
					_r$9 = f.MapIndex(key); /* */ $s = 43; case 43: if($c) { $c = false; _r$9 = _r$9.$blk(); } if (_r$9 && _r$9.$blk !== undefined) { break s; }
					_r$10 = p.printValue(_r$9, verb, depth + 1 >> 0); /* */ $s = 44; case 44: if($c) { $c = false; _r$10 = _r$10.$blk(); } if (_r$10 && _r$10.$blk !== undefined) { break s; }
					_r$10;
					_i++;
				/* } */ $s = 40; continue; case 41:
				if (p.fmt.fmtFlags.sharpV) {
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(125);
				} else {
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(93);
				}
				$s = 16; continue;
			/* } else if (_16 === (25)) { */ case 10:
				/* */ if (p.fmt.fmtFlags.sharpV) { $s = 45; continue; }
				/* */ $s = 46; continue;
				/* if (p.fmt.fmtFlags.sharpV) { */ case 45:
					_r$11 = value.Type().String(); /* */ $s = 47; case 47: if($c) { $c = false; _r$11 = _r$11.$blk(); } if (_r$11 && _r$11.$blk !== undefined) { break s; }
					_r$12 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$11); /* */ $s = 48; case 48: if($c) { $c = false; _r$12 = _r$12.$blk(); } if (_r$12 && _r$12.$blk !== undefined) { break s; }
					_r$12;
				/* } */ case 46:
				p.add(123);
				v = f;
				t = v.Type();
				i$1 = 0;
				/* while (true) { */ case 49:
					/* if (!(i$1 < v.NumField())) { break; } */ if(!(i$1 < v.NumField())) { $s = 50; continue; }
					if (i$1 > 0) {
						if (p.fmt.fmtFlags.sharpV) {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(commaSpaceBytes);
						} else {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(32);
						}
					}
					/* */ if (p.fmt.fmtFlags.plusV || p.fmt.fmtFlags.sharpV) { $s = 51; continue; }
					/* */ $s = 52; continue;
					/* if (p.fmt.fmtFlags.plusV || p.fmt.fmtFlags.sharpV) { */ case 51:
						_r$13 = t.Field(i$1); /* */ $s = 53; case 53: if($c) { $c = false; _r$13 = _r$13.$blk(); } if (_r$13 && _r$13.$blk !== undefined) { break s; }
						f$1 = $clone(_r$13, reflect.StructField);
						if (!(f$1.Name === "")) {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(f$1.Name);
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(58);
						}
					/* } */ case 52:
					_r$14 = getField(v, i$1); /* */ $s = 54; case 54: if($c) { $c = false; _r$14 = _r$14.$blk(); } if (_r$14 && _r$14.$blk !== undefined) { break s; }
					_r$15 = p.printValue(_r$14, verb, depth + 1 >> 0); /* */ $s = 55; case 55: if($c) { $c = false; _r$15 = _r$15.$blk(); } if (_r$15 && _r$15.$blk !== undefined) { break s; }
					_r$15;
					i$1 = i$1 + (1) >> 0;
				/* } */ $s = 49; continue; case 50:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(125);
				$s = 16; continue;
			/* } else if (_16 === (20)) { */ case 11:
				_r$16 = f.Elem(); /* */ $s = 56; case 56: if($c) { $c = false; _r$16 = _r$16.$blk(); } if (_r$16 && _r$16.$blk !== undefined) { break s; }
				value$1 = _r$16;
				/* */ if (!value$1.IsValid()) { $s = 57; continue; }
				/* */ $s = 58; continue;
				/* if (!value$1.IsValid()) { */ case 57:
					/* */ if (p.fmt.fmtFlags.sharpV) { $s = 60; continue; }
					/* */ $s = 61; continue;
					/* if (p.fmt.fmtFlags.sharpV) { */ case 60:
						_r$17 = f.Type().String(); /* */ $s = 63; case 63: if($c) { $c = false; _r$17 = _r$17.$blk(); } if (_r$17 && _r$17.$blk !== undefined) { break s; }
						_r$18 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$17); /* */ $s = 64; case 64: if($c) { $c = false; _r$18 = _r$18.$blk(); } if (_r$18 && _r$18.$blk !== undefined) { break s; }
						_r$18;
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilParenBytes);
						$s = 62; continue;
					/* } else { */ case 61:
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(nilAngleBytes);
					/* } */ case 62:
					$s = 59; continue;
				/* } else { */ case 58:
					_r$19 = p.printValue(value$1, verb, depth + 1 >> 0); /* */ $s = 65; case 65: if($c) { $c = false; _r$19 = _r$19.$blk(); } if (_r$19 && _r$19.$blk !== undefined) { break s; }
					wasString = _r$19;
				/* } */ case 59:
				$s = 16; continue;
			/* } else if ((_16 === (17)) || (_16 === (23))) { */ case 12:
				typ = f.Type();
				_r$20 = typ.Elem(); /* */ $s = 69; case 69: if($c) { $c = false; _r$20 = _r$20.$blk(); } if (_r$20 && _r$20.$blk !== undefined) { break s; }
				_r$21 = _r$20.Kind(); /* */ $s = 70; case 70: if($c) { $c = false; _r$21 = _r$21.$blk(); } if (_r$21 && _r$21.$blk !== undefined) { break s; }
				if (!(_r$21 === 8)) { _v = false; $s = 68; continue s; }
				_r$22 = typ.Elem(); /* */ $s = 71; case 71: if($c) { $c = false; _r$22 = _r$22.$blk(); } if (_r$22 && _r$22.$blk !== undefined) { break s; }
				_v = $interfaceIsEqual(_r$22, byteType) || (verb === 115) || (verb === 113) || (verb === 120); case 68:
				/* */ if (_v) { $s = 66; continue; }
				/* */ $s = 67; continue;
				/* if (_v) { */ case 66:
					bytes = sliceType.nil;
					/* */ if (f.Kind() === 23) { $s = 72; continue; }
					/* */ if (f.CanAddr()) { $s = 73; continue; }
					/* */ $s = 74; continue;
					/* if (f.Kind() === 23) { */ case 72:
						_r$23 = f.Bytes(); /* */ $s = 76; case 76: if($c) { $c = false; _r$23 = _r$23.$blk(); } if (_r$23 && _r$23.$blk !== undefined) { break s; }
						bytes = _r$23;
						$s = 75; continue;
					/* } else if (f.CanAddr()) { */ case 73:
						_r$24 = f.Slice(0, f.Len()); /* */ $s = 77; case 77: if($c) { $c = false; _r$24 = _r$24.$blk(); } if (_r$24 && _r$24.$blk !== undefined) { break s; }
						_r$25 = _r$24.Bytes(); /* */ $s = 78; case 78: if($c) { $c = false; _r$25 = _r$25.$blk(); } if (_r$25 && _r$25.$blk !== undefined) { break s; }
						bytes = _r$25;
						$s = 75; continue;
					/* } else { */ case 74:
						bytes = $makeSlice(sliceType, f.Len());
						_ref$1 = bytes;
						_i$1 = 0;
						/* while (true) { */ case 79:
							/* if (!(_i$1 < _ref$1.$length)) { break; } */ if(!(_i$1 < _ref$1.$length)) { $s = 80; continue; }
							i$2 = _i$1;
							_r$26 = f.Index(i$2); /* */ $s = 81; case 81: if($c) { $c = false; _r$26 = _r$26.$blk(); } if (_r$26 && _r$26.$blk !== undefined) { break s; }
							_r$27 = _r$26.Uint(); /* */ $s = 82; case 82: if($c) { $c = false; _r$27 = _r$27.$blk(); } if (_r$27 && _r$27.$blk !== undefined) { break s; }
							((i$2 < 0 || i$2 >= bytes.$length) ? $throwRuntimeError("index out of range") : bytes.$array[bytes.$offset + i$2] = (_r$27.$low << 24 >>> 24));
							_i$1++;
						/* } */ $s = 79; continue; case 80:
					/* } */ case 75:
					$r = p.fmtBytes(bytes, verb, typ, depth); /* */ $s = 83; case 83: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
					wasString = verb === 115;
					/* break; */ $s = 1; continue;
				/* } */ case 67:
				/* */ if (p.fmt.fmtFlags.sharpV) { $s = 84; continue; }
				/* */ $s = 85; continue;
				/* if (p.fmt.fmtFlags.sharpV) { */ case 84:
					_r$28 = value.Type().String(); /* */ $s = 87; case 87: if($c) { $c = false; _r$28 = _r$28.$blk(); } if (_r$28 && _r$28.$blk !== undefined) { break s; }
					_r$29 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$28); /* */ $s = 88; case 88: if($c) { $c = false; _r$29 = _r$29.$blk(); } if (_r$29 && _r$29.$blk !== undefined) { break s; }
					_r$29;
					if ((f.Kind() === 23) && f.IsNil()) {
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString("(nil)");
						/* break; */ $s = 1; continue;
					}
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(123);
					$s = 86; continue;
				/* } else { */ case 85:
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(91);
				/* } */ case 86:
				i$3 = 0;
				/* while (true) { */ case 89:
					/* if (!(i$3 < f.Len())) { break; } */ if(!(i$3 < f.Len())) { $s = 90; continue; }
					if (i$3 > 0) {
						if (p.fmt.fmtFlags.sharpV) {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(commaSpaceBytes);
						} else {
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(32);
						}
					}
					_r$30 = f.Index(i$3); /* */ $s = 91; case 91: if($c) { $c = false; _r$30 = _r$30.$blk(); } if (_r$30 && _r$30.$blk !== undefined) { break s; }
					_r$31 = p.printValue(_r$30, verb, depth + 1 >> 0); /* */ $s = 92; case 92: if($c) { $c = false; _r$31 = _r$31.$blk(); } if (_r$31 && _r$31.$blk !== undefined) { break s; }
					_r$31;
					i$3 = i$3 + (1) >> 0;
				/* } */ $s = 89; continue; case 90:
				if (p.fmt.fmtFlags.sharpV) {
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(125);
				} else {
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(93);
				}
				$s = 16; continue;
			/* } else if (_16 === (22)) { */ case 13:
				v$1 = f.Pointer();
				/* */ if (!((v$1 === 0)) && (depth === 0)) { $s = 93; continue; }
				/* */ $s = 94; continue;
				/* if (!((v$1 === 0)) && (depth === 0)) { */ case 93:
						_r$32 = f.Elem(); /* */ $s = 96; case 96: if($c) { $c = false; _r$32 = _r$32.$blk(); } if (_r$32 && _r$32.$blk !== undefined) { break s; }
						a = _r$32;
						_17 = a.Kind();
						/* */ if ((_17 === (17)) || (_17 === (23))) { $s = 97; continue; }
						/* */ if (_17 === (25)) { $s = 98; continue; }
						/* */ if (_17 === (21)) { $s = 99; continue; }
						/* */ $s = 100; continue;
						/* if ((_17 === (17)) || (_17 === (23))) { */ case 97:
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(38);
							_r$33 = p.printValue(a, verb, depth + 1 >> 0); /* */ $s = 101; case 101: if($c) { $c = false; _r$33 = _r$33.$blk(); } if (_r$33 && _r$33.$blk !== undefined) { break s; }
							_r$33;
							/* break BigSwitch; */ $s = 1; continue s;
							$s = 100; continue;
						/* } else if (_17 === (25)) { */ case 98:
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(38);
							_r$34 = p.printValue(a, verb, depth + 1 >> 0); /* */ $s = 102; case 102: if($c) { $c = false; _r$34 = _r$34.$blk(); } if (_r$34 && _r$34.$blk !== undefined) { break s; }
							_r$34;
							/* break BigSwitch; */ $s = 1; continue s;
							$s = 100; continue;
						/* } else if (_17 === (21)) { */ case 99:
							(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(38);
							_r$35 = p.printValue(a, verb, depth + 1 >> 0); /* */ $s = 103; case 103: if($c) { $c = false; _r$35 = _r$35.$blk(); } if (_r$35 && _r$35.$blk !== undefined) { break s; }
							_r$35;
							/* break BigSwitch; */ $s = 1; continue s;
						/* } */ case 100:
					case 95:
				/* } */ case 94:
				$r = p.fmtPointer(value, verb); /* */ $s = 104; case 104: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				$s = 16; continue;
			/* } else if ((_16 === (18)) || (_16 === (19)) || (_16 === (26))) { */ case 14:
				$r = p.fmtPointer(value, verb); /* */ $s = 105; case 105: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
				$s = 16; continue;
			/* } else { */ case 15:
				$r = p.unknownType(f); /* */ $s = 106; case 106: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
			/* } */ case 16:
		case 1:
		p.value = oldValue;
		wasString = wasString;
		return wasString;
		/* */ } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.printReflectValue }; } $f.$ptr = $ptr; $f._16 = _16; $f._17 = _17; $f._i = _i; $f._i$1 = _i$1; $f._r$10 = _r$10; $f._r$11 = _r$11; $f._r$12 = _r$12; $f._r$13 = _r$13; $f._r$14 = _r$14; $f._r$15 = _r$15; $f._r$16 = _r$16; $f._r$17 = _r$17; $f._r$18 = _r$18; $f._r$19 = _r$19; $f._r$2 = _r$2; $f._r$20 = _r$20; $f._r$21 = _r$21; $f._r$22 = _r$22; $f._r$23 = _r$23; $f._r$24 = _r$24; $f._r$25 = _r$25; $f._r$26 = _r$26; $f._r$27 = _r$27; $f._r$28 = _r$28; $f._r$29 = _r$29; $f._r$3 = _r$3; $f._r$30 = _r$30; $f._r$31 = _r$31; $f._r$32 = _r$32; $f._r$33 = _r$33; $f._r$34 = _r$34; $f._r$35 = _r$35; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._r$8 = _r$8; $f._r$9 = _r$9; $f._ref = _ref; $f._ref$1 = _ref$1; $f._v = _v; $f.a = a; $f.bytes = bytes; $f.depth = depth; $f.f = f; $f.f$1 = f$1; $f.i = i; $f.i$1 = i$1; $f.i$2 = i$2; $f.i$3 = i$3; $f.key = key; $f.keys = keys; $f.oldValue = oldValue; $f.p = p; $f.t = t; $f.typ = typ; $f.v = v; $f.v$1 = v$1; $f.value = value; $f.value$1 = value$1; $f.verb = verb; $f.wasString = wasString; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.printReflectValue = function(value, verb, depth) { return this.$val.printReflectValue(value, verb, depth); };
	intFromArg = function(a, argNum) {
		var $ptr, _18, _r$2, _tuple, a, argNum, isInt, n, n$1, newArgNum, num, v, x, x$1, x$2, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _18 = $f._18; _r$2 = $f._r$2; _tuple = $f._tuple; a = $f.a; argNum = $f.argNum; isInt = $f.isInt; n = $f.n; n$1 = $f.n$1; newArgNum = $f.newArgNum; num = $f.num; v = $f.v; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		num = 0;
		isInt = false;
		newArgNum = 0;
		newArgNum = argNum;
		/* */ if (argNum < a.$length) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (argNum < a.$length) { */ case 1:
			_tuple = $assertType(((argNum < 0 || argNum >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + argNum]), $Int, true);
			num = _tuple[0];
			isInt = _tuple[1];
			/* */ if (!isInt) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!isInt) { */ case 3:
					_r$2 = reflect.ValueOf(((argNum < 0 || argNum >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + argNum])); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					v = _r$2;
					_18 = v.Kind();
					if ((_18 === (2)) || (_18 === (3)) || (_18 === (4)) || (_18 === (5)) || (_18 === (6))) {
						n = v.Int();
						if ((x = new $Int64(0, ((n.$low + ((n.$high >> 31) * 4294967296)) >> 0)), (x.$high === n.$high && x.$low === n.$low))) {
							num = ((n.$low + ((n.$high >> 31) * 4294967296)) >> 0);
							isInt = true;
						}
					} else if ((_18 === (7)) || (_18 === (8)) || (_18 === (9)) || (_18 === (10)) || (_18 === (11)) || (_18 === (12))) {
						n$1 = v.Uint();
						if ((x$1 = new $Int64(n$1.$high, n$1.$low), (x$1.$high > 0 || (x$1.$high === 0 && x$1.$low >= 0))) && (x$2 = new $Uint64(0, (n$1.$low >> 0)), (x$2.$high === n$1.$high && x$2.$low === n$1.$low))) {
							num = (n$1.$low >> 0);
							isInt = true;
						}
					}
				case 5:
			/* } */ case 4:
			newArgNum = argNum + 1 >> 0;
			if (tooLarge(num)) {
				num = 0;
				isInt = false;
			}
		/* } */ case 2:
		return [num, isInt, newArgNum];
		/* */ } return; } if ($f === undefined) { $f = { $blk: intFromArg }; } $f.$ptr = $ptr; $f._18 = _18; $f._r$2 = _r$2; $f._tuple = _tuple; $f.a = a; $f.argNum = argNum; $f.isInt = isInt; $f.n = n; $f.n$1 = n$1; $f.newArgNum = newArgNum; $f.num = num; $f.v = v; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.$s = $s; $f.$r = $r; return $f;
	};
	parseArgNumber = function(format) {
		var $ptr, _tmp, _tmp$1, _tmp$10, _tmp$11, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tmp$9, _tuple, format, i, index, newi, ok, ok$1, wid, width;
		index = 0;
		wid = 0;
		ok = false;
		if (format.length < 3) {
			_tmp = 0;
			_tmp$1 = 1;
			_tmp$2 = false;
			index = _tmp;
			wid = _tmp$1;
			ok = _tmp$2;
			return [index, wid, ok];
		}
		i = 1;
		while (true) {
			if (!(i < format.length)) { break; }
			if (format.charCodeAt(i) === 93) {
				_tuple = parsenum(format, 1, i);
				width = _tuple[0];
				ok$1 = _tuple[1];
				newi = _tuple[2];
				if (!ok$1 || !((newi === i))) {
					_tmp$3 = 0;
					_tmp$4 = i + 1 >> 0;
					_tmp$5 = false;
					index = _tmp$3;
					wid = _tmp$4;
					ok = _tmp$5;
					return [index, wid, ok];
				}
				_tmp$6 = width - 1 >> 0;
				_tmp$7 = i + 1 >> 0;
				_tmp$8 = true;
				index = _tmp$6;
				wid = _tmp$7;
				ok = _tmp$8;
				return [index, wid, ok];
			}
			i = i + (1) >> 0;
		}
		_tmp$9 = 0;
		_tmp$10 = 1;
		_tmp$11 = false;
		index = _tmp$9;
		wid = _tmp$10;
		ok = _tmp$11;
		return [index, wid, ok];
	};
	pp.ptr.prototype.argNumber = function(argNum, format, i, numArgs) {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tmp$8, _tuple, argNum, format, found, i, index, newArgNum, newi, numArgs, ok, p, wid;
		newArgNum = 0;
		newi = 0;
		found = false;
		p = this;
		if (format.length <= i || !((format.charCodeAt(i) === 91))) {
			_tmp = argNum;
			_tmp$1 = i;
			_tmp$2 = false;
			newArgNum = _tmp;
			newi = _tmp$1;
			found = _tmp$2;
			return [newArgNum, newi, found];
		}
		p.reordered = true;
		_tuple = parseArgNumber(format.substring(i));
		index = _tuple[0];
		wid = _tuple[1];
		ok = _tuple[2];
		if (ok && 0 <= index && index < numArgs) {
			_tmp$3 = index;
			_tmp$4 = i + wid >> 0;
			_tmp$5 = true;
			newArgNum = _tmp$3;
			newi = _tmp$4;
			found = _tmp$5;
			return [newArgNum, newi, found];
		}
		p.goodArgNum = false;
		_tmp$6 = argNum;
		_tmp$7 = i + wid >> 0;
		_tmp$8 = ok;
		newArgNum = _tmp$6;
		newi = _tmp$7;
		found = _tmp$8;
		return [newArgNum, newi, found];
	};
	pp.prototype.argNumber = function(argNum, format, i, numArgs) { return this.$val.argNumber(argNum, format, i, numArgs); };
	pp.ptr.prototype.doPrintf = function(format, a) {
		var $ptr, _19, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, _tuple, _tuple$1, _tuple$2, _tuple$3, _tuple$4, _tuple$5, _tuple$6, _tuple$7, a, afterIndex, arg, arg$1, argNum, c, end, format, i, lasti, p, w, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _19 = $f._19; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _tuple = $f._tuple; _tuple$1 = $f._tuple$1; _tuple$2 = $f._tuple$2; _tuple$3 = $f._tuple$3; _tuple$4 = $f._tuple$4; _tuple$5 = $f._tuple$5; _tuple$6 = $f._tuple$6; _tuple$7 = $f._tuple$7; a = $f.a; afterIndex = $f.afterIndex; arg = $f.arg; arg$1 = $f.arg$1; argNum = $f.argNum; c = $f.c; end = $f.end; format = $f.format; i = $f.i; lasti = $f.lasti; p = $f.p; w = $f.w; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = this;
		end = format.length;
		argNum = 0;
		afterIndex = false;
		p.reordered = false;
		i = 0;
		/* while (true) { */ case 1:
			/* if (!(i < end)) { break; } */ if(!(i < end)) { $s = 2; continue; }
			p.goodArgNum = true;
			lasti = i;
			while (true) {
				if (!(i < end && !((format.charCodeAt(i) === 37)))) { break; }
				i = i + (1) >> 0;
			}
			if (i > lasti) {
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(format.substring(lasti, i));
			}
			if (i >= end) {
				/* break; */ $s = 2; continue;
			}
			i = i + (1) >> 0;
			p.fmt.clearflags();
			F:
			while (true) {
				if (!(i < end)) { break; }
				_19 = format.charCodeAt(i);
				if (_19 === (35)) {
					p.fmt.fmtFlags.sharp = true;
				} else if (_19 === (48)) {
					p.fmt.fmtFlags.zero = true;
				} else if (_19 === (43)) {
					p.fmt.fmtFlags.plus = true;
				} else if (_19 === (45)) {
					p.fmt.fmtFlags.minus = true;
				} else if (_19 === (32)) {
					p.fmt.fmtFlags.space = true;
				} else {
					break F;
				}
				i = i + (1) >> 0;
			}
			_tuple = p.argNumber(argNum, format, i, a.$length);
			argNum = _tuple[0];
			i = _tuple[1];
			afterIndex = _tuple[2];
			/* */ if (i < end && (format.charCodeAt(i) === 42)) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (i < end && (format.charCodeAt(i) === 42)) { */ case 3:
				i = i + (1) >> 0;
				_r$2 = intFromArg(a, argNum); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_tuple$1 = _r$2;
				p.fmt.wid = _tuple$1[0];
				p.fmt.fmtFlags.widPresent = _tuple$1[1];
				argNum = _tuple$1[2];
				if (!p.fmt.fmtFlags.widPresent) {
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(badWidthBytes);
				}
				if (p.fmt.wid < 0) {
					p.fmt.wid = -p.fmt.wid;
					p.fmt.fmtFlags.minus = true;
				}
				afterIndex = false;
				$s = 5; continue;
			/* } else { */ case 4:
				_tuple$2 = parsenum(format, i, end);
				p.fmt.wid = _tuple$2[0];
				p.fmt.fmtFlags.widPresent = _tuple$2[1];
				i = _tuple$2[2];
				if (afterIndex && p.fmt.fmtFlags.widPresent) {
					p.goodArgNum = false;
				}
			/* } */ case 5:
			/* */ if ((i + 1 >> 0) < end && (format.charCodeAt(i) === 46)) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if ((i + 1 >> 0) < end && (format.charCodeAt(i) === 46)) { */ case 7:
				i = i + (1) >> 0;
				if (afterIndex) {
					p.goodArgNum = false;
				}
				_tuple$3 = p.argNumber(argNum, format, i, a.$length);
				argNum = _tuple$3[0];
				i = _tuple$3[1];
				afterIndex = _tuple$3[2];
				/* */ if (i < end && (format.charCodeAt(i) === 42)) { $s = 9; continue; }
				/* */ $s = 10; continue;
				/* if (i < end && (format.charCodeAt(i) === 42)) { */ case 9:
					i = i + (1) >> 0;
					_r$3 = intFromArg(a, argNum); /* */ $s = 12; case 12: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
					_tuple$4 = _r$3;
					p.fmt.prec = _tuple$4[0];
					p.fmt.fmtFlags.precPresent = _tuple$4[1];
					argNum = _tuple$4[2];
					if (p.fmt.prec < 0) {
						p.fmt.prec = 0;
						p.fmt.fmtFlags.precPresent = false;
					}
					if (!p.fmt.fmtFlags.precPresent) {
						(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(badPrecBytes);
					}
					afterIndex = false;
					$s = 11; continue;
				/* } else { */ case 10:
					_tuple$5 = parsenum(format, i, end);
					p.fmt.prec = _tuple$5[0];
					p.fmt.fmtFlags.precPresent = _tuple$5[1];
					i = _tuple$5[2];
					if (!p.fmt.fmtFlags.precPresent) {
						p.fmt.prec = 0;
						p.fmt.fmtFlags.precPresent = true;
					}
				/* } */ case 11:
			/* } */ case 8:
			if (!afterIndex) {
				_tuple$6 = p.argNumber(argNum, format, i, a.$length);
				argNum = _tuple$6[0];
				i = _tuple$6[1];
				afterIndex = _tuple$6[2];
			}
			/* */ if (i >= end) { $s = 13; continue; }
			/* */ $s = 14; continue;
			/* if (i >= end) { */ case 13:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(noVerbBytes);
				/* continue; */ $s = 1; continue;
			/* } */ case 14:
			_tuple$7 = utf8.DecodeRuneInString(format.substring(i));
			c = _tuple$7[0];
			w = _tuple$7[1];
			i = i + (w) >> 0;
			/* */ if (c === 37) { $s = 15; continue; }
			/* */ $s = 16; continue;
			/* if (c === 37) { */ case 15:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(37);
				/* continue; */ $s = 1; continue;
			/* } */ case 16:
			/* */ if (!p.goodArgNum) { $s = 17; continue; }
			/* */ if (argNum >= a.$length) { $s = 18; continue; }
			/* */ $s = 19; continue;
			/* if (!p.goodArgNum) { */ case 17:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(percentBangBytes);
				p.add(c);
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(badIndexBytes);
				/* continue; */ $s = 1; continue;
				$s = 19; continue;
			/* } else if (argNum >= a.$length) { */ case 18:
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(percentBangBytes);
				p.add(c);
				(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(missingBytes);
				/* continue; */ $s = 1; continue;
			/* } */ case 19:
			arg = ((argNum < 0 || argNum >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + argNum]);
			argNum = argNum + (1) >> 0;
			if (c === 118) {
				if (p.fmt.fmtFlags.sharp) {
					p.fmt.fmtFlags.sharp = false;
					p.fmt.fmtFlags.sharpV = true;
				}
				if (p.fmt.fmtFlags.plus) {
					p.fmt.fmtFlags.plus = false;
					p.fmt.fmtFlags.plusV = true;
				}
			}
			_r$4 = p.printArg(arg, c, 0); /* */ $s = 20; case 20: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			_r$4;
		/* } */ $s = 1; continue; case 2:
		/* */ if (!p.reordered && argNum < a.$length) { $s = 21; continue; }
		/* */ $s = 22; continue;
		/* if (!p.reordered && argNum < a.$length) { */ case 21:
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(extraBytes);
			/* while (true) { */ case 23:
				/* if (!(argNum < a.$length)) { break; } */ if(!(argNum < a.$length)) { $s = 24; continue; }
				arg$1 = ((argNum < 0 || argNum >= a.$length) ? $throwRuntimeError("index out of range") : a.$array[a.$offset + argNum]);
				/* */ if (!($interfaceIsEqual(arg$1, $ifaceNil))) { $s = 25; continue; }
				/* */ $s = 26; continue;
				/* if (!($interfaceIsEqual(arg$1, $ifaceNil))) { */ case 25:
					_r$5 = reflect.TypeOf(arg$1).String(); /* */ $s = 27; case 27: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
					_r$6 = (p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteString(_r$5); /* */ $s = 28; case 28: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
					_r$6;
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(61);
				/* } */ case 26:
				_r$7 = p.printArg(arg$1, 118, 0); /* */ $s = 29; case 29: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
				_r$7;
				if ((argNum + 1 >> 0) < a.$length) {
					(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).Write(commaSpaceBytes);
				}
				argNum = argNum + (1) >> 0;
			/* } */ $s = 23; continue; case 24:
			(p.$ptr_buf || (p.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, p))).WriteByte(41);
		/* } */ case 22:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: pp.ptr.prototype.doPrintf }; } $f.$ptr = $ptr; $f._19 = _19; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._tuple = _tuple; $f._tuple$1 = _tuple$1; $f._tuple$2 = _tuple$2; $f._tuple$3 = _tuple$3; $f._tuple$4 = _tuple$4; $f._tuple$5 = _tuple$5; $f._tuple$6 = _tuple$6; $f._tuple$7 = _tuple$7; $f.a = a; $f.afterIndex = afterIndex; $f.arg = arg; $f.arg$1 = arg$1; $f.argNum = argNum; $f.c = c; $f.end = end; $f.format = format; $f.i = i; $f.lasti = lasti; $f.p = p; $f.w = w; $f.$s = $s; $f.$r = $r; return $f;
	};
	pp.prototype.doPrintf = function(format, a) { return this.$val.doPrintf(format, a); };
	ss.ptr.prototype.Read = function(buf) {
		var $ptr, _tmp, _tmp$1, buf, err, n, s;
		n = 0;
		err = $ifaceNil;
		s = this;
		_tmp = 0;
		_tmp$1 = errors.New("ScanState's Read should not be called. Use ReadRune");
		n = _tmp;
		err = _tmp$1;
		return [n, err];
	};
	ss.prototype.Read = function(buf) { return this.$val.Read(buf); };
	ss.ptr.prototype.ReadRune = function() {
		var $ptr, _r$2, _tuple, err, r, s, size, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _tuple = $f._tuple; err = $f.err; r = $f.r; s = $f.s; size = $f.size; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		r = 0;
		size = 0;
		err = $ifaceNil;
		s = this;
		if (s.peekRune >= 0) {
			s.count = s.count + (1) >> 0;
			r = s.peekRune;
			size = utf8.RuneLen(r);
			s.prevRune = r;
			s.peekRune = -1;
			return [r, size, err];
		}
		if (s.atEOF || s.ssave.nlIsEnd && (s.prevRune === 10) || s.count >= s.ssave.argLimit) {
			err = io.EOF;
			return [r, size, err];
		}
		_r$2 = s.rr.ReadRune(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_tuple = _r$2;
		r = _tuple[0];
		size = _tuple[1];
		err = _tuple[2];
		if ($interfaceIsEqual(err, $ifaceNil)) {
			s.count = s.count + (1) >> 0;
			s.prevRune = r;
		} else if ($interfaceIsEqual(err, io.EOF)) {
			s.atEOF = true;
		}
		return [r, size, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.ReadRune }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._tuple = _tuple; $f.err = err; $f.r = r; $f.s = s; $f.size = size; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.ReadRune = function() { return this.$val.ReadRune(); };
	ss.ptr.prototype.Width = function() {
		var $ptr, _tmp, _tmp$1, _tmp$2, _tmp$3, ok, s, wid;
		wid = 0;
		ok = false;
		s = this;
		if (s.ssave.maxWid === 1073741824) {
			_tmp = 0;
			_tmp$1 = false;
			wid = _tmp;
			ok = _tmp$1;
			return [wid, ok];
		}
		_tmp$2 = s.ssave.maxWid;
		_tmp$3 = true;
		wid = _tmp$2;
		ok = _tmp$3;
		return [wid, ok];
	};
	ss.prototype.Width = function() { return this.$val.Width(); };
	ss.ptr.prototype.getRune = function() {
		var $ptr, _r$2, _tuple, err, r, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _tuple = $f._tuple; err = $f.err; r = $f.r; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		r = 0;
		s = this;
		_r$2 = s.ReadRune(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_tuple = _r$2;
		r = _tuple[0];
		err = _tuple[2];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			if ($interfaceIsEqual(err, io.EOF)) {
				r = -1;
				return r;
			}
			s.error(err);
		}
		return r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.getRune }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._tuple = _tuple; $f.err = err; $f.r = r; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.getRune = function() { return this.$val.getRune(); };
	ss.ptr.prototype.UnreadRune = function() {
		var $ptr, _r$2, _tuple, ok, s, u, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _tuple = $f._tuple; ok = $f.ok; s = $f.s; u = $f.u; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		s = this;
		_tuple = $assertType(s.rr, runeUnreader, true);
		u = _tuple[0];
		ok = _tuple[1];
		/* */ if (ok) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (ok) { */ case 1:
			_r$2 = u.UnreadRune(); /* */ $s = 4; case 4: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			_r$2;
			$s = 3; continue;
		/* } else { */ case 2:
			s.peekRune = s.prevRune;
		/* } */ case 3:
		s.prevRune = -1;
		s.count = s.count - (1) >> 0;
		return $ifaceNil;
		/* */ } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.UnreadRune }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._tuple = _tuple; $f.ok = ok; $f.s = s; $f.u = u; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.UnreadRune = function() { return this.$val.UnreadRune(); };
	ss.ptr.prototype.error = function(err) {
		var $ptr, err, s, x;
		s = this;
		$panic((x = new scanError.ptr(err), new x.constructor.elem(x)));
	};
	ss.prototype.error = function(err) { return this.$val.error(err); };
	ss.ptr.prototype.errorString = function(err) {
		var $ptr, err, s, x;
		s = this;
		$panic((x = new scanError.ptr(errors.New(err)), new x.constructor.elem(x)));
	};
	ss.prototype.errorString = function(err) { return this.$val.errorString(err); };
	ss.ptr.prototype.Token = function(skipSpace, f) {
		var $ptr, _r$2, err, f, s, skipSpace, tok, $s, $deferred, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; err = $f.err; f = $f.f; s = $f.s; skipSpace = $f.skipSpace; tok = $f.tok; $s = $f.$s; $deferred = $f.$deferred; $r = $f.$r; } var $err = null; try { s: while (true) { switch ($s) { case 0: $deferred = []; $deferred.index = $curGoroutine.deferStack.length; $curGoroutine.deferStack.push($deferred);
		err = [err];
		tok = sliceType.nil;
		err[0] = $ifaceNil;
		s = this;
		$deferred.push([(function(err) { return function() {
			var $ptr, _tuple, e, ok, se;
			e = $recover();
			if (!($interfaceIsEqual(e, $ifaceNil))) {
				_tuple = $assertType(e, scanError, true);
				se = $clone(_tuple[0], scanError);
				ok = _tuple[1];
				if (ok) {
					err[0] = se.err;
				} else {
					$panic(e);
				}
			}
		}; })(err), []]);
		if (f === $throwNilPointerError) {
			f = notSpace;
		}
		s.buf = $subslice(s.buf, 0, 0);
		_r$2 = s.token(skipSpace, f); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		tok = _r$2;
		return [tok, err[0]];
		/* */ } return; } } catch(err) { $err = err; $s = -1; } finally { $callDeferred($deferred, $err); if (!$curGoroutine.asleep) { return  [tok, err[0]]; } if($curGoroutine.asleep) { if ($f === undefined) { $f = { $blk: ss.ptr.prototype.Token }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f.err = err; $f.f = f; $f.s = s; $f.skipSpace = skipSpace; $f.tok = tok; $f.$s = $s; $f.$deferred = $deferred; $f.$r = $r; return $f; } }
	};
	ss.prototype.Token = function(skipSpace, f) { return this.$val.Token(skipSpace, f); };
	isSpace = function(r) {
		var $ptr, _i, _ref, r, rng, rx;
		if (r >= 65536) {
			return false;
		}
		rx = (r << 16 >>> 16);
		_ref = space;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			rng = $clone(((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]), arrayType$2);
			if (rx < rng[0]) {
				return false;
			}
			if (rx <= rng[1]) {
				return true;
			}
			_i++;
		}
		return false;
	};
	notSpace = function(r) {
		var $ptr, r;
		return !isSpace(r);
	};
	ss.ptr.prototype.SkipSpace = function() {
		var $ptr, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		s = this;
		$r = s.skipSpace(false); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.SkipSpace }; } $f.$ptr = $ptr; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.SkipSpace = function() { return this.$val.SkipSpace(); };
	ss.ptr.prototype.free = function(old) {
		var $ptr, old, s;
		old = $clone(old, ssave);
		s = this;
		if (old.validSave) {
			ssave.copy(s.ssave, old);
			return;
		}
		if (s.buf.$capacity > 1024) {
			return;
		}
		s.buf = $subslice(s.buf, 0, 0);
		s.rr = $ifaceNil;
		ssFree.Put(s);
	};
	ss.prototype.free = function(old) { return this.$val.free(old); };
	ss.ptr.prototype.skipSpace = function(stopAtNewline) {
		var $ptr, _r$2, _r$3, _r$4, _v, r, s, stopAtNewline, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _v = $f._v; r = $f.r; s = $f.s; stopAtNewline = $f.stopAtNewline; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		s = this;
		/* while (true) { */ case 1:
			_r$2 = s.getRune(); /* */ $s = 3; case 3: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			r = _r$2;
			if (r === -1) {
				return;
			}
			if (!(r === 13)) { _v = false; $s = 6; continue s; }
			_r$3 = s.peek("\n"); /* */ $s = 7; case 7: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_v = _r$3; case 6:
			/* */ if (_v) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if (_v) { */ case 4:
				/* continue; */ $s = 1; continue;
			/* } */ case 5:
			/* */ if (r === 10) { $s = 8; continue; }
			/* */ $s = 9; continue;
			/* if (r === 10) { */ case 8:
				if (stopAtNewline) {
					/* break; */ $s = 2; continue;
				}
				if (s.ssave.nlIsSpace) {
					/* continue; */ $s = 1; continue;
				}
				s.errorString("unexpected newline");
				return;
			/* } */ case 9:
			/* */ if (!isSpace(r)) { $s = 10; continue; }
			/* */ $s = 11; continue;
			/* if (!isSpace(r)) { */ case 10:
				_r$4 = s.UnreadRune(); /* */ $s = 12; case 12: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				_r$4;
				/* break; */ $s = 2; continue;
			/* } */ case 11:
		/* } */ $s = 1; continue; case 2:
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.skipSpace }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._v = _v; $f.r = r; $f.s = s; $f.stopAtNewline = stopAtNewline; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.skipSpace = function(stopAtNewline) { return this.$val.skipSpace(stopAtNewline); };
	ss.ptr.prototype.token = function(skipSpace, f) {
		var $ptr, _r$2, _r$3, _r$4, f, r, s, skipSpace, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; f = $f.f; r = $f.r; s = $f.s; skipSpace = $f.skipSpace; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		s = this;
		/* */ if (skipSpace) { $s = 1; continue; }
		/* */ $s = 2; continue;
		/* if (skipSpace) { */ case 1:
			$r = s.skipSpace(false); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* } */ case 2:
		/* while (true) { */ case 4:
			_r$2 = s.getRune(); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			r = _r$2;
			if (r === -1) {
				/* break; */ $s = 5; continue;
			}
			_r$3 = f(r); /* */ $s = 9; case 9: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			/* */ if (!_r$3) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (!_r$3) { */ case 7:
				_r$4 = s.UnreadRune(); /* */ $s = 10; case 10: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				_r$4;
				/* break; */ $s = 5; continue;
			/* } */ case 8:
			(s.$ptr_buf || (s.$ptr_buf = new ptrType$1(function() { return this.$target.buf; }, function($v) { this.$target.buf = $v; }, s))).WriteRune(r);
		/* } */ $s = 4; continue; case 5:
		return (x = s.buf, $subslice(new sliceType(x.$array), x.$offset, x.$offset + x.$length));
		/* */ } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.token }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f.f = f; $f.r = r; $f.s = s; $f.skipSpace = skipSpace; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.token = function(skipSpace, f) { return this.$val.token(skipSpace, f); };
	indexRune = function(s, r) {
		var $ptr, _i, _ref, _rune, c, i, r, s;
		_ref = s;
		_i = 0;
		while (true) {
			if (!(_i < _ref.length)) { break; }
			_rune = $decodeRune(_ref, _i);
			i = _i;
			c = _rune[0];
			if (c === r) {
				return i;
			}
			_i += _rune[1];
		}
		return -1;
	};
	ss.ptr.prototype.peek = function(ok) {
		var $ptr, _r$2, _r$3, ok, r, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; ok = $f.ok; r = $f.r; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		s = this;
		_r$2 = s.getRune(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		r = _r$2;
		/* */ if (!((r === -1))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!((r === -1))) { */ case 2:
			_r$3 = s.UnreadRune(); /* */ $s = 4; case 4: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_r$3;
		/* } */ case 3:
		return indexRune(ok, r) >= 0;
		/* */ } return; } if ($f === undefined) { $f = { $blk: ss.ptr.prototype.peek }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.ok = ok; $f.r = r; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	ss.prototype.peek = function(ok) { return this.$val.peek(ok); };
	ptrType$25.methods = [{prop: "clearflags", name: "clearflags", pkg: "fmt", typ: $funcType([], [], false)}, {prop: "init", name: "init", pkg: "fmt", typ: $funcType([ptrType$1], [], false)}, {prop: "computePadding", name: "computePadding", pkg: "fmt", typ: $funcType([$Int], [sliceType, $Int, $Int], false)}, {prop: "writePadding", name: "writePadding", pkg: "fmt", typ: $funcType([$Int, sliceType], [], false)}, {prop: "pad", name: "pad", pkg: "fmt", typ: $funcType([sliceType], [], false)}, {prop: "padString", name: "padString", pkg: "fmt", typ: $funcType([$String], [], false)}, {prop: "fmt_boolean", name: "fmt_boolean", pkg: "fmt", typ: $funcType([$Bool], [], false)}, {prop: "integer", name: "integer", pkg: "fmt", typ: $funcType([$Int64, $Uint64, $Bool, $String], [], false)}, {prop: "truncate", name: "truncate", pkg: "fmt", typ: $funcType([$String], [$String], false)}, {prop: "fmt_s", name: "fmt_s", pkg: "fmt", typ: $funcType([$String], [], false)}, {prop: "fmt_sbx", name: "fmt_sbx", pkg: "fmt", typ: $funcType([$String, sliceType, $String], [], false)}, {prop: "fmt_sx", name: "fmt_sx", pkg: "fmt", typ: $funcType([$String, $String], [], false)}, {prop: "fmt_bx", name: "fmt_bx", pkg: "fmt", typ: $funcType([sliceType, $String], [], false)}, {prop: "fmt_q", name: "fmt_q", pkg: "fmt", typ: $funcType([$String], [], false)}, {prop: "fmt_qc", name: "fmt_qc", pkg: "fmt", typ: $funcType([$Int64], [], false)}, {prop: "formatFloat", name: "formatFloat", pkg: "fmt", typ: $funcType([$Float64, $Uint8, $Int, $Int], [], false)}, {prop: "fmt_e64", name: "fmt_e64", pkg: "fmt", typ: $funcType([$Float64], [], false)}, {prop: "fmt_E64", name: "fmt_E64", pkg: "fmt", typ: $funcType([$Float64], [], false)}, {prop: "fmt_f64", name: "fmt_f64", pkg: "fmt", typ: $funcType([$Float64], [], false)}, {prop: "fmt_g64", name: "fmt_g64", pkg: "fmt", typ: $funcType([$Float64], [], false)}, {prop: "fmt_G64", name: "fmt_G64", pkg: "fmt", typ: $funcType([$Float64], [], false)}, {prop: "fmt_fb64", name: "fmt_fb64", pkg: "fmt", typ: $funcType([$Float64], [], false)}, {prop: "fmt_e32", name: "fmt_e32", pkg: "fmt", typ: $funcType([$Float32], [], false)}, {prop: "fmt_E32", name: "fmt_E32", pkg: "fmt", typ: $funcType([$Float32], [], false)}, {prop: "fmt_f32", name: "fmt_f32", pkg: "fmt", typ: $funcType([$Float32], [], false)}, {prop: "fmt_g32", name: "fmt_g32", pkg: "fmt", typ: $funcType([$Float32], [], false)}, {prop: "fmt_G32", name: "fmt_G32", pkg: "fmt", typ: $funcType([$Float32], [], false)}, {prop: "fmt_fb32", name: "fmt_fb32", pkg: "fmt", typ: $funcType([$Float32], [], false)}, {prop: "fmt_c64", name: "fmt_c64", pkg: "fmt", typ: $funcType([$Complex64, $Int32], [], false)}, {prop: "fmt_c128", name: "fmt_c128", pkg: "fmt", typ: $funcType([$Complex128, $Int32], [], false)}, {prop: "fmt_complex", name: "fmt_complex", pkg: "fmt", typ: $funcType([$Float64, $Float64, $Int, $Int32], [], false)}];
	ptrType$1.methods = [{prop: "Write", name: "Write", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}, {prop: "WriteString", name: "WriteString", pkg: "", typ: $funcType([$String], [$Int, $error], false)}, {prop: "WriteByte", name: "WriteByte", pkg: "", typ: $funcType([$Uint8], [$error], false)}, {prop: "WriteRune", name: "WriteRune", pkg: "", typ: $funcType([$Int32], [$error], false)}];
	ptrType$2.methods = [{prop: "free", name: "free", pkg: "fmt", typ: $funcType([], [], false)}, {prop: "Width", name: "Width", pkg: "", typ: $funcType([], [$Int, $Bool], false)}, {prop: "Precision", name: "Precision", pkg: "", typ: $funcType([], [$Int, $Bool], false)}, {prop: "Flag", name: "Flag", pkg: "", typ: $funcType([$Int], [$Bool], false)}, {prop: "add", name: "add", pkg: "fmt", typ: $funcType([$Int32], [], false)}, {prop: "Write", name: "Write", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}, {prop: "unknownType", name: "unknownType", pkg: "fmt", typ: $funcType([reflect.Value], [], false)}, {prop: "badVerb", name: "badVerb", pkg: "fmt", typ: $funcType([$Int32], [], false)}, {prop: "fmtBool", name: "fmtBool", pkg: "fmt", typ: $funcType([$Bool, $Int32], [], false)}, {prop: "fmtC", name: "fmtC", pkg: "fmt", typ: $funcType([$Int64], [], false)}, {prop: "fmtInt64", name: "fmtInt64", pkg: "fmt", typ: $funcType([$Int64, $Int32], [], false)}, {prop: "fmt0x64", name: "fmt0x64", pkg: "fmt", typ: $funcType([$Uint64, $Bool], [], false)}, {prop: "fmtUnicode", name: "fmtUnicode", pkg: "fmt", typ: $funcType([$Int64], [], false)}, {prop: "fmtUint64", name: "fmtUint64", pkg: "fmt", typ: $funcType([$Uint64, $Int32], [], false)}, {prop: "fmtFloat32", name: "fmtFloat32", pkg: "fmt", typ: $funcType([$Float32, $Int32], [], false)}, {prop: "fmtFloat64", name: "fmtFloat64", pkg: "fmt", typ: $funcType([$Float64, $Int32], [], false)}, {prop: "fmtComplex64", name: "fmtComplex64", pkg: "fmt", typ: $funcType([$Complex64, $Int32], [], false)}, {prop: "fmtComplex128", name: "fmtComplex128", pkg: "fmt", typ: $funcType([$Complex128, $Int32], [], false)}, {prop: "fmtString", name: "fmtString", pkg: "fmt", typ: $funcType([$String, $Int32], [], false)}, {prop: "fmtBytes", name: "fmtBytes", pkg: "fmt", typ: $funcType([sliceType, $Int32, reflect.Type, $Int], [], false)}, {prop: "fmtPointer", name: "fmtPointer", pkg: "fmt", typ: $funcType([reflect.Value, $Int32], [], false)}, {prop: "catchPanic", name: "catchPanic", pkg: "fmt", typ: $funcType([$emptyInterface, $Int32], [], false)}, {prop: "clearSpecialFlags", name: "clearSpecialFlags", pkg: "fmt", typ: $funcType([], [$Bool, $Bool], false)}, {prop: "restoreSpecialFlags", name: "restoreSpecialFlags", pkg: "fmt", typ: $funcType([$Bool, $Bool], [], false)}, {prop: "handleMethods", name: "handleMethods", pkg: "fmt", typ: $funcType([$Int32, $Int], [$Bool], false)}, {prop: "printArg", name: "printArg", pkg: "fmt", typ: $funcType([$emptyInterface, $Int32, $Int], [$Bool], false)}, {prop: "printValue", name: "printValue", pkg: "fmt", typ: $funcType([reflect.Value, $Int32, $Int], [$Bool], false)}, {prop: "printReflectValue", name: "printReflectValue", pkg: "fmt", typ: $funcType([reflect.Value, $Int32, $Int], [$Bool], false)}, {prop: "argNumber", name: "argNumber", pkg: "fmt", typ: $funcType([$Int, $String, $Int, $Int], [$Int, $Int, $Bool], false)}, {prop: "doPrintf", name: "doPrintf", pkg: "fmt", typ: $funcType([$String, sliceType$1], [], false)}, {prop: "doPrint", name: "doPrint", pkg: "fmt", typ: $funcType([sliceType$1, $Bool, $Bool], [], false)}];
	ptrType$5.methods = [{prop: "Read", name: "Read", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}, {prop: "ReadRune", name: "ReadRune", pkg: "", typ: $funcType([], [$Int32, $Int, $error], false)}, {prop: "Width", name: "Width", pkg: "", typ: $funcType([], [$Int, $Bool], false)}, {prop: "getRune", name: "getRune", pkg: "fmt", typ: $funcType([], [$Int32], false)}, {prop: "mustReadRune", name: "mustReadRune", pkg: "fmt", typ: $funcType([], [$Int32], false)}, {prop: "UnreadRune", name: "UnreadRune", pkg: "", typ: $funcType([], [$error], false)}, {prop: "error", name: "error", pkg: "fmt", typ: $funcType([$error], [], false)}, {prop: "errorString", name: "errorString", pkg: "fmt", typ: $funcType([$String], [], false)}, {prop: "Token", name: "Token", pkg: "", typ: $funcType([$Bool, funcType], [sliceType, $error], false)}, {prop: "SkipSpace", name: "SkipSpace", pkg: "", typ: $funcType([], [], false)}, {prop: "free", name: "free", pkg: "fmt", typ: $funcType([ssave], [], false)}, {prop: "skipSpace", name: "skipSpace", pkg: "fmt", typ: $funcType([$Bool], [], false)}, {prop: "token", name: "token", pkg: "fmt", typ: $funcType([$Bool, funcType], [sliceType], false)}, {prop: "consume", name: "consume", pkg: "fmt", typ: $funcType([$String, $Bool], [$Bool], false)}, {prop: "peek", name: "peek", pkg: "fmt", typ: $funcType([$String], [$Bool], false)}, {prop: "notEOF", name: "notEOF", pkg: "fmt", typ: $funcType([], [], false)}, {prop: "accept", name: "accept", pkg: "fmt", typ: $funcType([$String], [$Bool], false)}, {prop: "okVerb", name: "okVerb", pkg: "fmt", typ: $funcType([$Int32, $String, $String], [$Bool], false)}, {prop: "scanBool", name: "scanBool", pkg: "fmt", typ: $funcType([$Int32], [$Bool], false)}, {prop: "getBase", name: "getBase", pkg: "fmt", typ: $funcType([$Int32], [$Int, $String], false)}, {prop: "scanNumber", name: "scanNumber", pkg: "fmt", typ: $funcType([$String, $Bool], [$String], false)}, {prop: "scanRune", name: "scanRune", pkg: "fmt", typ: $funcType([$Int], [$Int64], false)}, {prop: "scanBasePrefix", name: "scanBasePrefix", pkg: "fmt", typ: $funcType([], [$Int, $String, $Bool], false)}, {prop: "scanInt", name: "scanInt", pkg: "fmt", typ: $funcType([$Int32, $Int], [$Int64], false)}, {prop: "scanUint", name: "scanUint", pkg: "fmt", typ: $funcType([$Int32, $Int], [$Uint64], false)}, {prop: "floatToken", name: "floatToken", pkg: "fmt", typ: $funcType([], [$String], false)}, {prop: "complexTokens", name: "complexTokens", pkg: "fmt", typ: $funcType([], [$String, $String], false)}, {prop: "convertFloat", name: "convertFloat", pkg: "fmt", typ: $funcType([$String, $Int], [$Float64], false)}, {prop: "scanComplex", name: "scanComplex", pkg: "fmt", typ: $funcType([$Int32, $Int], [$Complex128], false)}, {prop: "convertString", name: "convertString", pkg: "fmt", typ: $funcType([$Int32], [$String], false)}, {prop: "quotedString", name: "quotedString", pkg: "fmt", typ: $funcType([], [$String], false)}, {prop: "hexByte", name: "hexByte", pkg: "fmt", typ: $funcType([], [$Uint8, $Bool], false)}, {prop: "hexString", name: "hexString", pkg: "fmt", typ: $funcType([], [$String], false)}, {prop: "scanOne", name: "scanOne", pkg: "fmt", typ: $funcType([$Int32, $emptyInterface], [], false)}, {prop: "doScan", name: "doScan", pkg: "fmt", typ: $funcType([sliceType$1], [$Int, $error], false)}, {prop: "advance", name: "advance", pkg: "fmt", typ: $funcType([$String], [$Int], false)}, {prop: "doScanf", name: "doScanf", pkg: "fmt", typ: $funcType([$String, sliceType$1], [$Int, $error], false)}];
	fmtFlags.init([{prop: "widPresent", name: "widPresent", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "precPresent", name: "precPresent", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "minus", name: "minus", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "plus", name: "plus", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "sharp", name: "sharp", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "space", name: "space", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "unicode", name: "unicode", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "uniQuote", name: "uniQuote", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "zero", name: "zero", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "plusV", name: "plusV", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "sharpV", name: "sharpV", pkg: "fmt", typ: $Bool, tag: ""}]);
	fmt.init([{prop: "intbuf", name: "intbuf", pkg: "fmt", typ: arrayType$1, tag: ""}, {prop: "buf", name: "buf", pkg: "fmt", typ: ptrType$1, tag: ""}, {prop: "wid", name: "wid", pkg: "fmt", typ: $Int, tag: ""}, {prop: "prec", name: "prec", pkg: "fmt", typ: $Int, tag: ""}, {prop: "fmtFlags", name: "", pkg: "fmt", typ: fmtFlags, tag: ""}]);
	State.init([{prop: "Flag", name: "Flag", pkg: "", typ: $funcType([$Int], [$Bool], false)}, {prop: "Precision", name: "Precision", pkg: "", typ: $funcType([], [$Int, $Bool], false)}, {prop: "Width", name: "Width", pkg: "", typ: $funcType([], [$Int, $Bool], false)}, {prop: "Write", name: "Write", pkg: "", typ: $funcType([sliceType], [$Int, $error], false)}]);
	Formatter.init([{prop: "Format", name: "Format", pkg: "", typ: $funcType([State, $Int32], [], false)}]);
	Stringer.init([{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}]);
	GoStringer.init([{prop: "GoString", name: "GoString", pkg: "", typ: $funcType([], [$String], false)}]);
	buffer.init($Uint8);
	pp.init([{prop: "n", name: "n", pkg: "fmt", typ: $Int, tag: ""}, {prop: "panicking", name: "panicking", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "erroring", name: "erroring", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "buf", name: "buf", pkg: "fmt", typ: buffer, tag: ""}, {prop: "arg", name: "arg", pkg: "fmt", typ: $emptyInterface, tag: ""}, {prop: "value", name: "value", pkg: "fmt", typ: reflect.Value, tag: ""}, {prop: "reordered", name: "reordered", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "goodArgNum", name: "goodArgNum", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "runeBuf", name: "runeBuf", pkg: "fmt", typ: arrayType, tag: ""}, {prop: "fmt", name: "fmt", pkg: "fmt", typ: fmt, tag: ""}]);
	runeUnreader.init([{prop: "UnreadRune", name: "UnreadRune", pkg: "", typ: $funcType([], [$error], false)}]);
	scanError.init([{prop: "err", name: "err", pkg: "fmt", typ: $error, tag: ""}]);
	ss.init([{prop: "rr", name: "rr", pkg: "fmt", typ: io.RuneReader, tag: ""}, {prop: "buf", name: "buf", pkg: "fmt", typ: buffer, tag: ""}, {prop: "peekRune", name: "peekRune", pkg: "fmt", typ: $Int32, tag: ""}, {prop: "prevRune", name: "prevRune", pkg: "fmt", typ: $Int32, tag: ""}, {prop: "count", name: "count", pkg: "fmt", typ: $Int, tag: ""}, {prop: "atEOF", name: "atEOF", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "ssave", name: "", pkg: "fmt", typ: ssave, tag: ""}]);
	ssave.init([{prop: "validSave", name: "validSave", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "nlIsEnd", name: "nlIsEnd", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "nlIsSpace", name: "nlIsSpace", pkg: "fmt", typ: $Bool, tag: ""}, {prop: "argLimit", name: "argLimit", pkg: "fmt", typ: $Int, tag: ""}, {prop: "limit", name: "limit", pkg: "fmt", typ: $Int, tag: ""}, {prop: "maxWid", name: "maxWid", pkg: "fmt", typ: $Int, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = io.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = math.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = os.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = reflect.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = strconv.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sync.$init(); /* */ $s = 7; case 7: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = utf8.$init(); /* */ $s = 8; case 8: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		padZeroBytes = $makeSlice(sliceType, 65);
		padSpaceBytes = $makeSlice(sliceType, 65);
		trueBytes = new sliceType($stringToBytes("true"));
		falseBytes = new sliceType($stringToBytes("false"));
		commaSpaceBytes = new sliceType($stringToBytes(", "));
		nilAngleBytes = new sliceType($stringToBytes("<nil>"));
		nilParenBytes = new sliceType($stringToBytes("(nil)"));
		nilBytes = new sliceType($stringToBytes("nil"));
		mapBytes = new sliceType($stringToBytes("map["));
		percentBangBytes = new sliceType($stringToBytes("%!"));
		missingBytes = new sliceType($stringToBytes("(MISSING)"));
		badIndexBytes = new sliceType($stringToBytes("(BADINDEX)"));
		panicBytes = new sliceType($stringToBytes("(PANIC="));
		extraBytes = new sliceType($stringToBytes("%!(EXTRA "));
		irparenBytes = new sliceType($stringToBytes("i)"));
		bytesBytes = new sliceType($stringToBytes("[]byte{"));
		badWidthBytes = new sliceType($stringToBytes("%!(BADWIDTH)"));
		badPrecBytes = new sliceType($stringToBytes("%!(BADPREC)"));
		noVerbBytes = new sliceType($stringToBytes("%!(NOVERB)"));
		ppFree = new sync.Pool.ptr(0, 0, sliceType$1.nil, (function() {
			var $ptr;
			return new pp.ptr(0, false, false, buffer.nil, $ifaceNil, new reflect.Value.ptr(ptrType.nil, 0, 0), false, false, arrayType.zero(), new fmt.ptr(arrayType$1.zero(), ptrType$1.nil, 0, 0, new fmtFlags.ptr(false, false, false, false, false, false, false, false, false, false, false)));
		}));
		_r = reflect.TypeOf(new $Int(0)).Bits(); /* */ $s = 9; case 9: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		intBits = _r;
		_r$1 = reflect.TypeOf(new $Uintptr(0)).Bits(); /* */ $s = 10; case 10: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		uintptrBits = _r$1;
		byteType = reflect.TypeOf(new $Uint8(0));
		space = new sliceType$2([$toNativeArray($kindUint16, [9, 13]), $toNativeArray($kindUint16, [32, 32]), $toNativeArray($kindUint16, [133, 133]), $toNativeArray($kindUint16, [160, 160]), $toNativeArray($kindUint16, [5760, 5760]), $toNativeArray($kindUint16, [8192, 8202]), $toNativeArray($kindUint16, [8232, 8233]), $toNativeArray($kindUint16, [8239, 8239]), $toNativeArray($kindUint16, [8287, 8287]), $toNativeArray($kindUint16, [12288, 12288])]);
		ssFree = new sync.Pool.ptr(0, 0, sliceType$1.nil, (function() {
			var $ptr;
			return new ss.ptr($ifaceNil, buffer.nil, 0, 0, 0, false, new ssave.ptr(false, false, false, 0, 0, 0));
		}));
		complexError = errors.New("syntax error scanning complex number");
		boolError = errors.New("syntax error scanning boolean");
		init();
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/awalterschulze/ragax/golang/ast"] = (function() {
	var $pkg = {}, $init, strings, Defs, Def, Type, LazyExpr, Expr, sliceType, ptrType, ptrType$1, ptrType$2, funcType, NewExpr, NewLazy;
	strings = $packages["strings"];
	Defs = $pkg.Defs = $newType(12, $kindSlice, "ast.Defs", "Defs", "github.com/awalterschulze/ragax/golang/ast", null);
	Def = $pkg.Def = $newType(0, $kindStruct, "ast.Def", "Def", "github.com/awalterschulze/ragax/golang/ast", function(Name_, Expr_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Name = "";
			this.Expr = ptrType$2.nil;
			return;
		}
		this.Name = Name_;
		this.Expr = Expr_;
	});
	Type = $pkg.Type = $newType(4, $kindInt, "ast.Type", "Type", "github.com/awalterschulze/ragax/golang/ast", null);
	LazyExpr = $pkg.LazyExpr = $newType(0, $kindStruct, "ast.LazyExpr", "LazyExpr", "github.com/awalterschulze/ragax/golang/ast", function(f_, e_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.f = $throwNilPointerError;
			this.e = ptrType.nil;
			return;
		}
		this.f = f_;
		this.e = e_;
	});
	Expr = $pkg.Expr = $newType(0, $kindStruct, "ast.Expr", "Expr", "github.com/awalterschulze/ragax/golang/ast", function(Type_, Expr_, Expr2_, Value_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Type = 0;
			this.Expr = ptrType$2.nil;
			this.Expr2 = ptrType$2.nil;
			this.Value = "";
			return;
		}
		this.Type = Type_;
		this.Expr = Expr_;
		this.Expr2 = Expr2_;
		this.Value = Value_;
	});
	sliceType = $sliceType($String);
	ptrType = $ptrType(Expr);
	ptrType$1 = $ptrType(Def);
	ptrType$2 = $ptrType(LazyExpr);
	funcType = $funcType([], [ptrType], false);
	Defs.prototype.String = function() {
		var $ptr, _i, _ref, d, ds, i, ss;
		ds = this;
		ss = $makeSlice(sliceType, ds.$length);
		_ref = ds;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			d = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			((i < 0 || i >= ss.$length) ? $throwRuntimeError("index out of range") : ss.$array[ss.$offset + i] = d.String());
			_i++;
		}
		return strings.Join(ss, "\n");
	};
	$ptrType(Defs).prototype.String = function() { return this.$get().String(); };
	Def.ptr.prototype.String = function() {
		var $ptr, d;
		d = this;
		return "#" + d.Name + " = " + d.Expr.String();
	};
	Def.prototype.String = function() { return this.$val.String(); };
	LazyExpr.ptr.prototype.IsLazy = function() {
		var $ptr, l;
		l = this;
		return l.e === ptrType.nil;
	};
	LazyExpr.prototype.IsLazy = function() { return this.$val.IsLazy(); };
	LazyExpr.ptr.prototype.Expr = function() {
		var $ptr, _r, e, l, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; e = $f.e; l = $f.l; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		l = this;
		if (!(l.e === ptrType.nil)) {
			return l.e;
		}
		_r = l.f(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		e = _r;
		l.e = e;
		return e;
		/* */ } return; } if ($f === undefined) { $f = { $blk: LazyExpr.ptr.prototype.Expr }; } $f.$ptr = $ptr; $f._r = _r; $f.e = e; $f.l = l; $f.$s = $s; $f.$r = $r; return $f;
	};
	LazyExpr.prototype.Expr = function() { return this.$val.Expr(); };
	LazyExpr.ptr.prototype.String = function() {
		var $ptr, l;
		l = this;
		if (!(l.e === ptrType.nil)) {
			return l.e.String();
		}
		return "lazy";
	};
	LazyExpr.prototype.String = function() { return this.$val.String(); };
	NewExpr = function(e) {
		var $ptr, e;
		return new LazyExpr.ptr($throwNilPointerError, e);
	};
	$pkg.NewExpr = NewExpr;
	NewLazy = function(f) {
		var $ptr, f;
		return new LazyExpr.ptr(f, ptrType.nil);
	};
	$pkg.NewLazy = NewLazy;
	Expr.ptr.prototype.String = function() {
		var $ptr, _1, e;
		e = this;
		_1 = e.Type;
		if (_1 === ($pkg.Value)) {
			return e.Value;
		} else if (_1 === ($pkg.Or)) {
			return "( " + e.Expr.String() + " | " + e.Expr2.String() + " )";
		} else if (_1 === ($pkg.Concat)) {
			return "[" + e.Expr.String() + ", " + e.Expr2.String() + "]";
		} else if (_1 === ($pkg.ZeroOrMore)) {
			return "(" + e.Expr.String() + ")*";
		} else if (_1 === ($pkg.Reference)) {
			return "@" + e.Value;
		}
		$panic(new $String("unreachable"));
	};
	Expr.prototype.String = function() { return this.$val.String(); };
	Defs.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$1.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$2.methods = [{prop: "IsLazy", name: "IsLazy", pkg: "", typ: $funcType([], [$Bool], false)}, {prop: "Expr", name: "Expr", pkg: "", typ: $funcType([], [ptrType], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	Defs.init(ptrType$1);
	Def.init([{prop: "Name", name: "Name", pkg: "", typ: $String, tag: ""}, {prop: "Expr", name: "Expr", pkg: "", typ: ptrType$2, tag: ""}]);
	LazyExpr.init([{prop: "f", name: "f", pkg: "github.com/awalterschulze/ragax/golang/ast", typ: funcType, tag: ""}, {prop: "e", name: "e", pkg: "github.com/awalterschulze/ragax/golang/ast", typ: ptrType, tag: ""}]);
	Expr.init([{prop: "Type", name: "Type", pkg: "", typ: Type, tag: ""}, {prop: "Expr", name: "Expr", pkg: "", typ: ptrType$2, tag: ""}, {prop: "Expr2", name: "Expr2", pkg: "", typ: ptrType$2, tag: ""}, {prop: "Value", name: "Value", pkg: "", typ: $String, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = strings.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.Or = 1;
		$pkg.Concat = 2;
		$pkg.ZeroOrMore = 3;
		$pkg.Reference = 4;
		$pkg.Value = 5;
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/awalterschulze/ragax/golang/token"] = (function() {
	var $pkg = {}, $init, fmt, Token, Type, Pos, TokenMap, sliceType, sliceType$1, sliceType$2, ptrType, mapType;
	fmt = $packages["fmt"];
	Token = $pkg.Token = $newType(0, $kindStruct, "token.Token", "Token", "github.com/awalterschulze/ragax/golang/token", function(Type_, Lit_, Pos_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Type = 0;
			this.Lit = sliceType$2.nil;
			this.Pos = new Pos.ptr(0, 0, 0);
			return;
		}
		this.Type = Type_;
		this.Lit = Lit_;
		this.Pos = Pos_;
	});
	Type = $pkg.Type = $newType(4, $kindInt, "token.Type", "Type", "github.com/awalterschulze/ragax/golang/token", null);
	Pos = $pkg.Pos = $newType(0, $kindStruct, "token.Pos", "Pos", "github.com/awalterschulze/ragax/golang/token", function(Offset_, Line_, Column_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Offset = 0;
			this.Line = 0;
			this.Column = 0;
			return;
		}
		this.Offset = Offset_;
		this.Line = Line_;
		this.Column = Column_;
	});
	TokenMap = $pkg.TokenMap = $newType(0, $kindStruct, "token.TokenMap", "TokenMap", "github.com/awalterschulze/ragax/golang/token", function(typeMap_, idMap_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.typeMap = sliceType.nil;
			this.idMap = false;
			return;
		}
		this.typeMap = typeMap_;
		this.idMap = idMap_;
	});
	sliceType = $sliceType($String);
	sliceType$1 = $sliceType($emptyInterface);
	sliceType$2 = $sliceType($Uint8);
	ptrType = $ptrType(Token);
	mapType = $mapType($String, Type);
	Pos.ptr.prototype.String = function() {
		var $ptr, _r, this$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; this$1 = $f.this$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = $clone(this, Pos);
		_r = fmt.Sprintf("Pos(offset=%d, line=%d, column=%d)", new sliceType$1([new $Int(this$1.Offset), new $Int(this$1.Line), new $Int(this$1.Column)])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Pos.ptr.prototype.String }; } $f.$ptr = $ptr; $f._r = _r; $f.this$1 = this$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	Pos.prototype.String = function() { return this.$val.String(); };
	TokenMap.ptr.prototype.Id = function(tok) {
		var $ptr, this$1, tok, x;
		this$1 = $clone(this, TokenMap);
		if ((tok >> 0) < this$1.typeMap.$length) {
			return (x = this$1.typeMap, ((tok < 0 || tok >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + tok]));
		}
		return "unknown";
	};
	TokenMap.prototype.Id = function(tok) { return this.$val.Id(tok); };
	TokenMap.ptr.prototype.Type = function(tok) {
		var $ptr, _entry, _tuple, exist, this$1, tok, typ;
		this$1 = $clone(this, TokenMap);
		_tuple = (_entry = this$1.idMap[$String.keyFor(tok)], _entry !== undefined ? [_entry.v, true] : [0, false]);
		typ = _tuple[0];
		exist = _tuple[1];
		if (exist) {
			return typ;
		}
		return 0;
	};
	TokenMap.prototype.Type = function(tok) { return this.$val.Type(tok); };
	TokenMap.ptr.prototype.TokenString = function(tok) {
		var $ptr, _r, this$1, tok, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; this$1 = $f.this$1; tok = $f.tok; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = $clone(this, TokenMap);
		_r = fmt.Sprintf("%s(%d,%s)", new sliceType$1([new $String(this$1.Id(tok.Type)), new Type(tok.Type), tok.Lit])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: TokenMap.ptr.prototype.TokenString }; } $f.$ptr = $ptr; $f._r = _r; $f.this$1 = this$1; $f.tok = tok; $f.$s = $s; $f.$r = $r; return $f;
	};
	TokenMap.prototype.TokenString = function(tok) { return this.$val.TokenString(tok); };
	TokenMap.ptr.prototype.StringType = function(typ) {
		var $ptr, _r, this$1, typ, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; this$1 = $f.this$1; typ = $f.typ; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = $clone(this, TokenMap);
		_r = fmt.Sprintf("%s(%d)", new sliceType$1([new $String(this$1.Id(typ)), new Type(typ)])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: TokenMap.ptr.prototype.StringType }; } $f.$ptr = $ptr; $f._r = _r; $f.this$1 = this$1; $f.typ = typ; $f.$s = $s; $f.$r = $r; return $f;
	};
	TokenMap.prototype.StringType = function(typ) { return this.$val.StringType(typ); };
	Pos.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	TokenMap.methods = [{prop: "Id", name: "Id", pkg: "", typ: $funcType([Type], [$String], false)}, {prop: "Type", name: "Type", pkg: "", typ: $funcType([$String], [Type], false)}, {prop: "TokenString", name: "TokenString", pkg: "", typ: $funcType([ptrType], [$String], false)}, {prop: "StringType", name: "StringType", pkg: "", typ: $funcType([Type], [$String], false)}];
	Token.init([{prop: "Type", name: "", pkg: "", typ: Type, tag: ""}, {prop: "Lit", name: "Lit", pkg: "", typ: sliceType$2, tag: ""}, {prop: "Pos", name: "", pkg: "", typ: Pos, tag: ""}]);
	Pos.init([{prop: "Offset", name: "Offset", pkg: "", typ: $Int, tag: ""}, {prop: "Line", name: "Line", pkg: "", typ: $Int, tag: ""}, {prop: "Column", name: "Column", pkg: "", typ: $Int, tag: ""}]);
	TokenMap.init([{prop: "typeMap", name: "typeMap", pkg: "github.com/awalterschulze/ragax/golang/token", typ: sliceType, tag: ""}, {prop: "idMap", name: "idMap", pkg: "github.com/awalterschulze/ragax/golang/token", typ: mapType, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = fmt.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.TokMap = new TokenMap.ptr(new sliceType(["INVALID", "$", "#", "id", "=", "(", "|", ")", "[", ",", "]", "*", "@"]), $makeMap($String.keyFor, [{ k: "INVALID", v: 0 }, { k: "$", v: 1 }, { k: "#", v: 2 }, { k: "id", v: 3 }, { k: "=", v: 4 }, { k: "(", v: 5 }, { k: "|", v: 6 }, { k: ")", v: 7 }, { k: "[", v: 8 }, { k: ",", v: 9 }, { k: "]", v: 10 }, { k: "*", v: 11 }, { k: "@", v: 12 }]));
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["sort"] = (function() {
	var $pkg = {}, $init;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["path/filepath"] = (function() {
	var $pkg = {}, $init, errors, os, runtime, sort, strings, utf8;
	errors = $packages["errors"];
	os = $packages["os"];
	runtime = $packages["runtime"];
	sort = $packages["sort"];
	strings = $packages["strings"];
	utf8 = $packages["unicode/utf8"];
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = errors.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = os.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = runtime.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sort.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = strings.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = utf8.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.ErrBadPattern = errors.New("syntax error in pattern");
		$pkg.SkipDir = errors.New("skip this directory");
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["io/ioutil"] = (function() {
	var $pkg = {}, $init, bytes, io, os, filepath, sort, strconv, sync, time, sliceType, sliceType$1, ptrType, blackHolePool;
	bytes = $packages["bytes"];
	io = $packages["io"];
	os = $packages["os"];
	filepath = $packages["path/filepath"];
	sort = $packages["sort"];
	strconv = $packages["strconv"];
	sync = $packages["sync"];
	time = $packages["time"];
	sliceType = $sliceType($emptyInterface);
	sliceType$1 = $sliceType($Uint8);
	ptrType = $ptrType(sliceType$1);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = bytes.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = io.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = os.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = filepath.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sort.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = strconv.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = sync.$init(); /* */ $s = 7; case 7: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = time.$init(); /* */ $s = 8; case 8: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		blackHolePool = new sync.Pool.ptr(0, 0, sliceType.nil, (function() {
			var $ptr, b, b$24ptr;
			b = $makeSlice(sliceType$1, 8192);
			return (b$24ptr || (b$24ptr = new ptrType(function() { return b; }, function($v) { b = $subslice(new sliceType$1($v.$array), $v.$offset, $v.$offset + $v.$length); })));
		}));
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/awalterschulze/ragax/golang/lexer"] = (function() {
	var $pkg = {}, $init, fmt, token, ioutil, utf8, ActionRow, Lexer, sliceType, sliceType$1, ptrType, ptrType$1, NewLexer;
	fmt = $packages["fmt"];
	token = $packages["github.com/awalterschulze/ragax/golang/token"];
	ioutil = $packages["io/ioutil"];
	utf8 = $packages["unicode/utf8"];
	ActionRow = $pkg.ActionRow = $newType(0, $kindStruct, "lexer.ActionRow", "ActionRow", "github.com/awalterschulze/ragax/golang/lexer", function(Accept_, Ignore_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Accept = 0;
			this.Ignore = "";
			return;
		}
		this.Accept = Accept_;
		this.Ignore = Ignore_;
	});
	Lexer = $pkg.Lexer = $newType(0, $kindStruct, "lexer.Lexer", "Lexer", "github.com/awalterschulze/ragax/golang/lexer", function(src_, pos_, line_, column_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.src = sliceType$1.nil;
			this.pos = 0;
			this.line = 0;
			this.column = 0;
			return;
		}
		this.src = src_;
		this.pos = pos_;
		this.line = line_;
		this.column = column_;
	});
	sliceType = $sliceType($emptyInterface);
	sliceType$1 = $sliceType($Uint8);
	ptrType = $ptrType(Lexer);
	ptrType$1 = $ptrType(token.Token);
	ActionRow.ptr.prototype.String = function() {
		var $ptr, _r, this$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; this$1 = $f.this$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = $clone(this, ActionRow);
		_r = fmt.Sprintf("Accept=%d, Ignore=%s", new sliceType([new token.Type(this$1.Accept), new $String(this$1.Ignore)])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: ActionRow.ptr.prototype.String }; } $f.$ptr = $ptr; $f._r = _r; $f.this$1 = this$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	ActionRow.prototype.String = function() { return this.$val.String(); };
	NewLexer = function(src) {
		var $ptr, lexer, src;
		lexer = new Lexer.ptr(src, 0, 1, 1);
		return lexer;
	};
	$pkg.NewLexer = NewLexer;
	Lexer.ptr.prototype.Scan = function() {
		var $ptr, _1, _r, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tmp$6, _tmp$7, _tuple, end, rune1, size, start, state, this$1, tok, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _1 = $f._1; _r = $f._r; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tmp$4 = $f._tmp$4; _tmp$5 = $f._tmp$5; _tmp$6 = $f._tmp$6; _tmp$7 = $f._tmp$7; _tuple = $f._tuple; end = $f.end; rune1 = $f.rune1; size = $f.size; start = $f.start; state = $f.state; this$1 = $f.this$1; tok = $f.tok; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		tok = ptrType$1.nil;
		this$1 = this;
		tok = new token.Token.ptr(0, sliceType$1.nil, new token.Pos.ptr(0, 0, 0));
		if (this$1.pos >= this$1.src.$length) {
			tok.Type = 1;
			_tmp = this$1.pos;
			_tmp$1 = this$1.line;
			_tmp$2 = this$1.column;
			tok.Pos.Offset = _tmp;
			tok.Pos.Line = _tmp$1;
			tok.Pos.Column = _tmp$2;
			return tok;
		}
		_tmp$3 = this$1.pos;
		_tmp$4 = 0;
		start = _tmp$3;
		end = _tmp$4;
		tok.Type = 0;
		_tmp$5 = 0;
		_tmp$6 = -1;
		_tmp$7 = 0;
		state = _tmp$5;
		rune1 = _tmp$6;
		size = _tmp$7;
		/* while (true) { */ case 1:
			/* if (!(!((state === -1)))) { break; } */ if(!(!((state === -1)))) { $s = 2; continue; }
			if (this$1.pos >= this$1.src.$length) {
				rune1 = -1;
			} else {
				_tuple = utf8.DecodeRune($subslice(this$1.src, this$1.pos));
				rune1 = _tuple[0];
				size = _tuple[1];
				this$1.pos = this$1.pos + (size) >> 0;
			}
			_1 = rune1;
			if (_1 === (10)) {
				this$1.line = this$1.line + (1) >> 0;
				this$1.column = 1;
			} else if (_1 === (13)) {
				this$1.column = 1;
			} else if (_1 === (9)) {
				this$1.column = this$1.column + (4) >> 0;
			} else {
				this$1.column = this$1.column + (1) >> 0;
			}
			/* */ if (!((rune1 === -1))) { $s = 3; continue; }
			/* */ $s = 4; continue;
			/* if (!((rune1 === -1))) { */ case 3:
				_r = ((state < 0 || state >= $pkg.TransTab.length) ? $throwRuntimeError("index out of range") : $pkg.TransTab[state])(rune1); /* */ $s = 6; case 6: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				state = _r;
				$s = 5; continue;
			/* } else { */ case 4:
				state = -1;
			/* } */ case 5:
			if (!((state === -1))) {
				if (!((((state < 0 || state >= $pkg.ActTab.length) ? $throwRuntimeError("index out of range") : $pkg.ActTab[state]).Accept === -1))) {
					tok.Type = ((state < 0 || state >= $pkg.ActTab.length) ? $throwRuntimeError("index out of range") : $pkg.ActTab[state]).Accept;
					end = this$1.pos;
				} else if (!(((state < 0 || state >= $pkg.ActTab.length) ? $throwRuntimeError("index out of range") : $pkg.ActTab[state]).Ignore === "")) {
					start = this$1.pos;
					state = 0;
					if (start >= this$1.src.$length) {
						tok.Type = 1;
					}
				}
			} else {
				if (tok.Type === 0) {
					end = this$1.pos;
				}
			}
		/* } */ $s = 1; continue; case 2:
		if (end > start) {
			this$1.pos = end;
			tok.Lit = $subslice(this$1.src, start, end);
		} else {
			tok.Lit = new sliceType$1([]);
		}
		tok.Pos.Offset = start;
		tok.Pos.Column = this$1.column;
		tok.Pos.Line = this$1.line;
		return tok;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Lexer.ptr.prototype.Scan }; } $f.$ptr = $ptr; $f._1 = _1; $f._r = _r; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tmp$4 = _tmp$4; $f._tmp$5 = _tmp$5; $f._tmp$6 = _tmp$6; $f._tmp$7 = _tmp$7; $f._tuple = _tuple; $f.end = end; $f.rune1 = rune1; $f.size = size; $f.start = start; $f.state = state; $f.this$1 = this$1; $f.tok = tok; $f.$s = $s; $f.$r = $r; return $f;
	};
	Lexer.prototype.Scan = function() { return this.$val.Scan(); };
	Lexer.ptr.prototype.Reset = function() {
		var $ptr, this$1;
		this$1 = this;
		this$1.pos = 0;
	};
	Lexer.prototype.Reset = function() { return this.$val.Reset(); };
	ActionRow.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType.methods = [{prop: "Scan", name: "Scan", pkg: "", typ: $funcType([], [ptrType$1], false)}, {prop: "Reset", name: "Reset", pkg: "", typ: $funcType([], [], false)}];
	ActionRow.init([{prop: "Accept", name: "Accept", pkg: "", typ: token.Type, tag: ""}, {prop: "Ignore", name: "Ignore", pkg: "", typ: $String, tag: ""}]);
	Lexer.init([{prop: "src", name: "src", pkg: "github.com/awalterschulze/ragax/golang/lexer", typ: sliceType$1, tag: ""}, {prop: "pos", name: "pos", pkg: "github.com/awalterschulze/ragax/golang/lexer", typ: $Int, tag: ""}, {prop: "line", name: "line", pkg: "github.com/awalterschulze/ragax/golang/lexer", typ: $Int, tag: ""}, {prop: "column", name: "column", pkg: "github.com/awalterschulze/ragax/golang/lexer", typ: $Int, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = fmt.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = token.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = ioutil.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = utf8.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$pkg.ActTab = $toNativeArray($kindStruct, [new ActionRow.ptr(0, ""), new ActionRow.ptr(-1, "!space"), new ActionRow.ptr(2, ""), new ActionRow.ptr(5, ""), new ActionRow.ptr(7, ""), new ActionRow.ptr(11, ""), new ActionRow.ptr(9, ""), new ActionRow.ptr(0, ""), new ActionRow.ptr(4, ""), new ActionRow.ptr(12, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(8, ""), new ActionRow.ptr(10, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(6, ""), new ActionRow.ptr(0, ""), new ActionRow.ptr(0, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(3, ""), new ActionRow.ptr(0, ""), new ActionRow.ptr(-1, "!space"), new ActionRow.ptr(-1, "!space")]);
		$pkg.TransTab = $toNativeArray($kindFunc, [(function(r) {
			var $ptr, r;
			if ((r === 9)) {
				return 1;
			} else if ((r === 10)) {
				return 1;
			} else if ((r === 13)) {
				return 1;
			} else if ((r === 32)) {
				return 1;
			} else if ((r === 35)) {
				return 2;
			} else if ((r === 40)) {
				return 3;
			} else if ((r === 41)) {
				return 4;
			} else if ((r === 42)) {
				return 5;
			} else if ((r === 44)) {
				return 6;
			} else if ((r === 47)) {
				return 7;
			} else if ((r === 61)) {
				return 8;
			} else if ((r === 64)) {
				return 9;
			} else if (65 <= r && r <= 90) {
				return 10;
			} else if ((r === 91)) {
				return 11;
			} else if ((r === 93)) {
				return 12;
			} else if ((r === 95)) {
				return 13;
			} else if (97 <= r && r <= 122) {
				return 14;
			} else if ((r === 124)) {
				return 15;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			if ((r === 9)) {
				return 1;
			} else if ((r === 10)) {
				return 1;
			} else if ((r === 13)) {
				return 1;
			} else if ((r === 32)) {
				return 1;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			if ((r === 42)) {
				return 16;
			} else if ((r === 47)) {
				return 17;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			if ((r === 42)) {
				return 22;
			} else {
				return 16;
			}
		}), (function(r) {
			var $ptr, r;
			if ((r === 10)) {
				return 23;
			} else {
				return 17;
			}
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			if (48 <= r && r <= 57) {
				return 18;
			} else if (65 <= r && r <= 90) {
				return 19;
			} else if ((r === 95)) {
				return 20;
			} else if (97 <= r && r <= 122) {
				return 21;
			}
			return -1;
		}), (function(r) {
			var $ptr, r;
			if ((r === 42)) {
				return 22;
			} else if ((r === 47)) {
				return 24;
			} else {
				return 16;
			}
		}), (function(r) {
			var $ptr, r;
			return -1;
		}), (function(r) {
			var $ptr, r;
			return -1;
		})]);
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/awalterschulze/ragax/golang/errors"] = (function() {
	var $pkg = {}, $init, bytes, fmt, token, ErrorSymbol, Error, sliceType, arrayType, arrayType$1, sliceType$1, ptrType, sliceType$2, sliceType$3, ptrType$1;
	bytes = $packages["bytes"];
	fmt = $packages["fmt"];
	token = $packages["github.com/awalterschulze/ragax/golang/token"];
	ErrorSymbol = $pkg.ErrorSymbol = $newType(8, $kindInterface, "errors.ErrorSymbol", "ErrorSymbol", "github.com/awalterschulze/ragax/golang/errors", null);
	Error = $pkg.Error = $newType(0, $kindStruct, "errors.Error", "Error", "github.com/awalterschulze/ragax/golang/errors", function(Err_, ErrorToken_, ErrorSymbols_, ExpectedTokens_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.Err = $ifaceNil;
			this.ErrorToken = ptrType.nil;
			this.ErrorSymbols = sliceType$2.nil;
			this.ExpectedTokens = sliceType$3.nil;
			return;
		}
		this.Err = Err_;
		this.ErrorToken = ErrorToken_;
		this.ErrorSymbols = ErrorSymbols_;
		this.ExpectedTokens = ExpectedTokens_;
	});
	sliceType = $sliceType($Uint8);
	arrayType = $arrayType($Uint8, 4);
	arrayType$1 = $arrayType($Uint8, 64);
	sliceType$1 = $sliceType($emptyInterface);
	ptrType = $ptrType(token.Token);
	sliceType$2 = $sliceType(ErrorSymbol);
	sliceType$3 = $sliceType($String);
	ptrType$1 = $ptrType(Error);
	Error.ptr.prototype.String = function() {
		var $ptr, E, _i, _i$1, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, _r$8, _ref, _ref$1, sym, sym$1, w, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; E = $f.E; _i = $f._i; _i$1 = $f._i$1; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _r$8 = $f._r$8; _ref = $f._ref; _ref$1 = $f._ref$1; sym = $f.sym; sym$1 = $f.sym$1; w = $f.w; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		E = this;
		w = new bytes.Buffer.ptr(sliceType.nil, 0, arrayType.zero(), arrayType$1.zero(), 0);
		_r = fmt.Fprintf(w, "Error", new sliceType$1([])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r;
		/* */ if (!($interfaceIsEqual(E.Err, $ifaceNil))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!($interfaceIsEqual(E.Err, $ifaceNil))) { */ case 2:
			_r$1 = fmt.Fprintf(w, " %s\n", new sliceType$1([E.Err])); /* */ $s = 5; case 5: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			_r$1;
			$s = 4; continue;
		/* } else { */ case 3:
			_r$2 = fmt.Fprintf(w, "\n", new sliceType$1([])); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			_r$2;
		/* } */ case 4:
		_r$3 = fmt.Fprintf(w, "Token: type=%d, lit=%s\n", new sliceType$1([new token.Type(E.ErrorToken.Type), E.ErrorToken.Lit])); /* */ $s = 7; case 7: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		_r$3;
		_r$4 = fmt.Fprintf(w, "Pos: offset=%d, line=%d, column=%d\n", new sliceType$1([new $Int(E.ErrorToken.Pos.Offset), new $Int(E.ErrorToken.Pos.Line), new $Int(E.ErrorToken.Pos.Column)])); /* */ $s = 8; case 8: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
		_r$4;
		_r$5 = fmt.Fprintf(w, "Expected one of: ", new sliceType$1([])); /* */ $s = 9; case 9: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
		_r$5;
		_ref = E.ExpectedTokens;
		_i = 0;
		/* while (true) { */ case 10:
			/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 11; continue; }
			sym = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			_r$6 = fmt.Fprintf(w, "%s ", new sliceType$1([new $String(sym)])); /* */ $s = 12; case 12: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
			_r$6;
			_i++;
		/* } */ $s = 10; continue; case 11:
		_r$7 = fmt.Fprintf(w, "ErrorSymbol:\n", new sliceType$1([])); /* */ $s = 13; case 13: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
		_r$7;
		_ref$1 = E.ErrorSymbols;
		_i$1 = 0;
		/* while (true) { */ case 14:
			/* if (!(_i$1 < _ref$1.$length)) { break; } */ if(!(_i$1 < _ref$1.$length)) { $s = 15; continue; }
			sym$1 = ((_i$1 < 0 || _i$1 >= _ref$1.$length) ? $throwRuntimeError("index out of range") : _ref$1.$array[_ref$1.$offset + _i$1]);
			_r$8 = fmt.Fprintf(w, "%v\n", new sliceType$1([sym$1])); /* */ $s = 16; case 16: if($c) { $c = false; _r$8 = _r$8.$blk(); } if (_r$8 && _r$8.$blk !== undefined) { break s; }
			_r$8;
			_i$1++;
		/* } */ $s = 14; continue; case 15:
		return w.String();
		/* */ } return; } if ($f === undefined) { $f = { $blk: Error.ptr.prototype.String }; } $f.$ptr = $ptr; $f.E = E; $f._i = _i; $f._i$1 = _i$1; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._r$8 = _r$8; $f._ref = _ref; $f._ref$1 = _ref$1; $f.sym = sym; $f.sym$1 = sym$1; $f.w = w; $f.$s = $s; $f.$r = $r; return $f;
	};
	Error.prototype.String = function() { return this.$val.String(); };
	ptrType$1.methods = [{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ErrorSymbol.init([]);
	Error.init([{prop: "Err", name: "Err", pkg: "", typ: $error, tag: ""}, {prop: "ErrorToken", name: "ErrorToken", pkg: "", typ: ptrType, tag: ""}, {prop: "ErrorSymbols", name: "ErrorSymbols", pkg: "", typ: sliceType$2, tag: ""}, {prop: "ExpectedTokens", name: "ExpectedTokens", pkg: "", typ: sliceType$3, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = bytes.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = fmt.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = token.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/awalterschulze/ragax/golang/parser"] = (function() {
	var $pkg = {}, $init, bytes, errors, fmt, ast, errors$1, token, action, accept, shift, reduce, actionRow, stack, Parser, Scanner, ProdTabEntry, Attrib, arrayType, arrayType$1, ptrType, ptrType$1, ptrType$2, sliceType, sliceType$1, sliceType$2, sliceType$3, arrayType$2, arrayType$3, ptrType$3, ptrType$4, ptrType$5, sliceType$4, sliceType$5, ptrType$6, funcType, actionTab, gotoTab, productionsTable, newStack, NewParser, newString;
	bytes = $packages["bytes"];
	errors = $packages["errors"];
	fmt = $packages["fmt"];
	ast = $packages["github.com/awalterschulze/ragax/golang/ast"];
	errors$1 = $packages["github.com/awalterschulze/ragax/golang/errors"];
	token = $packages["github.com/awalterschulze/ragax/golang/token"];
	action = $pkg.action = $newType(8, $kindInterface, "parser.action", "action", "github.com/awalterschulze/ragax/golang/parser", null);
	accept = $pkg.accept = $newType(1, $kindBool, "parser.accept", "accept", "github.com/awalterschulze/ragax/golang/parser", null);
	shift = $pkg.shift = $newType(4, $kindInt, "parser.shift", "shift", "github.com/awalterschulze/ragax/golang/parser", null);
	reduce = $pkg.reduce = $newType(4, $kindInt, "parser.reduce", "reduce", "github.com/awalterschulze/ragax/golang/parser", null);
	actionRow = $pkg.actionRow = $newType(0, $kindStruct, "parser.actionRow", "actionRow", "github.com/awalterschulze/ragax/golang/parser", function(canRecover_, actions_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.canRecover = false;
			this.actions = arrayType$1.zero();
			return;
		}
		this.canRecover = canRecover_;
		this.actions = actions_;
	});
	stack = $pkg.stack = $newType(0, $kindStruct, "parser.stack", "stack", "github.com/awalterschulze/ragax/golang/parser", function(state_, attrib_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.state = sliceType$1.nil;
			this.attrib = sliceType$2.nil;
			return;
		}
		this.state = state_;
		this.attrib = attrib_;
	});
	Parser = $pkg.Parser = $newType(0, $kindStruct, "parser.Parser", "Parser", "github.com/awalterschulze/ragax/golang/parser", function(stack_, nextToken_, pos_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.stack = ptrType$3.nil;
			this.nextToken = ptrType$4.nil;
			this.pos = 0;
			return;
		}
		this.stack = stack_;
		this.nextToken = nextToken_;
		this.pos = pos_;
	});
	Scanner = $pkg.Scanner = $newType(8, $kindInterface, "parser.Scanner", "Scanner", "github.com/awalterschulze/ragax/golang/parser", null);
	ProdTabEntry = $pkg.ProdTabEntry = $newType(0, $kindStruct, "parser.ProdTabEntry", "ProdTabEntry", "github.com/awalterschulze/ragax/golang/parser", function(String_, Id_, NTType_, Index_, NumSymbols_, ReduceFunc_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.String = "";
			this.Id = "";
			this.NTType = 0;
			this.Index = 0;
			this.NumSymbols = 0;
			this.ReduceFunc = $throwNilPointerError;
			return;
		}
		this.String = String_;
		this.Id = Id_;
		this.NTType = NTType_;
		this.Index = Index_;
		this.NumSymbols = NumSymbols_;
		this.ReduceFunc = ReduceFunc_;
	});
	Attrib = $pkg.Attrib = $newType(8, $kindInterface, "parser.Attrib", "Attrib", "github.com/awalterschulze/ragax/golang/parser", null);
	arrayType = $arrayType($Int, 4);
	arrayType$1 = $arrayType(action, 17);
	ptrType = $ptrType(ast.Def);
	ptrType$1 = $ptrType(ast.LazyExpr);
	ptrType$2 = $ptrType(ast.Expr);
	sliceType = $sliceType($emptyInterface);
	sliceType$1 = $sliceType($Int);
	sliceType$2 = $sliceType(Attrib);
	sliceType$3 = $sliceType($Uint8);
	arrayType$2 = $arrayType($Uint8, 4);
	arrayType$3 = $arrayType($Uint8, 64);
	ptrType$3 = $ptrType(stack);
	ptrType$4 = $ptrType(token.Token);
	ptrType$5 = $ptrType(errors$1.Error);
	sliceType$4 = $sliceType(errors$1.ErrorSymbol);
	sliceType$5 = $sliceType($String);
	ptrType$6 = $ptrType(Parser);
	funcType = $funcType([sliceType$2], [Attrib, $error], false);
	accept.prototype.Equal = function(that) {
		var $ptr, _tuple, ok, that, this$1;
		this$1 = this.$val;
		_tuple = $assertType(that, accept, true);
		ok = _tuple[1];
		if (ok) {
			return true;
		}
		return false;
	};
	$ptrType(accept).prototype.Equal = function(that) { return new accept(this.$get()).Equal(that); };
	reduce.prototype.Equal = function(that) {
		var $ptr, _tuple, ok, that, that1, this$1;
		this$1 = this.$val;
		_tuple = $assertType(that, reduce, true);
		that1 = _tuple[0];
		ok = _tuple[1];
		if (!ok) {
			return false;
		}
		return this$1 === that1;
	};
	$ptrType(reduce).prototype.Equal = function(that) { return new reduce(this.$get()).Equal(that); };
	shift.prototype.Equal = function(that) {
		var $ptr, _tuple, ok, that, that1, this$1;
		this$1 = this.$val;
		_tuple = $assertType(that, shift, true);
		that1 = _tuple[0];
		ok = _tuple[1];
		if (!ok) {
			return false;
		}
		return this$1 === that1;
	};
	$ptrType(shift).prototype.Equal = function(that) { return new shift(this.$get()).Equal(that); };
	accept.prototype.String = function() {
		var $ptr, this$1;
		this$1 = this.$val;
		return "accept(0)";
	};
	$ptrType(accept).prototype.String = function() { return new accept(this.$get()).String(); };
	shift.prototype.String = function() {
		var $ptr, _r, this$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; this$1 = $f.this$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = this.$val;
		_r = fmt.Sprintf("shift:%d", new sliceType([new shift(this$1)])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: shift.prototype.String }; } $f.$ptr = $ptr; $f._r = _r; $f.this$1 = this$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	$ptrType(shift).prototype.String = function() { return new shift(this.$get()).String(); };
	reduce.prototype.String = function() {
		var $ptr, _r, this$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; this$1 = $f.this$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = this.$val;
		_r = fmt.Sprintf("reduce:%d(%s)", new sliceType([new reduce(this$1), new $String(((this$1 < 0 || this$1 >= productionsTable.length) ? $throwRuntimeError("index out of range") : productionsTable[this$1]).String)])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: reduce.prototype.String }; } $f.$ptr = $ptr; $f._r = _r; $f.this$1 = this$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	$ptrType(reduce).prototype.String = function() { return new reduce(this.$get()).String(); };
	newStack = function() {
		var $ptr;
		return new stack.ptr($makeSlice(sliceType$1, 0, 100), $makeSlice(sliceType$2, 0, 100));
	};
	stack.ptr.prototype.reset = function() {
		var $ptr, this$1;
		this$1 = this;
		this$1.state = $subslice(this$1.state, 0, 0);
		this$1.attrib = $subslice(this$1.attrib, 0, 0);
	};
	stack.prototype.reset = function() { return this.$val.reset(); };
	stack.ptr.prototype.push = function(s, a) {
		var $ptr, a, s, this$1;
		this$1 = this;
		this$1.state = $append(this$1.state, s);
		this$1.attrib = $append(this$1.attrib, a);
	};
	stack.prototype.push = function(s, a) { return this.$val.push(s, a); };
	stack.ptr.prototype.top = function() {
		var $ptr, this$1, x, x$1;
		this$1 = this;
		return (x = this$1.state, x$1 = this$1.state.$length - 1 >> 0, ((x$1 < 0 || x$1 >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + x$1]));
	};
	stack.prototype.top = function() { return this.$val.top(); };
	stack.ptr.prototype.peek = function(pos) {
		var $ptr, pos, this$1, x;
		this$1 = this;
		return (x = this$1.state, ((pos < 0 || pos >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + pos]));
	};
	stack.prototype.peek = function(pos) { return this.$val.peek(pos); };
	stack.ptr.prototype.topIndex = function() {
		var $ptr, this$1;
		this$1 = this;
		return this$1.state.$length - 1 >> 0;
	};
	stack.prototype.topIndex = function() { return this.$val.topIndex(); };
	stack.ptr.prototype.popN = function(items) {
		var $ptr, _tmp, _tmp$1, attrib, hi, items, lo, this$1;
		this$1 = this;
		_tmp = this$1.state.$length - items >> 0;
		_tmp$1 = this$1.state.$length;
		lo = _tmp;
		hi = _tmp$1;
		attrib = $subslice(this$1.attrib, lo, hi);
		this$1.state = $subslice(this$1.state, 0, lo);
		this$1.attrib = $subslice(this$1.attrib, 0, lo);
		return attrib;
	};
	stack.prototype.popN = function(items) { return this.$val.popN(items); };
	stack.ptr.prototype.String = function() {
		var $ptr, S, _i, _r, _r$1, _r$2, _r$3, _ref, i, st, w, x, x$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; S = $f.S; _i = $f._i; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _ref = $f._ref; i = $f.i; st = $f.st; w = $f.w; x = $f.x; x$1 = $f.x$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		S = this;
		w = new bytes.Buffer.ptr(sliceType$3.nil, 0, arrayType$2.zero(), arrayType$3.zero(), 0);
		_r = fmt.Fprintf(w, "stack:\n", new sliceType([])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r;
		_ref = S.state;
		_i = 0;
		/* while (true) { */ case 2:
			/* if (!(_i < _ref.$length)) { break; } */ if(!(_i < _ref.$length)) { $s = 3; continue; }
			i = _i;
			st = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			_r$1 = fmt.Fprintf(w, "\t%d:%d , ", new sliceType([new $Int(i), new $Int(st)])); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			_r$1;
			/* */ if ($interfaceIsEqual((x = S.attrib, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i])), $ifaceNil)) { $s = 5; continue; }
			/* */ $s = 6; continue;
			/* if ($interfaceIsEqual((x = S.attrib, ((i < 0 || i >= x.$length) ? $throwRuntimeError("index out of range") : x.$array[x.$offset + i])), $ifaceNil)) { */ case 5:
				_r$2 = fmt.Fprintf(w, "nil", new sliceType([])); /* */ $s = 8; case 8: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_r$2;
				$s = 7; continue;
			/* } else { */ case 6:
				_r$3 = fmt.Fprintf(w, "%v", new sliceType([(x$1 = S.attrib, ((i < 0 || i >= x$1.$length) ? $throwRuntimeError("index out of range") : x$1.$array[x$1.$offset + i]))])); /* */ $s = 9; case 9: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				_r$3;
			/* } */ case 7:
			w.WriteString("\n");
			_i++;
		/* } */ $s = 2; continue; case 3:
		return w.String();
		/* */ } return; } if ($f === undefined) { $f = { $blk: stack.ptr.prototype.String }; } $f.$ptr = $ptr; $f.S = S; $f._i = _i; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._ref = _ref; $f.i = i; $f.st = st; $f.w = w; $f.x = x; $f.x$1 = x$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	stack.prototype.String = function() { return this.$val.String(); };
	NewParser = function() {
		var $ptr, p;
		p = new Parser.ptr(newStack(), ptrType$4.nil, 0);
		p.Reset();
		return p;
	};
	$pkg.NewParser = NewParser;
	Parser.ptr.prototype.Reset = function() {
		var $ptr, P;
		P = this;
		P.stack.reset();
		P.stack.push(0, $ifaceNil);
	};
	Parser.prototype.Reset = function() { return this.$val.Reset(); };
	Parser.ptr.prototype.Error = function(err, scanner) {
		var $ptr, P, _i, _r, _ref, action$1, action$2, action$3, action$4, err, errorAttrib, recovered, scanner, t, x, x$1, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; P = $f.P; _i = $f._i; _r = $f._r; _ref = $f._ref; action$1 = $f.action$1; action$2 = $f.action$2; action$3 = $f.action$3; action$4 = $f.action$4; err = $f.err; errorAttrib = $f.errorAttrib; recovered = $f.recovered; scanner = $f.scanner; t = $f.t; x = $f.x; x$1 = $f.x$1; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; x$6 = $f.x$6; x$7 = $f.x$7; x$8 = $f.x$8; x$9 = $f.x$9; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		recovered = false;
		errorAttrib = ptrType$5.nil;
		P = this;
		errorAttrib = new errors$1.Error.ptr(err, P.nextToken, P.popNonRecoveryStates(), $makeSlice(sliceType$5, 0, 8));
		_ref = (x = P.stack.top(), ((x < 0 || x >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x])).actions;
		_i = 0;
		while (true) {
			if (!(_i < 17)) { break; }
			t = _i;
			action$1 = ((_i < 0 || _i >= _ref.length) ? $throwRuntimeError("index out of range") : _ref[_i]);
			if (!($interfaceIsEqual(action$1, $ifaceNil))) {
				errorAttrib.ExpectedTokens = $append(errorAttrib.ExpectedTokens, token.TokMap.Id((t >> 0)));
			}
			_i++;
		}
		action$2 = (x$1 = (x$2 = P.stack.top(), ((x$2 < 0 || x$2 >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x$2])).actions, x$3 = token.TokMap.Type("error"), ((x$3 < 0 || x$3 >= x$1.length) ? $throwRuntimeError("index out of range") : x$1[x$3]));
		if (!($interfaceIsEqual(action$2, $ifaceNil))) {
			P.stack.push(($assertType(action$2, shift) >> 0), errorAttrib);
		} else {
			return [recovered, errorAttrib];
		}
		action$3 = (x$4 = (x$5 = P.stack.top(), ((x$5 < 0 || x$5 >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x$5])).actions, x$6 = P.nextToken.Type, ((x$6 < 0 || x$6 >= x$4.length) ? $throwRuntimeError("index out of range") : x$4[x$6]));
		if (!($interfaceIsEqual(action$3, $ifaceNil))) {
			recovered = true;
		}
		/* while (true) { */ case 1:
			/* if (!(!recovered && !((P.nextToken.Type === 1)))) { break; } */ if(!(!recovered && !((P.nextToken.Type === 1)))) { $s = 2; continue; }
			_r = scanner.Scan(); /* */ $s = 3; case 3: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
			P.nextToken = _r;
			action$4 = (x$7 = (x$8 = P.stack.top(), ((x$8 < 0 || x$8 >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x$8])).actions, x$9 = P.nextToken.Type, ((x$9 < 0 || x$9 >= x$7.length) ? $throwRuntimeError("index out of range") : x$7[x$9]));
			if (!($interfaceIsEqual(action$4, $ifaceNil))) {
				recovered = true;
			}
		/* } */ $s = 1; continue; case 2:
		return [recovered, errorAttrib];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Parser.ptr.prototype.Error }; } $f.$ptr = $ptr; $f.P = P; $f._i = _i; $f._r = _r; $f._ref = _ref; $f.action$1 = action$1; $f.action$2 = action$2; $f.action$3 = action$3; $f.action$4 = action$4; $f.err = err; $f.errorAttrib = errorAttrib; $f.recovered = recovered; $f.scanner = scanner; $f.t = t; $f.x = x; $f.x$1 = x$1; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.x$6 = x$6; $f.x$7 = x$7; $f.x$8 = x$8; $f.x$9 = x$9; $f.$s = $s; $f.$r = $r; return $f;
	};
	Parser.prototype.Error = function(err, scanner) { return this.$val.Error(err, scanner); };
	Parser.ptr.prototype.popNonRecoveryStates = function() {
		var $ptr, P, _i, _ref, _tuple, e, errorSymbols, i, ok, removedAttribs, rs;
		removedAttribs = sliceType$4.nil;
		P = this;
		_tuple = P.firstRecoveryState();
		rs = _tuple[0];
		ok = _tuple[1];
		if (ok) {
			errorSymbols = P.stack.popN((P.stack.topIndex() - rs >> 0));
			removedAttribs = $makeSlice(sliceType$4, errorSymbols.$length);
			_ref = errorSymbols;
			_i = 0;
			while (true) {
				if (!(_i < _ref.$length)) { break; }
				i = _i;
				e = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
				((i < 0 || i >= removedAttribs.$length) ? $throwRuntimeError("index out of range") : removedAttribs.$array[removedAttribs.$offset + i] = e);
				_i++;
			}
		} else {
			removedAttribs = new sliceType$4([]);
		}
		return removedAttribs;
	};
	Parser.prototype.popNonRecoveryStates = function() { return this.$val.popNonRecoveryStates(); };
	Parser.ptr.prototype.firstRecoveryState = function() {
		var $ptr, P, _tmp, _tmp$1, canRecover, recoveryState, x, x$1;
		recoveryState = 0;
		canRecover = false;
		P = this;
		_tmp = P.stack.topIndex();
		_tmp$1 = (x = P.stack.top(), ((x < 0 || x >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x])).canRecover;
		recoveryState = _tmp;
		canRecover = _tmp$1;
		while (true) {
			if (!(recoveryState > 0 && !canRecover)) { break; }
			recoveryState = recoveryState - (1) >> 0;
			canRecover = (x$1 = P.stack.peek(recoveryState), ((x$1 < 0 || x$1 >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x$1])).canRecover;
		}
		return [recoveryState, canRecover];
	};
	Parser.prototype.firstRecoveryState = function() { return this.$val.firstRecoveryState(); };
	Parser.ptr.prototype.newError = function(err) {
		var $ptr, P, _arg, _arg$1, _arg$2, _arg$3, _i, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _ref, actRow, err, i, t, w, x, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; P = $f.P; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _arg$3 = $f._arg$3; _i = $f._i; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _ref = $f._ref; actRow = $f.actRow; err = $f.err; i = $f.i; t = $f.t; w = $f.w; x = $f.x; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		P = this;
		w = new bytes.Buffer.ptr(sliceType$3.nil, 0, arrayType$2.zero(), arrayType$3.zero(), 0);
		_arg = w;
		_arg$1 = new $Int(P.stack.top());
		_r = token.TokMap.TokenString(P.nextToken); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_arg$2 = new $String(_r);
		_r$1 = P.nextToken.Pos.String(); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		_arg$3 = new $String(_r$1);
		_r$2 = fmt.Fprintf(_arg, "Error in S%d: %s, %s", new sliceType([_arg$1, _arg$2, _arg$3])); /* */ $s = 3; case 3: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		_r$2;
		/* */ if (!($interfaceIsEqual(err, $ifaceNil))) { $s = 4; continue; }
		/* */ $s = 5; continue;
		/* if (!($interfaceIsEqual(err, $ifaceNil))) { */ case 4:
			_r$3 = err.Error(); /* */ $s = 7; case 7: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_r$4 = w.WriteString(_r$3); /* */ $s = 8; case 8: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
			_r$4;
			$s = 6; continue;
		/* } else { */ case 5:
			w.WriteString(", expected one of: ");
			actRow = $clone((x = P.stack.top(), ((x < 0 || x >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x])), actionRow);
			_ref = actRow.actions;
			_i = 0;
			/* while (true) { */ case 9:
				/* if (!(_i < 17)) { break; } */ if(!(_i < 17)) { $s = 10; continue; }
				i = _i;
				t = ((_i < 0 || _i >= _ref.length) ? $throwRuntimeError("index out of range") : _ref[_i]);
				/* */ if (!($interfaceIsEqual(t, $ifaceNil))) { $s = 11; continue; }
				/* */ $s = 12; continue;
				/* if (!($interfaceIsEqual(t, $ifaceNil))) { */ case 11:
					_r$5 = fmt.Fprintf(w, "%s ", new sliceType([new $String(token.TokMap.Id((i >> 0)))])); /* */ $s = 13; case 13: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
					_r$5;
				/* } */ case 12:
				_i++;
			/* } */ $s = 9; continue; case 10:
		/* } */ case 6:
		return errors.New(w.String());
		/* */ } return; } if ($f === undefined) { $f = { $blk: Parser.ptr.prototype.newError }; } $f.$ptr = $ptr; $f.P = P; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._arg$3 = _arg$3; $f._i = _i; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._ref = _ref; $f.actRow = actRow; $f.err = err; $f.i = i; $f.t = t; $f.w = w; $f.x = x; $f.$s = $s; $f.$r = $r; return $f;
	};
	Parser.prototype.newError = function(err) { return this.$val.newError(err); };
	Parser.ptr.prototype.Parse = function(scanner) {
		var $ptr, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _ref, _tmp, _tmp$1, _tmp$2, _tmp$3, _tmp$4, _tmp$5, _tuple, _tuple$1, acc, act, act$1, act$2, act$3, action$1, attrib, err, err$1, errAttrib, prod, recovered, res, scanner, this$1, x, x$1, x$10, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _ref = $f._ref; _tmp = $f._tmp; _tmp$1 = $f._tmp$1; _tmp$2 = $f._tmp$2; _tmp$3 = $f._tmp$3; _tmp$4 = $f._tmp$4; _tmp$5 = $f._tmp$5; _tuple = $f._tuple; _tuple$1 = $f._tuple$1; acc = $f.acc; act = $f.act; act$1 = $f.act$1; act$2 = $f.act$2; act$3 = $f.act$3; action$1 = $f.action$1; attrib = $f.attrib; err = $f.err; err$1 = $f.err$1; errAttrib = $f.errAttrib; prod = $f.prod; recovered = $f.recovered; res = $f.res; scanner = $f.scanner; this$1 = $f.this$1; x = $f.x; x$1 = $f.x$1; x$10 = $f.x$10; x$2 = $f.x$2; x$3 = $f.x$3; x$4 = $f.x$4; x$5 = $f.x$5; x$6 = $f.x$6; x$7 = $f.x$7; x$8 = $f.x$8; x$9 = $f.x$9; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		res = $ifaceNil;
		err = $ifaceNil;
		this$1 = this;
		this$1.Reset();
		_r = scanner.Scan(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		this$1.nextToken = _r;
		acc = false;
		/* while (true) { */ case 2:
			/* if (!(!acc)) { break; } */ if(!(!acc)) { $s = 3; continue; }
			action$1 = (x = (x$1 = this$1.stack.top(), ((x$1 < 0 || x$1 >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x$1])).actions, x$2 = this$1.nextToken.Type, ((x$2 < 0 || x$2 >= x.length) ? $throwRuntimeError("index out of range") : x[x$2]));
			/* */ if ($interfaceIsEqual(action$1, $ifaceNil)) { $s = 4; continue; }
			/* */ $s = 5; continue;
			/* if ($interfaceIsEqual(action$1, $ifaceNil)) { */ case 4:
				_r$1 = this$1.Error($ifaceNil, scanner); /* */ $s = 6; case 6: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				_tuple = _r$1;
				recovered = _tuple[0];
				errAttrib = _tuple[1];
				/* */ if (!recovered) { $s = 7; continue; }
				/* */ $s = 8; continue;
				/* if (!recovered) { */ case 7:
					this$1.nextToken = errAttrib.ErrorToken;
					_tmp = $ifaceNil;
					_r$2 = this$1.newError($ifaceNil); /* */ $s = 9; case 9: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					_tmp$1 = _r$2;
					res = _tmp;
					err = _tmp$1;
					/* */ $s = 10; case 10:
					return [res, err];
				/* } */ case 8:
				action$1 = (x$3 = (x$4 = this$1.stack.top(), ((x$4 < 0 || x$4 >= actionTab.length) ? $throwRuntimeError("index out of range") : actionTab[x$4])).actions, x$5 = this$1.nextToken.Type, ((x$5 < 0 || x$5 >= x$3.length) ? $throwRuntimeError("index out of range") : x$3[x$5]));
				if ($interfaceIsEqual(action$1, $ifaceNil)) {
					$panic(new $String("Error recovery led to invalid action"));
				}
			/* } */ case 5:
			_ref = action$1;
			/* */ if ($assertType(_ref, accept, true)[1]) { $s = 11; continue; }
			/* */ if ($assertType(_ref, shift, true)[1]) { $s = 12; continue; }
			/* */ if ($assertType(_ref, reduce, true)[1]) { $s = 13; continue; }
			/* */ $s = 14; continue;
			/* if ($assertType(_ref, accept, true)[1]) { */ case 11:
				act = _ref.$val;
				res = (x$6 = this$1.stack.popN(1), (0 >= x$6.$length ? $throwRuntimeError("index out of range") : x$6.$array[x$6.$offset + 0]));
				acc = true;
				$s = 15; continue;
			/* } else if ($assertType(_ref, shift, true)[1]) { */ case 12:
				act$1 = _ref.$val;
				this$1.stack.push((act$1 >> 0), this$1.nextToken);
				_r$3 = scanner.Scan(); /* */ $s = 16; case 16: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				this$1.nextToken = _r$3;
				$s = 15; continue;
			/* } else if ($assertType(_ref, reduce, true)[1]) { */ case 13:
				act$2 = _ref.$val;
				prod = $clone((x$7 = (act$2 >> 0), ((x$7 < 0 || x$7 >= productionsTable.length) ? $throwRuntimeError("index out of range") : productionsTable[x$7])), ProdTabEntry);
				_r$4 = prod.ReduceFunc(this$1.stack.popN(prod.NumSymbols)); /* */ $s = 17; case 17: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				_tuple$1 = _r$4;
				attrib = _tuple$1[0];
				err$1 = _tuple$1[1];
				/* */ if (!($interfaceIsEqual(err$1, $ifaceNil))) { $s = 18; continue; }
				/* */ $s = 19; continue;
				/* if (!($interfaceIsEqual(err$1, $ifaceNil))) { */ case 18:
					_tmp$2 = $ifaceNil;
					_r$5 = this$1.newError(err$1); /* */ $s = 21; case 21: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
					_tmp$3 = _r$5;
					res = _tmp$2;
					err = _tmp$3;
					/* */ $s = 22; case 22:
					return [res, err];
				/* } else { */ case 19:
					this$1.stack.push((x$8 = (x$9 = this$1.stack.top(), ((x$9 < 0 || x$9 >= gotoTab.length) ? $throwRuntimeError("index out of range") : gotoTab[x$9])), x$10 = prod.NTType, ((x$10 < 0 || x$10 >= x$8.length) ? $throwRuntimeError("index out of range") : x$8[x$10])), attrib);
				/* } */ case 20:
				$s = 15; continue;
			/* } else { */ case 14:
				act$3 = _ref;
				_r$6 = action$1.String(); /* */ $s = 23; case 23: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
				$panic(new $String("unknown action: " + _r$6));
			/* } */ case 15:
		/* } */ $s = 2; continue; case 3:
		_tmp$4 = res;
		_tmp$5 = $ifaceNil;
		res = _tmp$4;
		err = _tmp$5;
		return [res, err];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Parser.ptr.prototype.Parse }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._ref = _ref; $f._tmp = _tmp; $f._tmp$1 = _tmp$1; $f._tmp$2 = _tmp$2; $f._tmp$3 = _tmp$3; $f._tmp$4 = _tmp$4; $f._tmp$5 = _tmp$5; $f._tuple = _tuple; $f._tuple$1 = _tuple$1; $f.acc = acc; $f.act = act; $f.act$1 = act$1; $f.act$2 = act$2; $f.act$3 = act$3; $f.action$1 = action$1; $f.attrib = attrib; $f.err = err; $f.err$1 = err$1; $f.errAttrib = errAttrib; $f.prod = prod; $f.recovered = recovered; $f.res = res; $f.scanner = scanner; $f.this$1 = this$1; $f.x = x; $f.x$1 = x$1; $f.x$10 = x$10; $f.x$2 = x$2; $f.x$3 = x$3; $f.x$4 = x$4; $f.x$5 = x$5; $f.x$6 = x$6; $f.x$7 = x$7; $f.x$8 = x$8; $f.x$9 = x$9; $f.$s = $s; $f.$r = $r; return $f;
	};
	Parser.prototype.Parse = function(scanner) { return this.$val.Parse(scanner); };
	newString = function(v) {
		var $ptr, t, v;
		t = $assertType(v, ptrType$4);
		return $bytesToString(t.Lit);
	};
	accept.methods = [{prop: "act", name: "act", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [], false)}, {prop: "Equal", name: "Equal", pkg: "", typ: $funcType([action], [$Bool], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	shift.methods = [{prop: "act", name: "act", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [], false)}, {prop: "Equal", name: "Equal", pkg: "", typ: $funcType([action], [$Bool], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	reduce.methods = [{prop: "act", name: "act", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [], false)}, {prop: "Equal", name: "Equal", pkg: "", typ: $funcType([action], [$Bool], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$3.methods = [{prop: "reset", name: "reset", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [], false)}, {prop: "push", name: "push", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([$Int, Attrib], [], false)}, {prop: "top", name: "top", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [$Int], false)}, {prop: "peek", name: "peek", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([$Int], [$Int], false)}, {prop: "topIndex", name: "topIndex", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [$Int], false)}, {prop: "popN", name: "popN", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([$Int], [sliceType$2], false)}, {prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}];
	ptrType$6.methods = [{prop: "Reset", name: "Reset", pkg: "", typ: $funcType([], [], false)}, {prop: "Error", name: "Error", pkg: "", typ: $funcType([$error, Scanner], [$Bool, ptrType$5], false)}, {prop: "popNonRecoveryStates", name: "popNonRecoveryStates", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [sliceType$4], false)}, {prop: "firstRecoveryState", name: "firstRecoveryState", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [$Int, $Bool], false)}, {prop: "newError", name: "newError", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([$error], [$error], false)}, {prop: "Parse", name: "Parse", pkg: "", typ: $funcType([Scanner], [$emptyInterface, $error], false)}];
	action.init([{prop: "String", name: "String", pkg: "", typ: $funcType([], [$String], false)}, {prop: "act", name: "act", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $funcType([], [], false)}]);
	actionRow.init([{prop: "canRecover", name: "canRecover", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $Bool, tag: ""}, {prop: "actions", name: "actions", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: arrayType$1, tag: ""}]);
	stack.init([{prop: "state", name: "state", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: sliceType$1, tag: ""}, {prop: "attrib", name: "attrib", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: sliceType$2, tag: ""}]);
	Parser.init([{prop: "stack", name: "stack", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: ptrType$3, tag: ""}, {prop: "nextToken", name: "nextToken", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: ptrType$4, tag: ""}, {prop: "pos", name: "pos", pkg: "github.com/awalterschulze/ragax/golang/parser", typ: $Int, tag: ""}]);
	Scanner.init([{prop: "Scan", name: "Scan", pkg: "", typ: $funcType([], [ptrType$4], false)}]);
	ProdTabEntry.init([{prop: "String", name: "String", pkg: "", typ: $String, tag: ""}, {prop: "Id", name: "Id", pkg: "", typ: $String, tag: ""}, {prop: "NTType", name: "NTType", pkg: "", typ: $Int, tag: ""}, {prop: "Index", name: "Index", pkg: "", typ: $Int, tag: ""}, {prop: "NumSymbols", name: "NumSymbols", pkg: "", typ: $Int, tag: ""}, {prop: "ReduceFunc", name: "ReduceFunc", pkg: "", typ: funcType, tag: ""}]);
	Attrib.init([]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = bytes.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = errors.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = fmt.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = ast.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = errors$1.$init(); /* */ $s = 5; case 5: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = token.$init(); /* */ $s = 6; case 6: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		gotoTab = $toNativeArray($kindArray, [$toNativeArray($kindInt, [-1, 1, 2, -1]), $toNativeArray($kindInt, [-1, -1, 4, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 8]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 13]), $toNativeArray($kindInt, [-1, -1, -1, 18]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 25]), $toNativeArray($kindInt, [-1, -1, -1, 26]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 29]), $toNativeArray($kindInt, [-1, -1, -1, 30]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 33]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 42]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 50]), $toNativeArray($kindInt, [-1, -1, -1, 51]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 53]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 55]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 57]), $toNativeArray($kindInt, [-1, -1, -1, 58]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 60]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 62]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 73]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 75]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 76]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, 78]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1]), $toNativeArray($kindInt, [-1, -1, -1, -1])]);
		actionTab = $toNativeArray($kindStruct, [new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, new shift(3), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new accept(true), new shift(3), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(1), new reduce(1), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(5), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(2), new reduce(2), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(6), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(7), $ifaceNil, new shift(9), $ifaceNil, $ifaceNil, new shift(10), $ifaceNil, $ifaceNil, $ifaceNil, new shift(11), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(8), new reduce(8), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(3), new reduce(3), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(12), $ifaceNil, new shift(14), $ifaceNil, $ifaceNil, new shift(15), $ifaceNil, $ifaceNil, $ifaceNil, new shift(16), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(17), $ifaceNil, new shift(19), $ifaceNil, $ifaceNil, new shift(20), $ifaceNil, $ifaceNil, $ifaceNil, new shift(21), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(22), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(8), new reduce(8), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(23), new shift(24), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(12), $ifaceNil, new shift(14), $ifaceNil, $ifaceNil, new shift(15), $ifaceNil, $ifaceNil, $ifaceNil, new shift(16), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(17), $ifaceNil, new shift(19), $ifaceNil, $ifaceNil, new shift(20), $ifaceNil, $ifaceNil, $ifaceNil, new shift(21), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(27), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(8), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(28), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(12), $ifaceNil, new shift(14), $ifaceNil, $ifaceNil, new shift(15), $ifaceNil, $ifaceNil, $ifaceNil, new shift(16), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(17), $ifaceNil, new shift(19), $ifaceNil, $ifaceNil, new shift(20), $ifaceNil, $ifaceNil, $ifaceNil, new shift(21), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(31), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(7), new reduce(7), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(32), $ifaceNil, new shift(34), $ifaceNil, $ifaceNil, new shift(35), $ifaceNil, $ifaceNil, $ifaceNil, new shift(36), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(37), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(38), new shift(39), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(40), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(7), new reduce(7), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(41), $ifaceNil, new shift(43), $ifaceNil, $ifaceNil, new shift(44), $ifaceNil, $ifaceNil, $ifaceNil, new shift(45), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(46), new shift(47), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(48), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(7), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(8), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(49), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(12), $ifaceNil, new shift(14), $ifaceNil, $ifaceNil, new shift(15), $ifaceNil, $ifaceNil, $ifaceNil, new shift(16), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(17), $ifaceNil, new shift(19), $ifaceNil, $ifaceNil, new shift(20), $ifaceNil, $ifaceNil, $ifaceNil, new shift(21), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(52), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(6), new reduce(6), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(32), $ifaceNil, new shift(34), $ifaceNil, $ifaceNil, new shift(35), $ifaceNil, $ifaceNil, $ifaceNil, new shift(36), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(54), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(41), $ifaceNil, new shift(43), $ifaceNil, $ifaceNil, new shift(44), $ifaceNil, $ifaceNil, $ifaceNil, new shift(45), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(8), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(56), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(12), $ifaceNil, new shift(14), $ifaceNil, $ifaceNil, new shift(15), $ifaceNil, $ifaceNil, $ifaceNil, new shift(16), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(17), $ifaceNil, new shift(19), $ifaceNil, $ifaceNil, new shift(20), $ifaceNil, $ifaceNil, $ifaceNil, new shift(21), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(59), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(32), $ifaceNil, new shift(34), $ifaceNil, $ifaceNil, new shift(35), $ifaceNil, $ifaceNil, $ifaceNil, new shift(36), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(61), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(41), $ifaceNil, new shift(43), $ifaceNil, $ifaceNil, new shift(44), $ifaceNil, $ifaceNil, $ifaceNil, new shift(45), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(4), new reduce(4), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(63), new shift(64), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(65), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(7), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(66), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(6), new reduce(6), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(67), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, new reduce(5), new reduce(5), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(68), new shift(69), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(70), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(7), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(71), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(6), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(72), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(32), $ifaceNil, new shift(34), $ifaceNil, $ifaceNil, new shift(35), $ifaceNil, $ifaceNil, $ifaceNil, new shift(36), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(74), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(41), $ifaceNil, new shift(43), $ifaceNil, $ifaceNil, new shift(44), $ifaceNil, $ifaceNil, $ifaceNil, new shift(45), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(4), new reduce(4), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(5), new reduce(5), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(32), $ifaceNil, new shift(34), $ifaceNil, $ifaceNil, new shift(35), $ifaceNil, $ifaceNil, $ifaceNil, new shift(36), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(77), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, new shift(41), $ifaceNil, new shift(43), $ifaceNil, $ifaceNil, new shift(44), $ifaceNil, $ifaceNil, $ifaceNil, new shift(45), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(4), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(5), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(79), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(6), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(80), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(81), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(6), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new shift(82), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(4), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(5), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(4), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil])), new actionRow.ptr(false, $toNativeArray($kindInterface, [$ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, new reduce(5), $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil, $ifaceNil]))]);
		productionsTable = $toNativeArray($kindStruct, [new ProdTabEntry.ptr("S' : Definitions\t<<  >>", "S'", 0, 0, 1, (function(X) {
			var $ptr, X;
			return [(0 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 0]), $ifaceNil];
		})), new ProdTabEntry.ptr("Definitions : Definition\t<< Defs{X[0].(*Def)}, nil >>", "Definitions", 1, 1, 1, (function(X) {
			var $ptr, X;
			return [new ast.Defs([$assertType((0 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 0]), ptrType)]), $ifaceNil];
		})), new ProdTabEntry.ptr("Definitions : Definitions Definition\t<< append(X[0].(Defs), X[1].(*Def)), nil >>", "Definitions", 1, 2, 2, (function(X) {
			var $ptr, X;
			return [$append($assertType((0 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 0]), ast.Defs), $assertType((1 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 1]), ptrType)), $ifaceNil];
		})), new ProdTabEntry.ptr("Definition : \"#\" id \"=\" Expr\t<< &Def{Name: newString(X[1]), Expr: NewExpr(X[3].(*Expr))}, nil >>", "Definition", 2, 3, 4, (function(X) {
			var $ptr, X;
			return [new ast.Def.ptr(newString((1 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 1])), ast.NewExpr($assertType((3 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 3]), ptrType$2))), $ifaceNil];
		})), new ProdTabEntry.ptr("Expr : \"(\" Expr \"|\" Expr \")\"\t<< &Expr{\n\t\tType: Or, \n\t\tExpr: NewExpr(X[1].(*Expr)), \n\t\tExpr2: NewExpr(X[3].(*Expr)),\n\t}, nil >>", "Expr", 3, 4, 5, (function(X) {
			var $ptr, X;
			return [new ast.Expr.ptr(ast.Or, ast.NewExpr($assertType((1 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 1]), ptrType$2)), ast.NewExpr($assertType((3 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 3]), ptrType$2)), ""), $ifaceNil];
		})), new ProdTabEntry.ptr("Expr : \"[\" Expr \",\" Expr \"]\"\t<< &Expr{\n\t\tType: Concat, \n\t\tExpr: NewExpr(X[1].(*Expr)), \n\t\tExpr2: NewExpr(X[3].(*Expr)),\n\t}, nil >>", "Expr", 3, 5, 5, (function(X) {
			var $ptr, X;
			return [new ast.Expr.ptr(ast.Concat, ast.NewExpr($assertType((1 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 1]), ptrType$2)), ast.NewExpr($assertType((3 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 3]), ptrType$2)), ""), $ifaceNil];
		})), new ProdTabEntry.ptr("Expr : \"(\" Expr \")\" \"*\"\t<< &Expr{\n\t\tType: ZeroOrMore, \n\t\tExpr: NewExpr(X[1].(*Expr)),\n\t}, nil >>", "Expr", 3, 6, 4, (function(X) {
			var $ptr, X;
			return [new ast.Expr.ptr(ast.ZeroOrMore, ast.NewExpr($assertType((1 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 1]), ptrType$2)), ptrType$1.nil, ""), $ifaceNil];
		})), new ProdTabEntry.ptr("Expr : \"@\" id\t<< &Expr{\n\t\tType: Reference, \n\t\tValue: newString(X[1]),\n\t}, nil >>", "Expr", 3, 7, 2, (function(X) {
			var $ptr, X;
			return [new ast.Expr.ptr(ast.Reference, ptrType$1.nil, ptrType$1.nil, newString((1 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 1]))), $ifaceNil];
		})), new ProdTabEntry.ptr("Expr : id\t<< &Expr{\n\t\tType: Value, \n\t\tValue: newString(X[0]),\n\t}, nil >>", "Expr", 3, 8, 1, (function(X) {
			var $ptr, X;
			return [new ast.Expr.ptr(ast.Value, ptrType$1.nil, ptrType$1.nil, newString((0 >= X.$length ? $throwRuntimeError("index out of range") : X.$array[X.$offset + 0]))), $ifaceNil];
		}))]);
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["github.com/awalterschulze/ragax/golang/deriv"] = (function() {
	var $pkg = {}, $init, fmt, ast, lexer, parser, Mem, deriv, nullable, ptrType, sliceType, sliceType$1, sliceType$2, ptrType$1, mapType, ptrType$2, ptrType$3, mapType$1, emptyset, empty, Parse, Eval, lookup, Nullable, isEmptySet, isEmpty, Simplify;
	fmt = $packages["fmt"];
	ast = $packages["github.com/awalterschulze/ragax/golang/ast"];
	lexer = $packages["github.com/awalterschulze/ragax/golang/lexer"];
	parser = $packages["github.com/awalterschulze/ragax/golang/parser"];
	Mem = $pkg.Mem = $newType(4, $kindMap, "deriv.Mem", "Mem", "github.com/awalterschulze/ragax/golang/deriv", null);
	deriv = $pkg.deriv = $newType(0, $kindStruct, "deriv.deriv", "deriv", "github.com/awalterschulze/ragax/golang/deriv", function(mem_, defs_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.mem = false;
			this.defs = ast.Defs.nil;
			return;
		}
		this.mem = mem_;
		this.defs = defs_;
	});
	nullable = $pkg.nullable = $newType(0, $kindStruct, "deriv.nullable", "nullable", "github.com/awalterschulze/ragax/golang/deriv", function(defs_, mem_, visited_) {
		this.$val = this;
		if (arguments.length === 0) {
			this.defs = ast.Defs.nil;
			this.mem = false;
			this.visited = false;
			return;
		}
		this.defs = defs_;
		this.mem = mem_;
		this.visited = visited_;
	});
	ptrType = $ptrType(ast.LazyExpr);
	sliceType = $sliceType($Uint8);
	sliceType$1 = $sliceType($Int32);
	sliceType$2 = $sliceType($emptyInterface);
	ptrType$1 = $ptrType(ast.Expr);
	mapType = $mapType($Int32, ptrType$1);
	ptrType$2 = $ptrType(deriv);
	ptrType$3 = $ptrType(nullable);
	mapType$1 = $mapType(ptrType, $Bool);
	Parse = function(s) {
		var $ptr, _r, _tuple, err, l, p, r, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _tuple = $f._tuple; err = $f.err; l = $f.l; p = $f.p; r = $f.r; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		p = parser.NewParser();
		l = lexer.NewLexer(new sliceType($stringToBytes(s)));
		_r = p.Parse(l); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		r = _tuple[0];
		err = _tuple[1];
		if (!($interfaceIsEqual(err, $ifaceNil))) {
			return [ast.Defs.nil, err];
		}
		return [$assertType(r, ast.Defs), $ifaceNil];
		/* */ } return; } if ($f === undefined) { $f = { $blk: Parse }; } $f.$ptr = $ptr; $f._r = _r; $f._tuple = _tuple; $f.err = err; $f.l = l; $f.p = p; $f.r = r; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Parse = Parse;
	Eval = function(defs, s) {
		var $ptr, _i, _r, _r$1, _r$2, _r$3, _ref, _rune, c, d, defs, deriv$1, mem, s, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _i = $f._i; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _ref = $f._ref; _rune = $f._rune; c = $f.c; d = $f.d; defs = $f.defs; deriv$1 = $f.deriv$1; mem = $f.mem; s = $f.s; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = (0 >= defs.$length ? $throwRuntimeError("index out of range") : defs.$array[defs.$offset + 0]).Expr.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		d = _r;
		mem = {};
		deriv$1 = new deriv.ptr(mem, defs);
		_ref = s;
		_i = 0;
		/* while (true) { */ case 2:
			/* if (!(_i < _ref.length)) { break; } */ if(!(_i < _ref.length)) { $s = 3; continue; }
			_rune = $decodeRune(_ref, _i);
			c = _rune[0];
			_r$1 = fmt.Printf("-- %s deriv %s\n", new sliceType$2([d, new $String($runesToString(new sliceType$1([c])))])); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			_r$1;
			_r$2 = deriv$1.Deriv(d, c); /* */ $s = 5; case 5: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
			d = _r$2;
			_r$3 = fmt.Printf("== res %s\n", new sliceType$2([d])); /* */ $s = 6; case 6: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
			_r$3;
			_i += _rune[1];
		/* } */ $s = 2; continue; case 3:
		return d;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Eval }; } $f.$ptr = $ptr; $f._i = _i; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._ref = _ref; $f._rune = _rune; $f.c = c; $f.d = d; $f.defs = defs; $f.deriv$1 = deriv$1; $f.mem = mem; $f.s = s; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Eval = Eval;
	Mem.prototype.Get = function(e, r) {
		var $ptr, _entry, _entry$1, _tuple, e, m, mm, ok, r;
		m = this.$val;
		_tuple = (_entry = m[ptrType$1.keyFor(e)], _entry !== undefined ? [_entry.v, true] : [false, false]);
		mm = _tuple[0];
		ok = _tuple[1];
		if (ok) {
			return (_entry$1 = mm[$Int32.keyFor(r)], _entry$1 !== undefined ? _entry$1.v : ptrType$1.nil);
		}
		return ptrType$1.nil;
	};
	$ptrType(Mem).prototype.Get = function(e, r) { return new Mem(this.$get()).Get(e, r); };
	Mem.prototype.Set = function(e, r, v) {
		var $ptr, _entry, _entry$1, _key, _key$1, _tuple, e, m, ok, r, v;
		m = this.$val;
		_tuple = (_entry = m[ptrType$1.keyFor(e)], _entry !== undefined ? [_entry.v, true] : [false, false]);
		ok = _tuple[1];
		if (!ok) {
			_key = e; (m || $throwRuntimeError("assignment to entry in nil map"))[ptrType$1.keyFor(_key)] = { k: _key, v: {} };
		}
		_key$1 = r; ((_entry$1 = m[ptrType$1.keyFor(e)], _entry$1 !== undefined ? _entry$1.v : false) || $throwRuntimeError("assignment to entry in nil map"))[$Int32.keyFor(_key$1)] = { k: _key$1, v: v };
		return v;
	};
	$ptrType(Mem).prototype.Set = function(e, r, v) { return new Mem(this.$get()).Set(e, r, v); };
	deriv.ptr.prototype.Sderiv = function(e, c) {
		var $ptr, _arg, _arg$1, _r, _r$1, c, e, this$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _arg = $f._arg; _arg$1 = $f._arg$1; _r = $f._r; _r$1 = $f._r$1; c = $f.c; e = $f.e; this$1 = $f.this$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = this;
		_arg = this$1.defs;
		_r = this$1.Deriv(e, c); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_arg$1 = _r;
		_r$1 = Simplify(_arg, _arg$1); /* */ $s = 2; case 2: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
		/* */ $s = 3; case 3:
		return _r$1;
		/* */ } return; } if ($f === undefined) { $f = { $blk: deriv.ptr.prototype.Sderiv }; } $f.$ptr = $ptr; $f._arg = _arg; $f._arg$1 = _arg$1; $f._r = _r; $f._r$1 = _r$1; $f.c = c; $f.e = e; $f.this$1 = this$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	deriv.prototype.Sderiv = function(e, c) { return this.$val.Sderiv(e, c); };
	deriv.ptr.prototype.Deriv = function(e, c) {
		var $ptr, _1, _2, _arg, _arg$1, _arg$2, _arg$3, _arg$4, _arg$5, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, c, e, expr1, res, this$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _1 = $f._1; _2 = $f._2; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _arg$3 = $f._arg$3; _arg$4 = $f._arg$4; _arg$5 = $f._arg$5; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; c = $f.c; e = $f.e; expr1 = $f.expr1; res = $f.res; this$1 = $f.this$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		c = [c];
		e = [e];
		this$1 = [this$1];
		this$1[0] = this;
		res = new Mem(this$1[0].mem).Get(e[0], c[0]);
		if (!(res === ptrType$1.nil)) {
			return res;
		}
		_r = fmt.Printf("%s deriv %v\n", new sliceType$2([e[0], new $String($runesToString(new sliceType$1([c[0]])))])); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_r;
			_1 = e[0].Type;
			/* */ if (_1 === (ast.Value)) { $s = 3; continue; }
			/* */ if (_1 === (ast.Or)) { $s = 4; continue; }
			/* */ if (_1 === (ast.Concat)) { $s = 5; continue; }
			/* */ if (_1 === (ast.ZeroOrMore)) { $s = 6; continue; }
			/* */ if (_1 === (ast.Reference)) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (_1 === (ast.Value)) { */ case 3:
				_2 = e[0].Value;
				if (_2 === ("empty")) {
					return new Mem(this$1[0].mem).Set(e[0], c[0], emptyset);
				} else if (_2 === ("emptyset")) {
					return new Mem(this$1[0].mem).Set(e[0], c[0], emptyset);
				} else {
					if (e[0].Value === $runesToString(new sliceType$1([c[0]]))) {
						return new Mem(this$1[0].mem).Set(e[0], c[0], empty);
					}
					return new Mem(this$1[0].mem).Set(e[0], c[0], emptyset);
				}
				$s = 8; continue;
			/* } else if (_1 === (ast.Or)) { */ case 4:
				/* */ $s = 9; case 9:
				return new Mem(this$1[0].mem).Set(e[0], c[0], new ast.Expr.ptr(ast.Or, ast.NewLazy((function(c, e, this$1) { return function $b() {
					var $ptr, _r$1, _r$2, $s, $r;
					/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
					_r$1 = e[0].Expr.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
					_r$2 = this$1[0].Sderiv(_r$1, c[0]); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					/* */ $s = 3; case 3:
					return _r$2;
					/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.$s = $s; $f.$r = $r; return $f;
				}; })(c, e, this$1)), ast.NewLazy((function(c, e, this$1) { return function $b() {
					var $ptr, _r$1, _r$2, $s, $r;
					/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$1 = $f._r$1; _r$2 = $f._r$2; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
					_r$1 = e[0].Expr2.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
					_r$2 = this$1[0].Sderiv(_r$1, c[0]); /* */ $s = 2; case 2: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					/* */ $s = 3; case 3:
					return _r$2;
					/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$1 = _r$1; $f._r$2 = _r$2; $f.$s = $s; $f.$r = $r; return $f;
				}; })(c, e, this$1)), ""));
			/* } else if (_1 === (ast.Concat)) { */ case 5:
				_r$1 = Nullable(this$1[0].defs, e[0].Expr); /* */ $s = 13; case 13: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				/* */ if (_r$1) { $s = 10; continue; }
				/* */ $s = 11; continue;
				/* if (_r$1) { */ case 10:
					return new Mem(this$1[0].mem).Set(e[0], c[0], new ast.Expr.ptr(ast.Or, ast.NewExpr(new ast.Expr.ptr(ast.Concat, ast.NewLazy((function(c, e, this$1) { return function $b() {
						var $ptr, _r$2, _r$3, $s, $r;
						/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
						_r$2 = e[0].Expr.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
						_r$3 = this$1[0].Sderiv(_r$2, c[0]); /* */ $s = 2; case 2: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
						/* */ $s = 3; case 3:
						return _r$3;
						/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.$s = $s; $f.$r = $r; return $f;
					}; })(c, e, this$1)), e[0].Expr2, "")), ast.NewLazy((function(c, e, this$1) { return function $b() {
						var $ptr, _r$2, _r$3, $s, $r;
						/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
						_r$2 = e[0].Expr2.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
						_r$3 = this$1[0].Sderiv(_r$2, c[0]); /* */ $s = 2; case 2: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
						/* */ $s = 3; case 3:
						return _r$3;
						/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.$s = $s; $f.$r = $r; return $f;
					}; })(c, e, this$1)), ""));
				/* } else { */ case 11:
					return new Mem(this$1[0].mem).Set(e[0], c[0], new ast.Expr.ptr(ast.Concat, ast.NewLazy((function(c, e, this$1) { return function $b() {
						var $ptr, _r$2, _r$3, $s, $r;
						/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
						_r$2 = e[0].Expr.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
						_r$3 = this$1[0].Sderiv(_r$2, c[0]); /* */ $s = 2; case 2: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
						/* */ $s = 3; case 3:
						return _r$3;
						/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.$s = $s; $f.$r = $r; return $f;
					}; })(c, e, this$1)), e[0].Expr2, ""));
				/* } */ case 12:
				$s = 8; continue;
			/* } else if (_1 === (ast.ZeroOrMore)) { */ case 6:
				_arg = e[0];
				_arg$1 = c[0];
				_r$2 = Simplify(this$1[0].defs, new ast.Expr.ptr(ast.ZeroOrMore, e[0].Expr, ptrType.nil, "")); /* */ $s = 14; case 14: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_r$3 = ast.NewExpr(_r$2); /* */ $s = 15; case 15: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				_arg$2 = new ast.Expr.ptr(ast.Concat, ast.NewLazy((function(c, e, this$1) { return function $b() {
					var $ptr, _r$2, _r$3, $s, $r;
					/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r$2 = $f._r$2; _r$3 = $f._r$3; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
					_r$2 = e[0].Expr.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
					_r$3 = this$1[0].Sderiv(_r$2, c[0]); /* */ $s = 2; case 2: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
					/* */ $s = 3; case 3:
					return _r$3;
					/* */ } return; } if ($f === undefined) { $f = { $blk: $b }; } $f.$ptr = $ptr; $f._r$2 = _r$2; $f._r$3 = _r$3; $f.$s = $s; $f.$r = $r; return $f;
				}; })(c, e, this$1)), _r$3, "");
				_r$4 = new Mem(this$1[0].mem).Set(_arg, _arg$1, _arg$2); /* */ $s = 16; case 16: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				/* */ $s = 17; case 17:
				return _r$4;
			/* } else if (_1 === (ast.Reference)) { */ case 7:
				_r$5 = lookup(this$1[0].defs, e[0].Value).Expr(); /* */ $s = 18; case 18: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
				expr1 = _r$5;
				_arg$3 = e[0];
				_arg$4 = c[0];
				_r$6 = this$1[0].Deriv(expr1, c[0]); /* */ $s = 19; case 19: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
				_arg$5 = _r$6;
				_r$7 = new Mem(this$1[0].mem).Set(_arg$3, _arg$4, _arg$5); /* */ $s = 20; case 20: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
				/* */ $s = 21; case 21:
				return _r$7;
			/* } */ case 8:
		case 2:
		$panic(new $String("unreachable"));
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: deriv.ptr.prototype.Deriv }; } $f.$ptr = $ptr; $f._1 = _1; $f._2 = _2; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._arg$3 = _arg$3; $f._arg$4 = _arg$4; $f._arg$5 = _arg$5; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f.c = c; $f.e = e; $f.expr1 = expr1; $f.res = res; $f.this$1 = this$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	deriv.prototype.Deriv = function(e, c) { return this.$val.Deriv(e, c); };
	lookup = function(defs, name) {
		var $ptr, _i, _ref, def, defs, name;
		_ref = defs;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			def = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			if (def.Name === name) {
				return def.Expr;
			}
			_i++;
		}
		return ptrType.nil;
	};
	nullable.ptr.prototype.Nullable = function(l) {
		var $ptr, _3, _4, _entry, _entry$1, _key, _key$1, _key$2, _key$3, _key$4, _key$5, _key$6, _key$7, _key$8, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _r$6, _tuple, _v, _v$1, e, l, m, ok, this$1, v, v$1, v$2, v$3, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _3 = $f._3; _4 = $f._4; _entry = $f._entry; _entry$1 = $f._entry$1; _key = $f._key; _key$1 = $f._key$1; _key$2 = $f._key$2; _key$3 = $f._key$3; _key$4 = $f._key$4; _key$5 = $f._key$5; _key$6 = $f._key$6; _key$7 = $f._key$7; _key$8 = $f._key$8; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _tuple = $f._tuple; _v = $f._v; _v$1 = $f._v$1; e = $f.e; l = $f.l; m = $f.m; ok = $f.ok; this$1 = $f.this$1; v = $f.v; v$1 = $f.v$1; v$2 = $f.v$2; v$3 = $f.v$3; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		this$1 = this;
		_tuple = (_entry = this$1.mem[ptrType.keyFor(l)], _entry !== undefined ? [_entry.v, true] : [false, false]);
		m = _tuple[0];
		ok = _tuple[1];
		if (ok) {
			return m;
		}
		v = (_entry$1 = this$1.visited[ptrType.keyFor(l)], _entry$1 !== undefined ? _entry$1.v : false);
		if (v) {
			_key = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key)] = { k: _key, v: false };
			return false;
		}
		_key$1 = l; (this$1.visited || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$1)] = { k: _key$1, v: true };
		_r = l.Expr(); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		e = _r;
			_3 = e.Type;
			/* */ if (_3 === (ast.Value)) { $s = 3; continue; }
			/* */ if (_3 === (ast.Or)) { $s = 4; continue; }
			/* */ if (_3 === (ast.Concat)) { $s = 5; continue; }
			/* */ if (_3 === (ast.ZeroOrMore)) { $s = 6; continue; }
			/* */ if (_3 === (ast.Reference)) { $s = 7; continue; }
			/* */ $s = 8; continue;
			/* if (_3 === (ast.Value)) { */ case 3:
				_4 = e.Value;
				if (_4 === ("empty")) {
					_key$2 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$2)] = { k: _key$2, v: true };
					return true;
				} else if (_4 === ("emptyset")) {
					_key$3 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$3)] = { k: _key$3, v: false };
					return false;
				} else {
					_key$4 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$4)] = { k: _key$4, v: false };
					return false;
				}
				$s = 8; continue;
			/* } else if (_3 === (ast.Or)) { */ case 4:
				_r$1 = this$1.Nullable(e.Expr); /* */ $s = 10; case 10: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				if (_r$1) { _v = true; $s = 9; continue s; }
				_r$2 = this$1.Nullable(e.Expr2); /* */ $s = 11; case 11: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_v = _r$2; case 9:
				v$1 = _v;
				_key$5 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$5)] = { k: _key$5, v: v$1 };
				return v$1;
			/* } else if (_3 === (ast.Concat)) { */ case 5:
				_r$3 = this$1.Nullable(e.Expr); /* */ $s = 13; case 13: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				if (!(_r$3)) { _v$1 = false; $s = 12; continue s; }
				_r$4 = this$1.Nullable(e.Expr2); /* */ $s = 14; case 14: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				_v$1 = _r$4; case 12:
				v$2 = _v$1;
				_key$6 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$6)] = { k: _key$6, v: v$2 };
				return v$2;
			/* } else if (_3 === (ast.ZeroOrMore)) { */ case 6:
				_key$7 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$7)] = { k: _key$7, v: true };
				return true;
			/* } else if (_3 === (ast.Reference)) { */ case 7:
				_r$5 = this$1.Nullable(lookup(this$1.defs, e.Value)); /* */ $s = 15; case 15: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
				v$3 = _r$5;
				_key$8 = l; (this$1.mem || $throwRuntimeError("assignment to entry in nil map"))[ptrType.keyFor(_key$8)] = { k: _key$8, v: v$3 };
				return v$3;
			/* } */ case 8:
		case 2:
		_r$6 = fmt.Sprintf("unreachable %#v", new sliceType$2([e])); /* */ $s = 16; case 16: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
		$panic(new $String(_r$6));
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: nullable.ptr.prototype.Nullable }; } $f.$ptr = $ptr; $f._3 = _3; $f._4 = _4; $f._entry = _entry; $f._entry$1 = _entry$1; $f._key = _key; $f._key$1 = _key$1; $f._key$2 = _key$2; $f._key$3 = _key$3; $f._key$4 = _key$4; $f._key$5 = _key$5; $f._key$6 = _key$6; $f._key$7 = _key$7; $f._key$8 = _key$8; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._tuple = _tuple; $f._v = _v; $f._v$1 = _v$1; $f.e = e; $f.l = l; $f.m = m; $f.ok = ok; $f.this$1 = this$1; $f.v = v; $f.v$1 = v$1; $f.v$2 = v$2; $f.v$3 = v$3; $f.$s = $s; $f.$r = $r; return $f;
	};
	nullable.prototype.Nullable = function(l) { return this.$val.Nullable(l); };
	Nullable = function(defs, l) {
		var $ptr, _r, defs, l, n, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; defs = $f.defs; l = $f.l; n = $f.n; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		n = new nullable.ptr(defs, {}, {});
		_r = n.Nullable(l); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: Nullable }; } $f.$ptr = $ptr; $f._r = _r; $f.defs = defs; $f.l = l; $f.n = n; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Nullable = Nullable;
	isEmptySet = function(e) {
		var $ptr, e;
		return (e.Type === ast.Value) && e.Value === "emptyset";
	};
	isEmpty = function(e) {
		var $ptr, e;
		return (e.Type === ast.Value) && e.Value === "empty";
	};
	Simplify = function(defs, e) {
		var $ptr, _5, _arg, _arg$1, _arg$2, _arg$3, _arg$4, _arg$5, _arg$6, _arg$7, _arg$8, _arg$9, _r, _r$1, _r$10, _r$2, _r$3, _r$4, _r$5, _r$6, _r$7, _r$8, _r$9, defs, e, expr1, expr1$1, expr1$2, expr2, expr2$1, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _5 = $f._5; _arg = $f._arg; _arg$1 = $f._arg$1; _arg$2 = $f._arg$2; _arg$3 = $f._arg$3; _arg$4 = $f._arg$4; _arg$5 = $f._arg$5; _arg$6 = $f._arg$6; _arg$7 = $f._arg$7; _arg$8 = $f._arg$8; _arg$9 = $f._arg$9; _r = $f._r; _r$1 = $f._r$1; _r$10 = $f._r$10; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _r$6 = $f._r$6; _r$7 = $f._r$7; _r$8 = $f._r$8; _r$9 = $f._r$9; defs = $f.defs; e = $f.e; expr1 = $f.expr1; expr1$1 = $f.expr1$1; expr1$2 = $f.expr1$2; expr2 = $f.expr2; expr2$1 = $f.expr2$1; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
			_5 = e.Type;
			/* */ if (_5 === (ast.Value)) { $s = 2; continue; }
			/* */ if (_5 === (ast.Or)) { $s = 3; continue; }
			/* */ if (_5 === (ast.Concat)) { $s = 4; continue; }
			/* */ if (_5 === (ast.ZeroOrMore)) { $s = 5; continue; }
			/* */ if (_5 === (ast.Reference)) { $s = 6; continue; }
			/* */ $s = 7; continue;
			/* if (_5 === (ast.Value)) { */ case 2:
				return e;
			/* } else if (_5 === (ast.Or)) { */ case 3:
				if (e.Expr.IsLazy() || e.Expr2.IsLazy()) {
					return e;
				}
				_arg = defs;
				_r = e.Expr.Expr(); /* */ $s = 8; case 8: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
				_arg$1 = _r;
				_r$1 = Simplify(_arg, _arg$1); /* */ $s = 9; case 9: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
				expr1 = _r$1;
				_arg$2 = defs;
				_r$2 = e.Expr2.Expr(); /* */ $s = 10; case 10: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
				_arg$3 = _r$2;
				_r$3 = Simplify(_arg$2, _arg$3); /* */ $s = 11; case 11: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
				expr2 = _r$3;
				if (isEmptySet(expr1)) {
					return expr2;
				}
				if (isEmptySet(expr2)) {
					return expr1;
				}
				return new ast.Expr.ptr(ast.Or, ast.NewExpr(expr1), ast.NewExpr(expr2), "");
			/* } else if (_5 === (ast.Concat)) { */ case 4:
				if (e.Expr.IsLazy() || e.Expr2.IsLazy()) {
					return e;
				}
				_arg$4 = defs;
				_r$4 = e.Expr.Expr(); /* */ $s = 12; case 12: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
				_arg$5 = _r$4;
				_r$5 = Simplify(_arg$4, _arg$5); /* */ $s = 13; case 13: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
				expr1$1 = _r$5;
				_arg$6 = defs;
				_r$6 = e.Expr2.Expr(); /* */ $s = 14; case 14: if($c) { $c = false; _r$6 = _r$6.$blk(); } if (_r$6 && _r$6.$blk !== undefined) { break s; }
				_arg$7 = _r$6;
				_r$7 = Simplify(_arg$6, _arg$7); /* */ $s = 15; case 15: if($c) { $c = false; _r$7 = _r$7.$blk(); } if (_r$7 && _r$7.$blk !== undefined) { break s; }
				expr2$1 = _r$7;
				if (isEmptySet(expr1$1)) {
					return emptyset;
				}
				if (isEmpty(expr1$1)) {
					return expr2$1;
				}
				if (isEmptySet(expr2$1)) {
					return emptyset;
				}
				return new ast.Expr.ptr(ast.Concat, ast.NewExpr(expr1$1), ast.NewExpr(expr2$1), "");
			/* } else if (_5 === (ast.ZeroOrMore)) { */ case 5:
				if (e.Expr.IsLazy()) {
					return e;
				}
				_arg$8 = defs;
				_r$8 = e.Expr.Expr(); /* */ $s = 16; case 16: if($c) { $c = false; _r$8 = _r$8.$blk(); } if (_r$8 && _r$8.$blk !== undefined) { break s; }
				_arg$9 = _r$8;
				_r$9 = Simplify(_arg$8, _arg$9); /* */ $s = 17; case 17: if($c) { $c = false; _r$9 = _r$9.$blk(); } if (_r$9 && _r$9.$blk !== undefined) { break s; }
				expr1$2 = _r$9;
				if (isEmptySet(expr1$2)) {
					return empty;
				}
				return new ast.Expr.ptr(ast.ZeroOrMore, ast.NewExpr(expr1$2), ptrType.nil, "");
			/* } else if (_5 === (ast.Reference)) { */ case 6:
				return e;
			/* } */ case 7:
		case 1:
		_r$10 = fmt.Sprintf("unreachable %#v", new sliceType$2([e])); /* */ $s = 18; case 18: if($c) { $c = false; _r$10 = _r$10.$blk(); } if (_r$10 && _r$10.$blk !== undefined) { break s; }
		$panic(new $String(_r$10));
		/* */ $s = -1; case -1: } return; } if ($f === undefined) { $f = { $blk: Simplify }; } $f.$ptr = $ptr; $f._5 = _5; $f._arg = _arg; $f._arg$1 = _arg$1; $f._arg$2 = _arg$2; $f._arg$3 = _arg$3; $f._arg$4 = _arg$4; $f._arg$5 = _arg$5; $f._arg$6 = _arg$6; $f._arg$7 = _arg$7; $f._arg$8 = _arg$8; $f._arg$9 = _arg$9; $f._r = _r; $f._r$1 = _r$1; $f._r$10 = _r$10; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._r$6 = _r$6; $f._r$7 = _r$7; $f._r$8 = _r$8; $f._r$9 = _r$9; $f.defs = defs; $f.e = e; $f.expr1 = expr1; $f.expr1$1 = expr1$1; $f.expr1$2 = expr1$2; $f.expr2 = expr2; $f.expr2$1 = expr2$1; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Simplify = Simplify;
	Mem.methods = [{prop: "Get", name: "Get", pkg: "", typ: $funcType([ptrType$1, $Int32], [ptrType$1], false)}, {prop: "Set", name: "Set", pkg: "", typ: $funcType([ptrType$1, $Int32, ptrType$1], [ptrType$1], false)}];
	ptrType$2.methods = [{prop: "Sderiv", name: "Sderiv", pkg: "", typ: $funcType([ptrType$1, $Int32], [ptrType$1], false)}, {prop: "Deriv", name: "Deriv", pkg: "", typ: $funcType([ptrType$1, $Int32], [ptrType$1], false)}];
	ptrType$3.methods = [{prop: "Nullable", name: "Nullable", pkg: "", typ: $funcType([ptrType], [$Bool], false)}];
	Mem.init(ptrType$1, mapType);
	deriv.init([{prop: "mem", name: "mem", pkg: "github.com/awalterschulze/ragax/golang/deriv", typ: Mem, tag: ""}, {prop: "defs", name: "defs", pkg: "github.com/awalterschulze/ragax/golang/deriv", typ: ast.Defs, tag: ""}]);
	nullable.init([{prop: "defs", name: "defs", pkg: "github.com/awalterschulze/ragax/golang/deriv", typ: ast.Defs, tag: ""}, {prop: "mem", name: "mem", pkg: "github.com/awalterschulze/ragax/golang/deriv", typ: mapType$1, tag: ""}, {prop: "visited", name: "visited", pkg: "github.com/awalterschulze/ragax/golang/deriv", typ: mapType$1, tag: ""}]);
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = fmt.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = ast.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = lexer.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = parser.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		emptyset = new ast.Expr.ptr(ast.Value, ptrType.nil, ptrType.nil, "emptyset");
		empty = new ast.Expr.ptr(ast.Value, ptrType.nil, ptrType.nil, "empty");
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$packages["main"] = (function() {
	var $pkg = {}, $init, fmt, ast, deriv, js, funcType, sliceType, funcType$1, mapType, sliceType$1, sliceType$2, main, midiToNote, midiToNotes, Eval, EvalToEmptySet, EvalToEmptySetNotes;
	fmt = $packages["fmt"];
	ast = $packages["github.com/awalterschulze/ragax/golang/ast"];
	deriv = $packages["github.com/awalterschulze/ragax/golang/deriv"];
	js = $packages["github.com/gopherjs/gopherjs/js"];
	funcType = $funcType([$String, $String], [$String], false);
	sliceType = $sliceType($Int64);
	funcType$1 = $funcType([$String, sliceType], [$String], false);
	mapType = $mapType($String, $emptyInterface);
	sliceType$1 = $sliceType($Int32);
	sliceType$2 = $sliceType($emptyInterface);
	main = function() {
		var $ptr;
		$global.gofunctions = $externalize($makeMap($String.keyFor, [{ k: "EvalToEmptySet", v: new funcType(EvalToEmptySet) }, { k: "EvalToEmptySetNotes", v: new funcType$1(EvalToEmptySetNotes) }, { k: "Eval", v: new funcType(Eval) }]), mapType);
	};
	midiToNote = function(v) {
		var $ptr, _1, v, x, x$1, x$10, x$11, x$2, x$3, x$4, x$5, x$6, x$7, x$8, x$9;
		_1 = $div64(v, new $Int64(0, 12), true);
		if ((x = new $Int64(0, 0), (_1.$high === x.$high && _1.$low === x.$low))) {
			return 83;
		} else if ((x$1 = new $Int64(0, 1), (_1.$high === x$1.$high && _1.$low === x$1.$low))) {
			return 114;
		} else if ((x$2 = new $Int64(0, 2), (_1.$high === x$2.$high && _1.$low === x$2.$low))) {
			return 82;
		} else if ((x$3 = new $Int64(0, 3), (_1.$high === x$3.$high && _1.$low === x$3.$low))) {
			return 103;
		} else if ((x$4 = new $Int64(0, 4), (_1.$high === x$4.$high && _1.$low === x$4.$low))) {
			return 71;
		} else if ((x$5 = new $Int64(0, 5), (_1.$high === x$5.$high && _1.$low === x$5.$low))) {
			return 109;
		} else if ((x$6 = new $Int64(0, 6), (_1.$high === x$6.$high && _1.$low === x$6.$low))) {
			return 77;
		} else if ((x$7 = new $Int64(0, 7), (_1.$high === x$7.$high && _1.$low === x$7.$low))) {
			return 80;
		} else if ((x$8 = new $Int64(0, 8), (_1.$high === x$8.$high && _1.$low === x$8.$low))) {
			return 100;
		} else if ((x$9 = new $Int64(0, 9), (_1.$high === x$9.$high && _1.$low === x$9.$low))) {
			return 68;
		} else if ((x$10 = new $Int64(0, 10), (_1.$high === x$10.$high && _1.$low === x$10.$low))) {
			return 110;
		} else if ((x$11 = new $Int64(0, 11), (_1.$high === x$11.$high && _1.$low === x$11.$low))) {
			return 78;
		}
		$panic(new $String("unreachable"));
	};
	midiToNotes = function(inputs) {
		var $ptr, _i, _ref, i, input, inputs, rs;
		rs = $makeSlice(sliceType$1, inputs.$length);
		_ref = inputs;
		_i = 0;
		while (true) {
			if (!(_i < _ref.$length)) { break; }
			i = _i;
			input = ((_i < 0 || _i >= _ref.$length) ? $throwRuntimeError("index out of range") : _ref.$array[_ref.$offset + _i]);
			((i < 0 || i >= rs.$length) ? $throwRuntimeError("index out of range") : rs.$array[rs.$offset + i] = midiToNote(input));
			_i++;
		}
		return $runesToString(rs);
	};
	Eval = function(g, input) {
		var $ptr, _r, _r$1, _r$2, _r$3, _tuple, defs, err, g, input, res, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _tuple = $f._tuple; defs = $f.defs; err = $f.err; g = $f.g; input = $f.input; res = $f.res; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = deriv.Parse(g); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		defs = _tuple[0];
		err = _tuple[1];
		/* */ if (!($interfaceIsEqual(err, $ifaceNil))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!($interfaceIsEqual(err, $ifaceNil))) { */ case 2:
			_r$1 = err.Error(); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			/* */ $s = 5; case 5:
			return _r$1;
		/* } */ case 3:
		_r$2 = deriv.Eval(defs, input); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		res = _r$2;
		_r$3 = deriv.Nullable(defs, ast.NewExpr(res)); /* */ $s = 9; case 9: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		/* */ if (_r$3) { $s = 7; continue; }
		/* */ $s = 8; continue;
		/* if (_r$3) { */ case 7:
			return "true";
		/* } */ case 8:
		return "false";
		/* */ } return; } if ($f === undefined) { $f = { $blk: Eval }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._tuple = _tuple; $f.defs = defs; $f.err = err; $f.g = g; $f.input = input; $f.res = res; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.Eval = Eval;
	EvalToEmptySet = function(g, input) {
		var $ptr, _r, _r$1, _r$2, _r$3, _r$4, _r$5, _tuple, defs, err, g, input, res, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; _r$1 = $f._r$1; _r$2 = $f._r$2; _r$3 = $f._r$3; _r$4 = $f._r$4; _r$5 = $f._r$5; _tuple = $f._tuple; defs = $f.defs; err = $f.err; g = $f.g; input = $f.input; res = $f.res; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		_r = deriv.Parse(g); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		_tuple = _r;
		defs = _tuple[0];
		err = _tuple[1];
		/* */ if (!($interfaceIsEqual(err, $ifaceNil))) { $s = 2; continue; }
		/* */ $s = 3; continue;
		/* if (!($interfaceIsEqual(err, $ifaceNil))) { */ case 2:
			_r$1 = err.Error(); /* */ $s = 4; case 4: if($c) { $c = false; _r$1 = _r$1.$blk(); } if (_r$1 && _r$1.$blk !== undefined) { break s; }
			/* */ $s = 5; case 5:
			return _r$1;
		/* } */ case 3:
		_r$2 = deriv.Eval(defs, input); /* */ $s = 6; case 6: if($c) { $c = false; _r$2 = _r$2.$blk(); } if (_r$2 && _r$2.$blk !== undefined) { break s; }
		res = _r$2;
		_r$3 = deriv.Nullable(defs, ast.NewExpr(res)); /* */ $s = 7; case 7: if($c) { $c = false; _r$3 = _r$3.$blk(); } if (_r$3 && _r$3.$blk !== undefined) { break s; }
		_r$3;
		_r$4 = deriv.Simplify(defs, res); /* */ $s = 8; case 8: if($c) { $c = false; _r$4 = _r$4.$blk(); } if (_r$4 && _r$4.$blk !== undefined) { break s; }
		res = _r$4;
		_r$5 = fmt.Printf("final = %s\n", new sliceType$2([res])); /* */ $s = 9; case 9: if($c) { $c = false; _r$5 = _r$5.$blk(); } if (_r$5 && _r$5.$blk !== undefined) { break s; }
		_r$5;
		if ((res.Type === ast.Value) && res.Value === "emptyset") {
			return "true";
		}
		return "false";
		/* */ } return; } if ($f === undefined) { $f = { $blk: EvalToEmptySet }; } $f.$ptr = $ptr; $f._r = _r; $f._r$1 = _r$1; $f._r$2 = _r$2; $f._r$3 = _r$3; $f._r$4 = _r$4; $f._r$5 = _r$5; $f._tuple = _tuple; $f.defs = defs; $f.err = err; $f.g = g; $f.input = input; $f.res = res; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.EvalToEmptySet = EvalToEmptySet;
	EvalToEmptySetNotes = function(g, is) {
		var $ptr, _r, g, input, is, $s, $r;
		/* */ $s = 0; var $f, $c = false; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $ptr = $f.$ptr; _r = $f._r; g = $f.g; input = $f.input; is = $f.is; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		input = midiToNotes(is);
		_r = EvalToEmptySet(g, input); /* */ $s = 1; case 1: if($c) { $c = false; _r = _r.$blk(); } if (_r && _r.$blk !== undefined) { break s; }
		/* */ $s = 2; case 2:
		return _r;
		/* */ } return; } if ($f === undefined) { $f = { $blk: EvalToEmptySetNotes }; } $f.$ptr = $ptr; $f._r = _r; $f.g = g; $f.input = input; $f.is = is; $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.EvalToEmptySetNotes = EvalToEmptySetNotes;
	$init = function() {
		$pkg.$init = function() {};
		/* */ var $f, $c = false, $s = 0, $r; if (this !== undefined && this.$blk !== undefined) { $f = this; $c = true; $s = $f.$s; $r = $f.$r; } s: while (true) { switch ($s) { case 0:
		$r = fmt.$init(); /* */ $s = 1; case 1: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = ast.$init(); /* */ $s = 2; case 2: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = deriv.$init(); /* */ $s = 3; case 3: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		$r = js.$init(); /* */ $s = 4; case 4: if($c) { $c = false; $r = $r.$blk(); } if ($r && $r.$blk !== undefined) { break s; }
		if ($pkg === $mainPkg) {
			main();
			$mainFinished = true;
		}
		/* */ } return; } if ($f === undefined) { $f = { $blk: $init }; } $f.$s = $s; $f.$r = $r; return $f;
	};
	$pkg.$init = $init;
	return $pkg;
})();
$synthesizeMethods();
var $mainPkg = $packages["main"];
$packages["runtime"].$init();
$go($mainPkg.$init, [], true);
$flushConsole();

}).call(this);
//# sourceMappingURL=main.js.map
