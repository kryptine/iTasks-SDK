function $w(e) {
    return e = e.strip(), e ? e.split(/\s+/) : [];
}

function $H(e) {
    return e instanceof Hash ? e : new Hash(e);
}

function $(e) {
    if (arguments.length > 1) {
        for (var t = 0, n = [], i = arguments.length; i > t; t++) n.push($(arguments[t]));
        return n;
    }
    return "string" == typeof e && (e = document.getElementById(e)), Element.extend(e);
}

function $$() {
    return Selector.findChildElements(document, $A(arguments));
}

function printf() {
    for (var e = arguments[0], t = 1; t < arguments.length; t++) e = e.replace("%" + (t - 1), arguments[t]);
    return e;
}

function init() {
    Ext.BLANK_IMAGE_URL = ORYX.BASE_FILE_PATH + "lib/ext-2.0.2/resources/images/default/s.gif", 
    ORYX.Log.debug("Querying editor instances"), ORYX.Editor.setMissingClasses();
}

function assert(e, t) {
    if (!e) throw t;
}

function DMCommand(e, t) {
    this.action = e, this.triple = t, this.toString = function() {
        return "Command(" + e + ", " + t + ")";
    };
}

function DMCommandHandler(e) {
    this.__setNext = function() {
        var t = this.__next;
        return this.__next = e, t ? t : !0;
    }, this.__setNext(e), this.__invokeNext = function(e) {
        return this.__next ? this.__next.handle(e) : !1;
    }, this.handle = function(e) {
        return this.process(e) ? !0 : this.__invokeNext(e);
    }, this.process = function() {
        return !1;
    };
}

function MetaTagHandler(next) {
    DMCommandHandler.apply(this, [ next ]), this.process = function(command) {
        with (command.triple) if (!(subject instanceof ERDF.Resource && subject.isCurrentDocument() && object instanceof ERDF.Literal)) return !1;
    };
}

function _evenMoreEvilHack(e, t) {
    if (window.ActiveXObject) {
        var n = new ActiveXObject("MSXML.DomDocument");
        return n.loadXML(e), n;
    }
    if (window.XMLHttpRequest) {
        var i = new XMLHttpRequest();
        return i.open("GET", "data:" + (t || "application/xml") + ";charset=utf-8," + encodeURIComponent(e), !1), 
        i.overrideMimeType && i.overrideMimeType(t), i.send(null), i.responseXML;
    }
}

function _evilSafariHack(e) {
    var t = e, n = "data:text/xml;charset=utf-8," + encodeURIComponent(t), i = null, r = new XMLHttpRequest();
    return r.open("GET", n), r.onload = function() {
        i = r.responseXML;
    }, r.send(null), i;
}

function jsonPath(obj, expr, arg) {
    var P = {
        resultType: arg && arg.resultType || "VALUE",
        result: [],
        normalize: function(e) {
            var t = [];
            return e.replace(/[\['](\??\(.*?\))[\]']/g, function(e, n) {
                return "[#" + (t.push(n) - 1) + "]";
            }).replace(/'?\.'?|\['?/g, ";").replace(/;;;|;;/g, ";..;").replace(/;$|'?\]|'$/g, "").replace(/#([0-9]+)/g, function(e, n) {
                return t[n];
            });
        },
        asPath: function(e) {
            for (var t = e.split(";"), n = "$", i = 1, r = t.length; r > i; i++) n += /^[0-9*]+$/.test(t[i]) ? "[" + t[i] + "]" : "['" + t[i] + "']";
            return n;
        },
        store: function(e, t) {
            return e && (P.result[P.result.length] = "PATH" == P.resultType ? P.asPath(e) : t), 
            !!e;
        },
        trace: function(e, t, n) {
            if (e) {
                var i = e.split(";"), r = i.shift();
                if (i = i.join(";"), t && t.hasOwnProperty(r)) P.trace(i, t[r], n + ";" + r); else if ("*" === r) P.walk(r, i, t, n, function(e, t, n, i, r) {
                    P.trace(e + ";" + n, i, r);
                }); else if (".." === r) P.trace(i, t, n), P.walk(r, i, t, n, function(e, t, n, i, r) {
                    "object" == typeof i[e] && P.trace("..;" + n, i[e], r + ";" + e);
                }); else if (/,/.test(r)) for (var s = r.split(/'?,'?/), o = 0, a = s.length; a > o; o++) P.trace(s[o] + ";" + i, t, n); else /^\(.*?\)$/.test(r) ? P.trace(P.eval(r, t, n.substr(n.lastIndexOf(";") + 1)) + ";" + i, t, n) : /^\?\(.*?\)$/.test(r) ? P.walk(r, i, t, n, function(e, t, n, i, r) {
                    P.eval(t.replace(/^\?\((.*?)\)$/, "$1"), i[e], e) && P.trace(e + ";" + n, i, r);
                }) : /^(-?[0-9]*):(-?[0-9]*):?([0-9]*)$/.test(r) && P.slice(r, i, t, n);
            } else P.store(n, t);
        },
        walk: function(e, t, n, i, r) {
            if (n instanceof Array) for (var s = 0, o = n.length; o > s; s++) s in n && r(s, e, t, n, i); else if ("object" == typeof n) for (var a in n) n.hasOwnProperty(a) && r(a, e, t, n, i);
        },
        slice: function(e, t, n, i) {
            if (n instanceof Array) {
                var r = n.length, s = 0, o = r, a = 1;
                e.replace(/^(-?[0-9]*):(-?[0-9]*):?(-?[0-9]*)$/g, function(e, t, n, i) {
                    s = parseInt(t || s), o = parseInt(n || o), a = parseInt(i || a);
                }), s = 0 > s ? Math.max(0, s + r) : Math.min(r, s), o = 0 > o ? Math.max(0, o + r) : Math.min(r, o);
                for (var c = s; o > c; c += a) P.trace(c + ";" + t, n, i);
            }
        },
        eval: function(x, _v, _vname) {
            try {
                return $ && _v && eval(x.replace(/@/g, "_v"));
            } catch (e) {
                throw new SyntaxError("jsonPath: " + e.message + ": " + x.replace(/@/g, "_v").replace(/\^/g, "_a"));
            }
        }
    }, $ = obj;
    return expr && obj && ("VALUE" == P.resultType || "PATH" == P.resultType) ? (P.trace(P.normalize(expr).replace(/^\$;/, ""), obj, "$"), 
    P.result.length ? P.result : !1) : void 0;
}

var Prototype = {
    Version: "1.5.1",
    Browser: {
        IE: !(!window.attachEvent || window.opera),
        Opera: !!window.opera,
        WebKit: navigator.userAgent.indexOf("AppleWebKit/") > -1,
        Gecko: navigator.userAgent.indexOf("Gecko") > -1 && -1 == navigator.userAgent.indexOf("KHTML")
    },
    BrowserFeatures: {
        XPath: !!document.evaluate,
        ElementExtensions: !!window.HTMLElement,
        SpecificElementExtensions: document.createElement("div").__proto__ !== document.createElement("form").__proto__
    },
    ScriptFragment: "<script[^>]*>([-￿]*?)</script>",
    JSONFilter: /^\/\*-secure-\s*(.*)\s*\*\/\s*$/,
    emptyFunction: function() {},
    K: function(e) {
        return e;
    }
}, Class = {
    create: function() {
        return function() {
            this.initialize.apply(this, arguments);
        };
    }
}, Abstract = new Object();

Object.extend = function(e, t) {
    for (var n in t) e[n] = t[n];
    return e;
}, Object.extend(Object, {
    inspect: function(e) {
        try {
            return void 0 === e ? "undefined" : null === e ? "null" : e.inspect ? e.inspect() : e.toString();
        } catch (t) {
            if (t instanceof RangeError) return "...";
            throw t;
        }
    },
    toJSON: function(e) {
        var t = typeof e;
        switch (t) {
          case "undefined":
          case "function":
          case "unknown":
            return;

          case "boolean":
            return e.toString();
        }
        if (null === e) return "null";
        if (e.toJSON) return e.toJSON();
        if (e.ownerDocument !== document) {
            var n = [];
            for (var i in e) {
                var r = Object.toJSON(e[i]);
                void 0 !== r && n.push(i.toJSON() + ": " + r);
            }
            return "{" + n.join(", ") + "}";
        }
    },
    keys: function(e) {
        var t = [];
        for (var n in e) t.push(n);
        return t;
    },
    values: function(e) {
        var t = [];
        for (var n in e) t.push(e[n]);
        return t;
    },
    clone: function(e) {
        return Object.extend({}, e);
    }
}), Function.prototype.bind = function() {
    var e = this, t = $A(arguments), n = t.shift();
    return function() {
        return e.apply(n, t.concat($A(arguments)));
    };
}, Function.prototype.bindAsEventListener = function(e) {
    var t = this, n = $A(arguments), e = n.shift();
    return function(i) {
        return t.apply(e, [ i || window.event ].concat(n));
    };
}, Object.extend(Number.prototype, {
    toColorPart: function() {
        return this.toPaddedString(2, 16);
    },
    succ: function() {
        return this + 1;
    },
    times: function(e) {
        return $R(0, this, !0).each(e), this;
    },
    toPaddedString: function(e, t) {
        var n = this.toString(t || 10);
        return "0".times(e - n.length) + n;
    },
    toJSON: function() {
        return isFinite(this) ? this.toString() : "null";
    }
}), Date.prototype.toJSON = function() {
    return '"' + this.getFullYear() + "-" + (this.getMonth() + 1).toPaddedString(2) + "-" + this.getDate().toPaddedString(2) + "T" + this.getHours().toPaddedString(2) + ":" + this.getMinutes().toPaddedString(2) + ":" + this.getSeconds().toPaddedString(2) + '"';
};

var Try = {
    these: function() {
        for (var e, t = 0, n = arguments.length; n > t; t++) {
            var i = arguments[t];
            try {
                e = i();
                break;
            } catch (r) {}
        }
        return e;
    }
}, PeriodicalExecuter = Class.create();

with (PeriodicalExecuter.prototype = {
    initialize: function(e, t) {
        this.callback = e, this.frequency = t, this.currentlyExecuting = !1, this.registerCallback();
    },
    registerCallback: function() {
        this.timer = setInterval(this.onTimerEvent.bind(this), 1e3 * this.frequency);
    },
    stop: function() {
        this.timer && (clearInterval(this.timer), this.timer = null);
    },
    onTimerEvent: function() {
        if (!this.currentlyExecuting) try {
            this.currentlyExecuting = !0, this.callback(this);
        } finally {
            this.currentlyExecuting = !1;
        }
    }
}, Object.extend(String, {
    interpret: function(e) {
        return null == e ? "" : String(e);
    },
    specialChar: {
        "\b": "\\b",
        "	": "\\t",
        "\n": "\\n",
        "\f": "\\f",
        "\r": "\\r",
        "\\": "\\\\"
    }
}), Object.extend(String.prototype, {
    gsub: function(e, t) {
        var n, i = "", r = this;
        for (t = arguments.callee.prepareReplacement(t); r.length > 0; ) (n = r.match(e)) ? (i += r.slice(0, n.index), 
        i += String.interpret(t(n)), r = r.slice(n.index + n[0].length)) : (i += r, r = "");
        return i;
    },
    sub: function(e, t, n) {
        return t = this.gsub.prepareReplacement(t), n = void 0 === n ? 1 : n, this.gsub(e, function(e) {
            return --n < 0 ? e[0] : t(e);
        });
    },
    scan: function(e, t) {
        return this.gsub(e, t), this;
    },
    truncate: function(e, t) {
        return e = e || 30, t = void 0 === t ? "..." : t, this.length > e ? this.slice(0, e - t.length) + t : this;
    },
    strip: function() {
        return this.replace(/^\s+/, "").replace(/\s+$/, "");
    },
    stripTags: function() {
        return this.replace(/<\/?[^>]+>/gi, "");
    },
    stripScripts: function() {
        return this.replace(new RegExp(Prototype.ScriptFragment, "img"), "");
    },
    extractScripts: function() {
        var e = new RegExp(Prototype.ScriptFragment, "img"), t = new RegExp(Prototype.ScriptFragment, "im");
        return (this.match(e) || []).map(function(e) {
            return (e.match(t) || [ "", "" ])[1];
        });
    },
    evalScripts: function() {
        return this.extractScripts().map(function(script) {
            return eval(script);
        });
    },
    escapeHTML: function() {
        var e = arguments.callee;
        return e.text.data = this, e.div.innerHTML;
    },
    unescapeHTML: function() {
        var e = document.createElement("div");
        return e.innerHTML = this.stripTags(), e.childNodes[0] ? e.childNodes.length > 1 ? $A(e.childNodes).inject("", function(e, t) {
            return e + t.nodeValue;
        }) : e.childNodes[0].nodeValue : "";
    },
    toQueryParams: function(e) {
        var t = this.strip().match(/([^?#]*)(#.*)?$/);
        return t ? t[1].split(e || "&").inject({}, function(e, t) {
            if ((t = t.split("="))[0]) {
                var n = decodeURIComponent(t.shift()), i = t.length > 1 ? t.join("=") : t[0];
                void 0 != i && (i = decodeURIComponent(i)), n in e ? (e[n].constructor != Array && (e[n] = [ e[n] ]), 
                e[n].push(i)) : e[n] = i;
            }
            return e;
        }) : {};
    },
    toArray: function() {
        return this.split("");
    },
    succ: function() {
        return this.slice(0, this.length - 1) + String.fromCharCode(this.charCodeAt(this.length - 1) + 1);
    },
    times: function(e) {
        for (var t = "", n = 0; e > n; n++) t += this;
        return t;
    },
    camelize: function() {
        var e = this.split("-"), t = e.length;
        if (1 == t) return e[0];
        for (var n = "-" == this.charAt(0) ? e[0].charAt(0).toUpperCase() + e[0].substring(1) : e[0], i = 1; t > i; i++) n += e[i].charAt(0).toUpperCase() + e[i].substring(1);
        return n;
    },
    capitalize: function() {
        return this.charAt(0).toUpperCase() + this.substring(1).toLowerCase();
    },
    underscore: function() {
        return this.gsub(/::/, "/").gsub(/([A-Z]+)([A-Z][a-z])/, "#{1}_#{2}").gsub(/([a-z\d])([A-Z])/, "#{1}_#{2}").gsub(/-/, "_").toLowerCase();
    },
    dasherize: function() {
        return this.gsub(/_/, "-");
    },
    inspect: function(e) {
        var t = this.gsub(/[\x00-\x1f\\]/, function(e) {
            var t = String.specialChar[e[0]];
            return t ? t : "\\u00" + e[0].charCodeAt().toPaddedString(2, 16);
        });
        return e ? '"' + t.replace(/"/g, '\\"') + '"' : "'" + t.replace(/'/g, "\\'") + "'";
    },
    toJSON: function() {
        return this.inspect(!0);
    },
    unfilterJSON: function(e) {
        return this.sub(e || Prototype.JSONFilter, "#{1}");
    },
    evalJSON: function(sanitize) {
        var json = this.unfilterJSON();
        try {
            if (!sanitize || /^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.test(json)) return eval("(" + json + ")");
        } catch (e) {}
        throw new SyntaxError("Badly formed JSON string: " + this.inspect());
    },
    include: function(e) {
        return this.indexOf(e) > -1;
    },
    startsWith: function(e) {
        return 0 === this.indexOf(e);
    },
    endsWith: function(e) {
        var t = this.length - e.length;
        return t >= 0 && this.lastIndexOf(e) === t;
    },
    empty: function() {
        return "" == this;
    },
    blank: function() {
        return /^\s*$/.test(this);
    }
}), (Prototype.Browser.WebKit || Prototype.Browser.IE) && Object.extend(String.prototype, {
    escapeHTML: function() {
        return this.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
    },
    unescapeHTML: function() {
        return this.replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">");
    }
}), String.prototype.gsub.prepareReplacement = function(e) {
    if ("function" == typeof e) return e;
    var t = new Template(e);
    return function(e) {
        return t.evaluate(e);
    };
}, String.prototype.parseQuery = String.prototype.toQueryParams, Object.extend(String.prototype.escapeHTML, {
    div: document.createElement("div"),
    text: document.createTextNode("")
}), String.prototype.escapeHTML) div.appendChild(text);

var Template = Class.create();

Template.Pattern = /(^|.|\r|\n)(#\{(.*?)\})/, Template.prototype = {
    initialize: function(e, t) {
        this.template = e.toString(), this.pattern = t || Template.Pattern;
    },
    evaluate: function(e) {
        return this.template.gsub(this.pattern, function(t) {
            var n = t[1];
            return "\\" == n ? t[2] : n + String.interpret(e[t[3]]);
        });
    }
};

var $break = {}, $continue = new Error('"throw $continue" is deprecated, use "return" instead'), Enumerable = {
    each: function(e) {
        var t = 0;
        try {
            this._each(function(n) {
                e(n, t++);
            });
        } catch (n) {
            if (n != $break) throw n;
        }
        return this;
    },
    eachSlice: function(e, t) {
        for (var n = -e, i = [], r = this.toArray(); (n += e) < r.length; ) i.push(r.slice(n, n + e));
        return i.map(t);
    },
    all: function(e) {
        var t = !0;
        return this.each(function(n, i) {
            if (t = t && !!(e || Prototype.K)(n, i), !t) throw $break;
        }), t;
    },
    any: function(e) {
        var t = !1;
        return this.each(function(n, i) {
            if (t = !!(e || Prototype.K)(n, i)) throw $break;
        }), t;
    },
    collect: function(e) {
        var t = [];
        return this.each(function(n, i) {
            t.push((e || Prototype.K)(n, i));
        }), t;
    },
    detect: function(e) {
        var t;
        return this.each(function(n, i) {
            if (e(n, i)) throw t = n, $break;
        }), t;
    },
    findAll: function(e) {
        var t = [];
        return this.each(function(n, i) {
            e(n, i) && t.push(n);
        }), t;
    },
    grep: function(e, t) {
        var n = [];
        return this.each(function(i, r) {
            var s = i.toString();
            s.match(e) && n.push((t || Prototype.K)(i, r));
        }), n;
    },
    include: function(e) {
        var t = !1;
        return this.each(function(n) {
            if (n == e) throw t = !0, $break;
        }), t;
    },
    inGroupsOf: function(e, t) {
        return t = void 0 === t ? null : t, this.eachSlice(e, function(n) {
            for (;n.length < e; ) n.push(t);
            return n;
        });
    },
    inject: function(e, t) {
        return this.each(function(n, i) {
            e = t(e, n, i);
        }), e;
    },
    invoke: function(e) {
        var t = $A(arguments).slice(1);
        return this.map(function(n) {
            return n[e].apply(n, t);
        });
    },
    max: function(e) {
        var t;
        return this.each(function(n, i) {
            n = (e || Prototype.K)(n, i), (void 0 == t || n >= t) && (t = n);
        }), t;
    },
    min: function(e) {
        var t;
        return this.each(function(n, i) {
            n = (e || Prototype.K)(n, i), (void 0 == t || t > n) && (t = n);
        }), t;
    },
    partition: function(e) {
        var t = [], n = [];
        return this.each(function(i, r) {
            ((e || Prototype.K)(i, r) ? t : n).push(i);
        }), [ t, n ];
    },
    pluck: function(e) {
        var t = [];
        return this.each(function(n) {
            t.push(n[e]);
        }), t;
    },
    reject: function(e) {
        var t = [];
        return this.each(function(n, i) {
            e(n, i) || t.push(n);
        }), t;
    },
    sortBy: function(e) {
        return this.map(function(t, n) {
            return {
                value: t,
                criteria: e(t, n)
            };
        }).sort(function(e, t) {
            var n = e.criteria, i = t.criteria;
            return i > n ? -1 : n > i ? 1 : 0;
        }).pluck("value");
    },
    toArray: function() {
        return this.map();
    },
    zip: function() {
        var e = Prototype.K, t = $A(arguments);
        "function" == typeof t.last() && (e = t.pop());
        var n = [ this ].concat(t).map($A);
        return this.map(function(t, i) {
            return e(n.pluck(i));
        });
    },
    size: function() {
        return this.toArray().length;
    },
    inspect: function() {
        return "#<Enumerable:" + this.toArray().inspect() + ">";
    }
};

Object.extend(Enumerable, {
    map: Enumerable.collect,
    find: Enumerable.detect,
    select: Enumerable.findAll,
    member: Enumerable.include,
    entries: Enumerable.toArray
});

var $A = Array.from = function(e) {
    if (!e) return [];
    if (e.toArray) return e.toArray();
    for (var t = [], n = 0, i = e.length; i > n; n++) t.push(e[n]);
    return t;
};

Prototype.Browser.WebKit && ($A = Array.from = function(e) {
    if (!e) return [];
    if ("function" == typeof e && "[object NodeList]" == e || !e.toArray) {
        for (var t = [], n = 0, i = e.length; i > n; n++) t.push(e[n]);
        return t;
    }
    return e.toArray();
}), Object.extend(Array.prototype, Enumerable), Array.prototype._reverse || (Array.prototype._reverse = Array.prototype.reverse), 
Object.extend(Array.prototype, {
    _each: function(e) {
        for (var t = 0, n = this.length; n > t; t++) e(this[t]);
    },
    clear: function() {
        return this.length = 0, this;
    },
    first: function() {
        return this[0];
    },
    last: function() {
        return this[this.length - 1];
    },
    compact: function() {
        return this.select(function(e) {
            return null != e;
        });
    },
    flatten: function() {
        return this.inject([], function(e, t) {
            return e.concat(t && t.constructor == Array ? t.flatten() : [ t ]);
        });
    },
    without: function() {
        var e = $A(arguments);
        return this.select(function(t) {
            return !e.include(t);
        });
    },
    indexOf: function(e) {
        for (var t = 0, n = this.length; n > t; t++) if (this[t] == e) return t;
        return -1;
    },
    reverse: function(e) {
        return (e !== !1 ? this : this.toArray())._reverse();
    },
    reduce: function() {
        return this.length > 1 ? this : this[0];
    },
    uniq: function(e) {
        return this.inject([], function(t, n, i) {
            return 0 != i && (e ? t.last() == n : t.include(n)) || t.push(n), t;
        });
    },
    clone: function() {
        return [].concat(this);
    },
    size: function() {
        return this.length;
    },
    inspect: function() {
        return "[" + this.map(Object.inspect).join(", ") + "]";
    },
    toJSON: function() {
        var e = [];
        return this.each(function(t) {
            var n = Object.toJSON(t);
            void 0 !== n && e.push(n);
        }), "[" + e.join(", ") + "]";
    }
}), Array.prototype.toArray = Array.prototype.clone, Prototype.Browser.Opera && (Array.prototype.concat = function() {
    for (var e = [], t = 0, n = this.length; n > t; t++) e.push(this[t]);
    for (var t = 0, n = arguments.length; n > t; t++) if (arguments[t].constructor == Array) for (var i = 0, r = arguments[t].length; r > i; i++) e.push(arguments[t][i]); else e.push(arguments[t]);
    return e;
});

var Hash = function(e) {
    e instanceof Hash ? this.merge(e) : Object.extend(this, e || {});
};

Object.extend(Hash, {
    toQueryString: function(e) {
        var t = [];
        return t.add = arguments.callee.addPair, this.prototype._each.call(e, function(e) {
            if (e.key) {
                var n = e.value;
                return n && "object" == typeof n ? (n.constructor == Array && n.each(function(n) {
                    t.add(e.key, n);
                }), void 0) : (t.add(e.key, n), void 0);
            }
        }), t.join("&");
    },
    toJSON: function(e) {
        var t = [];
        return this.prototype._each.call(e, function(e) {
            var n = Object.toJSON(e.value);
            void 0 !== n && t.push(e.key.toJSON() + ": " + n);
        }), "{" + t.join(", ") + "}";
    }
}), Hash.toQueryString.addPair = function(e, t) {
    e = encodeURIComponent(e), void 0 === t ? this.push(e) : this.push(e + "=" + (null == t ? "" : encodeURIComponent(t)));
}, Object.extend(Hash.prototype, Enumerable), Object.extend(Hash.prototype, {
    _each: function(e) {
        for (var t in this) {
            var n = this[t];
            if (!n || n != Hash.prototype[t]) {
                var i = [ t, n ];
                i.key = t, i.value = n, e(i);
            }
        }
    },
    keys: function() {
        return this.pluck("key");
    },
    values: function() {
        return this.pluck("value");
    },
    merge: function(e) {
        return $H(e).inject(this, function(e, t) {
            return e[t.key] = t.value, e;
        });
    },
    remove: function() {
        for (var e, t = 0, n = arguments.length; n > t; t++) {
            var i = this[arguments[t]];
            void 0 !== i && (void 0 === e ? e = i : (e.constructor != Array && (e = [ e ]), 
            e.push(i))), delete this[arguments[t]];
        }
        return e;
    },
    toQueryString: function() {
        return Hash.toQueryString(this);
    },
    inspect: function() {
        return "#<Hash:{" + this.map(function(e) {
            return e.map(Object.inspect).join(": ");
        }).join(", ") + "}>";
    },
    toJSON: function() {
        return Hash.toJSON(this);
    }
}), function() {
    var e = 0, t = function(e) {
        this.key = e;
    };
    t.prototype.key = "foo";
    for (var n in new t("bar")) e++;
    return e > 1;
}() && (Hash.prototype._each = function(e) {
    var t = [];
    for (var n in this) {
        var i = this[n];
        if (!(i && i == Hash.prototype[n] || t.include(n))) {
            t.push(n);
            var r = [ n, i ];
            r.key = n, r.value = i, e(r);
        }
    }
}), ObjectRange = Class.create(), Object.extend(ObjectRange.prototype, Enumerable), 
Object.extend(ObjectRange.prototype, {
    initialize: function(e, t, n) {
        this.start = e, this.end = t, this.exclusive = n;
    },
    _each: function(e) {
        for (var t = this.start; this.include(t); ) e(t), t = t.succ();
    },
    include: function(e) {
        return e < this.start ? !1 : this.exclusive ? e < this.end : e <= this.end;
    }
});

var $R = function(e, t, n) {
    return new ObjectRange(e, t, n);
}, Ajax = {
    getTransport: function() {
        return Try.these(function() {
            return new XMLHttpRequest();
        }, function() {
            return new ActiveXObject("Msxml2.XMLHTTP");
        }, function() {
            return new ActiveXObject("Microsoft.XMLHTTP");
        }) || !1;
    },
    activeRequestCount: 0
};

if (Ajax.Responders = {
    responders: [],
    _each: function(e) {
        this.responders._each(e);
    },
    register: function(e) {
        this.include(e) || this.responders.push(e);
    },
    unregister: function(e) {
        this.responders = this.responders.without(e);
    },
    dispatch: function(e, t, n, i) {
        this.each(function(r) {
            if ("function" == typeof r[e]) try {
                r[e].apply(r, [ t, n, i ]);
            } catch (s) {}
        });
    }
}, Object.extend(Ajax.Responders, Enumerable), Ajax.Responders.register({
    onCreate: function() {
        Ajax.activeRequestCount++;
    },
    onComplete: function() {
        Ajax.activeRequestCount--;
    }
}), Ajax.Base = function() {}, Ajax.Base.prototype = {
    setOptions: function(e) {
        this.options = {
            method: "post",
            asynchronous: !0,
            contentType: "application/x-www-form-urlencoded",
            encoding: "UTF-8",
            parameters: ""
        }, Object.extend(this.options, e || {}), this.options.method = this.options.method.toLowerCase(), 
        "string" == typeof this.options.parameters && (this.options.parameters = this.options.parameters.toQueryParams());
    }
}, Ajax.Request = Class.create(), Ajax.Request.Events = [ "Uninitialized", "Loading", "Loaded", "Interactive", "Complete" ], 
Ajax.Request.prototype = Object.extend(new Ajax.Base(), {
    _complete: !1,
    initialize: function(e, t) {
        this.transport = Ajax.getTransport(), this.setOptions(t), this.request(e);
    },
    request: function(e) {
        this.url = e, this.method = this.options.method;
        var t = Object.clone(this.options.parameters);
        [ "get", "post" ].include(this.method) || (t._method = this.method, this.method = "post"), 
        this.parameters = t, (t = Hash.toQueryString(t)) && ("get" == this.method || this.options.postBody ? this.url += (this.url.include("?") ? "&" : "?") + t : /Konqueror|Safari|KHTML/.test(navigator.userAgent) && (t += "&_="));
        try {
            this.options.onCreate && this.options.onCreate(this.transport), Ajax.Responders.dispatch("onCreate", this, this.transport), 
            this.transport.open(this.method.toUpperCase(), this.url, this.options.asynchronous), 
            this.options.asynchronous && setTimeout(function() {
                this.respondToReadyState(1);
            }.bind(this), 10), this.transport.onreadystatechange = this.onStateChange.bind(this), 
            this.setRequestHeaders(), this.body = "post" == this.method ? this.options.postBody || t : null, 
            this.transport.send(this.body), !this.options.asynchronous && this.transport.overrideMimeType && this.onStateChange();
        } catch (n) {
            this.dispatchException(n);
        }
    },
    onStateChange: function() {
        var e = this.transport.readyState;
        e > 1 && (4 != e || !this._complete) && this.respondToReadyState(this.transport.readyState);
    },
    setRequestHeaders: function() {
        var e = {
            "X-Requested-With": "XMLHttpRequest",
            "X-Prototype-Version": Prototype.Version,
            Accept: "text/javascript, text/html, application/xml, text/xml, */*"
        };
        if ("post" == this.method && (e["Content-type"] = this.options.contentType + (this.options.encoding ? "; charset=" + this.options.encoding : ""), 
        this.transport.overrideMimeType && (navigator.userAgent.match(/Gecko\/(\d{4})/) || [ 0, 2005 ])[1] < 2005 && (e.Connection = "close")), 
        "object" == typeof this.options.requestHeaders) {
            var t = this.options.requestHeaders;
            if ("function" == typeof t.push) for (var n = 0, i = t.length; i > n; n += 2) e[t[n]] = t[n + 1]; else $H(t).each(function(t) {
                e[t.key] = t.value;
            });
        }
        for (var r in e) this.transport.setRequestHeader(r, e[r]);
    },
    success: function() {
        return !this.transport.status || this.transport.status >= 200 && this.transport.status < 300;
    },
    respondToReadyState: function(e) {
        var t = Ajax.Request.Events[e], n = this.transport, i = this.evalJSON();
        if ("Complete" == t) {
            try {
                this._complete = !0, (this.options["on" + this.transport.status] || this.options["on" + (this.success() ? "Success" : "Failure")] || Prototype.emptyFunction)(n, i);
            } catch (r) {
                this.dispatchException(r);
            }
            var s = this.getHeader("Content-type");
            s && s.strip().match(/^(text|application)\/(x-)?(java|ecma)script(;.*)?$/i) && this.evalResponse();
        }
        try {
            (this.options["on" + t] || Prototype.emptyFunction)(n, i), Ajax.Responders.dispatch("on" + t, this, n, i);
        } catch (r) {
            this.dispatchException(r);
        }
        "Complete" == t && (this.transport.onreadystatechange = Prototype.emptyFunction);
    },
    getHeader: function(e) {
        try {
            return this.transport.getResponseHeader(e);
        } catch (t) {
            return null;
        }
    },
    evalJSON: function() {
        try {
            var e = this.getHeader("X-JSON");
            return e ? e.evalJSON() : null;
        } catch (t) {
            return null;
        }
    },
    evalResponse: function() {
        try {
            return eval((this.transport.responseText || "").unfilterJSON());
        } catch (e) {
            this.dispatchException(e);
        }
    },
    dispatchException: function(e) {
        (this.options.onException || Prototype.emptyFunction)(this, e), Ajax.Responders.dispatch("onException", this, e);
    }
}), Ajax.Updater = Class.create(), Object.extend(Object.extend(Ajax.Updater.prototype, Ajax.Request.prototype), {
    initialize: function(e, t, n) {
        this.container = {
            success: e.success || e,
            failure: e.failure || (e.success ? null : e)
        }, this.transport = Ajax.getTransport(), this.setOptions(n);
        var i = this.options.onComplete || Prototype.emptyFunction;
        this.options.onComplete = function(e, t) {
            this.updateContent(), i(e, t);
        }.bind(this), this.request(t);
    },
    updateContent: function() {
        var e = this.container[this.success() ? "success" : "failure"], t = this.transport.responseText;
        this.options.evalScripts || (t = t.stripScripts()), (e = $(e)) && (this.options.insertion ? new this.options.insertion(e, t) : e.update(t)), 
        this.success() && this.onComplete && setTimeout(this.onComplete.bind(this), 10);
    }
}), Ajax.PeriodicalUpdater = Class.create(), Ajax.PeriodicalUpdater.prototype = Object.extend(new Ajax.Base(), {
    initialize: function(e, t, n) {
        this.setOptions(n), this.onComplete = this.options.onComplete, this.frequency = this.options.frequency || 2, 
        this.decay = this.options.decay || 1, this.updater = {}, this.container = e, this.url = t, 
        this.start();
    },
    start: function() {
        this.options.onComplete = this.updateComplete.bind(this), this.onTimerEvent();
    },
    stop: function() {
        this.updater.options.onComplete = void 0, clearTimeout(this.timer), (this.onComplete || Prototype.emptyFunction).apply(this, arguments);
    },
    updateComplete: function(e) {
        this.options.decay && (this.decay = e.responseText == this.lastText ? this.decay * this.options.decay : 1, 
        this.lastText = e.responseText), this.timer = setTimeout(this.onTimerEvent.bind(this), 1e3 * this.decay * this.frequency);
    },
    onTimerEvent: function() {
        this.updater = new Ajax.Updater(this.container, this.url, this.options);
    }
}), Prototype.BrowserFeatures.XPath ? (document._getElementsByXPath = function(e, t) {
    for (var n = [], i = document.evaluate(e, $(t) || document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null), r = 0, s = i.snapshotLength; s > r; r++) n.push(i.snapshotItem(r));
    return n;
}, document.getElementsByClassName = function(e, t) {
    var n = ".//*[contains(concat(' ', @class, ' '), ' " + e + " ')]";
    return document._getElementsByXPath(n, t);
}) : document.getElementsByClassName = function(e, t) {
    for (var n, i = ($(t) || document.body).getElementsByTagName("*"), r = [], s = 0, o = i.length; o > s; s++) n = i[s], 
    Element.hasClassName(n, e) && r.push(Element.extend(n));
    return r;
}, !window.Element) var Element = {};

Element.extend = function(e) {
    var t = Prototype.BrowserFeatures;
    if (!e || !e.tagName || 3 == e.nodeType || e._extended || t.SpecificElementExtensions || e == window) return e;
    var n = {}, i = e.tagName, r = Element.extend.cache, s = Element.Methods.ByTag;
    t.ElementExtensions || (Object.extend(n, Element.Methods), Object.extend(n, Element.Methods.Simulated)), 
    s[i] && Object.extend(n, s[i]);
    for (var o in n) {
        var a = n[o];
        "function" != typeof a || o in e || (e[o] = r.findOrStore(a));
    }
    return e._extended = Prototype.emptyFunction, e;
}, Element.extend.cache = {
    findOrStore: function(e) {
        return this[e] = this[e] || function() {
            return e.apply(null, [ this ].concat($A(arguments)));
        };
    }
}, Element.Methods = {
    visible: function(e) {
        return "none" != $(e).style.display;
    },
    toggle: function(e) {
        return e = $(e), Element[Element.visible(e) ? "hide" : "show"](e), e;
    },
    hide: function(e) {
        return $(e).style.display = "none", e;
    },
    show: function(e) {
        return $(e).style.display = "", e;
    },
    remove: function(e) {
        return e = $(e), e.parentNode.removeChild(e), e;
    },
    update: function(e, t) {
        return t = "undefined" == typeof t ? "" : t.toString(), $(e).innerHTML = t.stripScripts(), 
        setTimeout(function() {
            t.evalScripts();
        }, 10), e;
    },
    replace: function(e, t) {
        if (e = $(e), t = "undefined" == typeof t ? "" : t.toString(), e.outerHTML) e.outerHTML = t.stripScripts(); else {
            var n = e.ownerDocument.createRange();
            n.selectNodeContents(e), e.parentNode.replaceChild(n.createContextualFragment(t.stripScripts()), e);
        }
        return setTimeout(function() {
            t.evalScripts();
        }, 10), e;
    },
    inspect: function(e) {
        e = $(e);
        var t = "<" + e.tagName.toLowerCase();
        return $H({
            id: "id",
            className: "class"
        }).each(function(n) {
            var i = n.first(), r = n.last(), s = (e[i] || "").toString();
            s && (t += " " + r + "=" + s.inspect(!0));
        }), t + ">";
    },
    recursivelyCollect: function(e, t) {
        e = $(e);
        for (var n = []; e = e[t]; ) 1 == e.nodeType && n.push(Element.extend(e));
        return n;
    },
    ancestors: function(e) {
        return $(e).recursivelyCollect("parentNode");
    },
    descendants: function(e) {
        return $A($(e).getElementsByTagName("*")).each(Element.extend);
    },
    firstDescendant: function(e) {
        for (e = $(e).firstChild; e && 1 != e.nodeType; ) e = e.nextSibling;
        return $(e);
    },
    immediateDescendants: function(e) {
        if (!(e = $(e).firstChild)) return [];
        for (;e && 1 != e.nodeType; ) e = e.nextSibling;
        return e ? [ e ].concat($(e).nextSiblings()) : [];
    },
    previousSiblings: function(e) {
        return $(e).recursivelyCollect("previousSibling");
    },
    nextSiblings: function(e) {
        return $(e).recursivelyCollect("nextSibling");
    },
    siblings: function(e) {
        return e = $(e), e.previousSiblings().reverse().concat(e.nextSiblings());
    },
    match: function(e, t) {
        return "string" == typeof t && (t = new Selector(t)), t.match($(e));
    },
    up: function(e, t, n) {
        if (e = $(e), 1 == arguments.length) return $(e.parentNode);
        var i = e.ancestors();
        return t ? Selector.findElement(i, t, n) : i[n || 0];
    },
    down: function(e, t, n) {
        if (e = $(e), 1 == arguments.length) return e.firstDescendant();
        var i = e.descendants();
        return t ? Selector.findElement(i, t, n) : i[n || 0];
    },
    previous: function(e, t, n) {
        if (e = $(e), 1 == arguments.length) return $(Selector.handlers.previousElementSibling(e));
        var i = e.previousSiblings();
        return t ? Selector.findElement(i, t, n) : i[n || 0];
    },
    next: function(e, t, n) {
        if (e = $(e), 1 == arguments.length) return $(Selector.handlers.nextElementSibling(e));
        var i = e.nextSiblings();
        return t ? Selector.findElement(i, t, n) : i[n || 0];
    },
    getElementsBySelector: function() {
        var e = $A(arguments), t = $(e.shift());
        return Selector.findChildElements(t, e);
    },
    getElementsByClassName: function(e, t) {
        return document.getElementsByClassName(t, e);
    },
    readAttribute: function(e, t) {
        if (e = $(e), Prototype.Browser.IE) {
            if (!e.attributes) return null;
            var n = Element._attributeTranslations;
            if (n.values[t]) return n.values[t](e, t);
            n.names[t] && (t = n.names[t]);
            var i = e.attributes[t];
            return i ? i.nodeValue : null;
        }
        return e.getAttribute(t);
    },
    getHeight: function(e) {
        return $(e).getDimensions().height;
    },
    getWidth: function(e) {
        return $(e).getDimensions().width;
    },
    classNames: function(e) {
        return new Element.ClassNames(e);
    },
    hasClassName: function(e, t) {
        if (e = $(e)) {
            var n = e.className;
            return 0 == n.length ? !1 : n == t || n.match(new RegExp("(^|\\s)" + t + "(\\s|$)")) ? !0 : !1;
        }
    },
    addClassName: function(e, t) {
        return (e = $(e)) ? (Element.classNames(e).add(t), e) : void 0;
    },
    removeClassName: function(e, t) {
        return (e = $(e)) ? (Element.classNames(e).remove(t), e) : void 0;
    },
    toggleClassName: function(e, t) {
        return (e = $(e)) ? (Element.classNames(e)[e.hasClassName(t) ? "remove" : "add"](t), 
        e) : void 0;
    },
    observe: function() {
        return Event.observe.apply(Event, arguments), $A(arguments).first();
    },
    stopObserving: function() {
        return Event.stopObserving.apply(Event, arguments), $A(arguments).first();
    },
    cleanWhitespace: function(e) {
        e = $(e);
        for (var t = e.firstChild; t; ) {
            var n = t.nextSibling;
            3 != t.nodeType || /\S/.test(t.nodeValue) || e.removeChild(t), t = n;
        }
        return e;
    },
    empty: function(e) {
        return $(e).innerHTML.blank();
    },
    descendantOf: function(e, t) {
        for (e = $(e), t = $(t); e = e.parentNode; ) if (e == t) return !0;
        return !1;
    },
    scrollTo: function(e) {
        e = $(e);
        var t = Position.cumulativeOffset(e);
        return window.scrollTo(t[0], t[1]), e;
    },
    getStyle: function(e, t) {
        e = $(e), t = "float" == t ? "cssFloat" : t.camelize();
        var n = e.style[t];
        if (!n) {
            var i = document.defaultView.getComputedStyle(e, null);
            n = i ? i[t] : null;
        }
        return "opacity" == t ? n ? parseFloat(n) : 1 : "auto" == n ? null : n;
    },
    getOpacity: function(e) {
        return $(e).getStyle("opacity");
    },
    setStyle: function(e, t, n) {
        e = $(e);
        var i = e.style;
        for (var r in t) "opacity" == r ? e.setOpacity(t[r]) : i["float" == r || "cssFloat" == r ? void 0 === i.styleFloat ? "cssFloat" : "styleFloat" : n ? r : r.camelize()] = t[r];
        return e;
    },
    setOpacity: function(e, t) {
        return e = $(e), e.style.opacity = 1 == t || "" === t ? "" : 1e-5 > t ? 0 : t, e;
    },
    getDimensions: function(e) {
        e = $(e);
        var t = $(e).getStyle("display");
        if ("none" != t && null != t) return {
            width: e.offsetWidth,
            height: e.offsetHeight
        };
        var n = e.style, i = n.visibility, r = n.position, s = n.display;
        n.visibility = "hidden", n.position = "absolute", n.display = "block";
        var o = e.clientWidth, a = e.clientHeight;
        return n.display = s, n.position = r, n.visibility = i, {
            width: o,
            height: a
        };
    },
    makePositioned: function(e) {
        e = $(e);
        var t = Element.getStyle(e, "position");
        return "static" != t && t || (e._madePositioned = !0, e.style.position = "relative", 
        window.opera && (e.style.top = 0, e.style.left = 0)), e;
    },
    undoPositioned: function(e) {
        return e = $(e), e._madePositioned && (e._madePositioned = void 0, e.style.position = e.style.top = e.style.left = e.style.bottom = e.style.right = ""), 
        e;
    },
    makeClipping: function(e) {
        return e = $(e), e._overflow ? e : (e._overflow = e.style.overflow || "auto", "hidden" != (Element.getStyle(e, "overflow") || "visible") && (e.style.overflow = "hidden"), 
        e);
    },
    undoClipping: function(e) {
        return e = $(e), e._overflow ? (e.style.overflow = "auto" == e._overflow ? "" : e._overflow, 
        e._overflow = null, e) : e;
    }
}, Object.extend(Element.Methods, {
    childOf: Element.Methods.descendantOf,
    childElements: Element.Methods.immediateDescendants
}), Prototype.Browser.Opera ? (Element.Methods._getStyle = Element.Methods.getStyle, 
Element.Methods.getStyle = function(e, t) {
    switch (t) {
      case "left":
      case "top":
      case "right":
      case "bottom":
        if ("static" == Element._getStyle(e, "position")) return null;

      default:
        return Element._getStyle(e, t);
    }
}) : Prototype.Browser.IE ? (Element.Methods.getStyle = function(e, t) {
    e = $(e), t = "float" == t || "cssFloat" == t ? "styleFloat" : t.camelize();
    var n = e.style[t];
    return !n && e.currentStyle && (n = e.currentStyle[t]), "opacity" == t ? (n = (e.getStyle("filter") || "").match(/alpha\(opacity=(.*)\)/)) && n[1] ? parseFloat(n[1]) / 100 : 1 : "auto" == n ? "width" != t && "height" != t || "none" == e.getStyle("display") ? null : e["offset" + t.capitalize()] + "px" : n;
}, Element.Methods.setOpacity = function(e, t) {
    e = $(e);
    var n = e.getStyle("filter"), i = e.style;
    return 1 == t || "" === t ? (i.filter = n.replace(/alpha\([^\)]*\)/gi, ""), e) : (1e-5 > t && (t = 0), 
    i.filter = n.replace(/alpha\([^\)]*\)/gi, "") + "alpha(opacity=" + 100 * t + ")", 
    e);
}, Element.Methods.update = function(e, t) {
    e = $(e), t = "undefined" == typeof t ? "" : t.toString();
    var n = e.tagName.toUpperCase();
    if ([ "THEAD", "TBODY", "TR", "TD" ].include(n)) {
        var i = document.createElement("div");
        switch (n) {
          case "THEAD":
          case "TBODY":
            i.innerHTML = "<table><tbody>" + t.stripScripts() + "</tbody></table>", depth = 2;
            break;

          case "TR":
            i.innerHTML = "<table><tbody><tr>" + t.stripScripts() + "</tr></tbody></table>", 
            depth = 3;
            break;

          case "TD":
            i.innerHTML = "<table><tbody><tr><td>" + t.stripScripts() + "</td></tr></tbody></table>", 
            depth = 4;
        }
        $A(e.childNodes).each(function(t) {
            e.removeChild(t);
        }), depth.times(function() {
            i = i.firstChild;
        }), $A(i.childNodes).each(function(t) {
            e.appendChild(t);
        });
    } else e.innerHTML = t.stripScripts();
    return setTimeout(function() {
        t.evalScripts();
    }, 10), e;
}) : Prototype.Browser.Gecko && (Element.Methods.setOpacity = function(e, t) {
    return e = $(e), e.style.opacity = 1 == t ? .999999 : "" === t ? "" : 1e-5 > t ? 0 : t, 
    e;
}), Element._attributeTranslations = {
    names: {
        colspan: "colSpan",
        rowspan: "rowSpan",
        valign: "vAlign",
        datetime: "dateTime",
        accesskey: "accessKey",
        tabindex: "tabIndex",
        enctype: "encType",
        maxlength: "maxLength",
        readonly: "readOnly",
        longdesc: "longDesc"
    },
    values: {
        _getAttr: function(e, t) {
            return e.getAttribute(t, 2);
        },
        _flag: function(e, t) {
            return $(e).hasAttribute(t) ? t : null;
        },
        style: function(e) {
            return e.style.cssText.toLowerCase();
        },
        title: function(e) {
            var t = e.getAttributeNode("title");
            return t.specified ? t.nodeValue : null;
        }
    }
}, function() {
    Object.extend(this, {
        href: this._getAttr,
        src: this._getAttr,
        type: this._getAttr,
        disabled: this._flag,
        checked: this._flag,
        readonly: this._flag,
        multiple: this._flag
    });
}.call(Element._attributeTranslations.values), Element.Methods.Simulated = {
    hasAttribute: function(e, t) {
        var n, i = Element._attributeTranslations;
        return t = i.names[t] || t, n = $(e).getAttributeNode(t), n && n.specified;
    }
}, Element.Methods.ByTag = {}, Object.extend(Element, Element.Methods), !Prototype.BrowserFeatures.ElementExtensions && document.createElement("div").__proto__ && (window.HTMLElement = {}, 
window.HTMLElement.prototype = document.createElement("div").__proto__, Prototype.BrowserFeatures.ElementExtensions = !0), 
Element.hasAttribute = function(e, t) {
    return e.hasAttribute ? e.hasAttribute(t) : Element.Methods.Simulated.hasAttribute(e, t);
}, Element.addMethods = function(e) {
    function t(t) {
        t = t.toUpperCase(), Element.Methods.ByTag[t] || (Element.Methods.ByTag[t] = {}), 
        Object.extend(Element.Methods.ByTag[t], e);
    }
    function n(e, t, n) {
        n = n || !1;
        var i = Element.extend.cache;
        for (var r in e) {
            var s = e[r];
            n && r in t || (t[r] = i.findOrStore(s));
        }
    }
    function i(e) {
        var t, n = {
            OPTGROUP: "OptGroup",
            TEXTAREA: "TextArea",
            P: "Paragraph",
            FIELDSET: "FieldSet",
            UL: "UList",
            OL: "OList",
            DL: "DList",
            DIR: "Directory",
            H1: "Heading",
            H2: "Heading",
            H3: "Heading",
            H4: "Heading",
            H5: "Heading",
            H6: "Heading",
            Q: "Quote",
            INS: "Mod",
            DEL: "Mod",
            A: "Anchor",
            IMG: "Image",
            CAPTION: "TableCaption",
            COL: "TableCol",
            COLGROUP: "TableCol",
            THEAD: "TableSection",
            TFOOT: "TableSection",
            TBODY: "TableSection",
            TR: "TableRow",
            TH: "TableCell",
            TD: "TableCell",
            FRAMESET: "FrameSet",
            IFRAME: "IFrame"
        };
        return n[e] && (t = "HTML" + n[e] + "Element"), window[t] ? window[t] : (t = "HTML" + e + "Element", 
        window[t] ? window[t] : (t = "HTML" + e.capitalize() + "Element", window[t] ? window[t] : (window[t] = {}, 
        window[t].prototype = document.createElement(e).__proto__, window[t])));
    }
    var r = Prototype.BrowserFeatures, s = Element.Methods.ByTag;
    if (e || (Object.extend(Form, Form.Methods), Object.extend(Form.Element, Form.Element.Methods), 
    Object.extend(Element.Methods.ByTag, {
        FORM: Object.clone(Form.Methods),
        INPUT: Object.clone(Form.Element.Methods),
        SELECT: Object.clone(Form.Element.Methods),
        TEXTAREA: Object.clone(Form.Element.Methods)
    })), 2 == arguments.length) {
        var o = e;
        e = arguments[1];
    }
    if (o ? o.constructor == Array ? o.each(t) : t(o) : Object.extend(Element.Methods, e || {}), 
    r.ElementExtensions && (n(Element.Methods, HTMLElement.prototype), n(Element.Methods.Simulated, HTMLElement.prototype, !0)), 
    r.SpecificElementExtensions) for (var a in Element.Methods.ByTag) {
        var c = i(a);
        "undefined" != typeof c && n(s[a], c.prototype);
    }
    Object.extend(Element, Element.Methods), delete Element.ByTag;
};

var Toggle = {
    display: Element.toggle
};

Abstract.Insertion = function(e) {
    this.adjacency = e;
}, Abstract.Insertion.prototype = {
    initialize: function(e, t) {
        if (this.element = $(e), this.content = t.stripScripts(), this.adjacency && this.element.insertAdjacentHTML) try {
            this.element.insertAdjacentHTML(this.adjacency, this.content);
        } catch (n) {
            var i = this.element.tagName.toUpperCase();
            if (![ "TBODY", "TR" ].include(i)) throw n;
            this.insertContent(this.contentFromAnonymousTable());
        } else this.range = this.element.ownerDocument.createRange(), this.initializeRange && this.initializeRange(), 
        this.insertContent([ this.range.createContextualFragment(this.content) ]);
        setTimeout(function() {
            t.evalScripts();
        }, 10);
    },
    contentFromAnonymousTable: function() {
        var e = document.createElement("div");
        return e.innerHTML = "<table><tbody>" + this.content + "</tbody></table>", $A(e.childNodes[0].childNodes[0].childNodes);
    }
};

var Insertion = new Object();

Insertion.Before = Class.create(), Insertion.Before.prototype = Object.extend(new Abstract.Insertion("beforeBegin"), {
    initializeRange: function() {
        this.range.setStartBefore(this.element);
    },
    insertContent: function(e) {
        e.each(function(e) {
            this.element.parentNode.insertBefore(e, this.element);
        }.bind(this));
    }
}), Insertion.Top = Class.create(), Insertion.Top.prototype = Object.extend(new Abstract.Insertion("afterBegin"), {
    initializeRange: function() {
        this.range.selectNodeContents(this.element), this.range.collapse(!0);
    },
    insertContent: function(e) {
        e.reverse(!1).each(function(e) {
            this.element.insertBefore(e, this.element.firstChild);
        }.bind(this));
    }
}), Insertion.Bottom = Class.create(), Insertion.Bottom.prototype = Object.extend(new Abstract.Insertion("beforeEnd"), {
    initializeRange: function() {
        this.range.selectNodeContents(this.element), this.range.collapse(this.element);
    },
    insertContent: function(e) {
        e.each(function(e) {
            this.element.appendChild(e);
        }.bind(this));
    }
}), Insertion.After = Class.create(), Insertion.After.prototype = Object.extend(new Abstract.Insertion("afterEnd"), {
    initializeRange: function() {
        this.range.setStartAfter(this.element);
    },
    insertContent: function(e) {
        e.each(function(e) {
            this.element.parentNode.insertBefore(e, this.element.nextSibling);
        }.bind(this));
    }
}), Element.ClassNames = Class.create(), Element.ClassNames.prototype = {
    initialize: function(e) {
        this.element = $(e);
    },
    _each: function(e) {
        this.element.className.split(/\s+/).select(function(e) {
            return e.length > 0;
        })._each(e);
    },
    set: function(e) {
        this.element.className = e;
    },
    add: function(e) {
        this.include(e) || this.set($A(this).concat(e).join(" "));
    },
    remove: function(e) {
        this.include(e) && this.set($A(this).without(e).join(" "));
    },
    toString: function() {
        return $A(this).join(" ");
    }
}, Object.extend(Element.ClassNames.prototype, Enumerable);

var Selector = Class.create();

Selector.prototype = {
    initialize: function(e) {
        this.expression = e.strip(), this.compileMatcher();
    },
    compileMatcher: function() {
        if (Prototype.BrowserFeatures.XPath && !/\[[\w-]*?:/.test(this.expression)) return this.compileXPathMatcher();
        var e = this.expression, ps = Selector.patterns, h = Selector.handlers, c = Selector.criteria, le, p, m;
        if (Selector._cache[e]) return this.matcher = Selector._cache[e], void 0;
        for (this.matcher = [ "this.matcher = function(root) {", "var r = root, h = Selector.handlers, c = false, n;" ]; e && le != e && /\S/.test(e); ) {
            le = e;
            for (var i in ps) if (p = ps[i], m = e.match(p)) {
                this.matcher.push("function" == typeof c[i] ? c[i](m) : new Template(c[i]).evaluate(m)), 
                e = e.replace(m[0], "");
                break;
            }
        }
        this.matcher.push("return h.unique(n);\n}"), eval(this.matcher.join("\n")), Selector._cache[this.expression] = this.matcher;
    },
    compileXPathMatcher: function() {
        var e, t, n = this.expression, i = Selector.patterns, r = Selector.xpath;
        if (Selector._cache[n]) return this.xpath = Selector._cache[n], void 0;
        for (this.matcher = [ ".//*" ]; n && e != n && /\S/.test(n); ) {
            e = n;
            for (var s in i) if (t = n.match(i[s])) {
                this.matcher.push("function" == typeof r[s] ? r[s](t) : new Template(r[s]).evaluate(t)), 
                n = n.replace(t[0], "");
                break;
            }
        }
        this.xpath = this.matcher.join(""), Selector._cache[this.expression] = this.xpath;
    },
    findElements: function(e) {
        return e = e || document, this.xpath ? document._getElementsByXPath(this.xpath, e) : this.matcher(e);
    },
    match: function(e) {
        return this.findElements(document).include(e);
    },
    toString: function() {
        return this.expression;
    },
    inspect: function() {
        return "#<Selector:" + this.expression.inspect() + ">";
    }
}, Object.extend(Selector, {
    _cache: {},
    xpath: {
        descendant: "//*",
        child: "/*",
        adjacent: "/following-sibling::*[1]",
        laterSibling: "/following-sibling::*",
        tagName: function(e) {
            return "*" == e[1] ? "" : "[local-name()='" + e[1].toLowerCase() + "' or local-name()='" + e[1].toUpperCase() + "']";
        },
        className: "[contains(concat(' ', @class, ' '), ' #{1} ')]",
        id: "[@id='#{1}']",
        attrPresence: "[@#{1}]",
        attr: function(e) {
            return e[3] = e[5] || e[6], new Template(Selector.xpath.operators[e[2]]).evaluate(e);
        },
        pseudo: function(e) {
            var t = Selector.xpath.pseudos[e[1]];
            return t ? "function" == typeof t ? t(e) : new Template(Selector.xpath.pseudos[e[1]]).evaluate(e) : "";
        },
        operators: {
            "=": "[@#{1}='#{3}']",
            "!=": "[@#{1}!='#{3}']",
            "^=": "[starts-with(@#{1}, '#{3}')]",
            "$=": "[substring(@#{1}, (string-length(@#{1}) - string-length('#{3}') + 1))='#{3}']",
            "*=": "[contains(@#{1}, '#{3}')]",
            "~=": "[contains(concat(' ', @#{1}, ' '), ' #{3} ')]",
            "|=": "[contains(concat('-', @#{1}, '-'), '-#{3}-')]"
        },
        pseudos: {
            "first-child": "[not(preceding-sibling::*)]",
            "last-child": "[not(following-sibling::*)]",
            "only-child": "[not(preceding-sibling::* or following-sibling::*)]",
            empty: "[count(*) = 0 and (count(text()) = 0 or translate(text(), ' 	\r\n', '') = '')]",
            checked: "[@checked]",
            disabled: "[@disabled]",
            enabled: "[not(@disabled)]",
            not: function(e) {
                for (var t, e, n, i = e[6], r = Selector.patterns, s = Selector.xpath, o = []; i && t != i && /\S/.test(i); ) {
                    t = i;
                    for (var a in r) if (e = i.match(r[a])) {
                        n = "function" == typeof s[a] ? s[a](e) : new Template(s[a]).evaluate(e), o.push("(" + n.substring(1, n.length - 1) + ")"), 
                        i = i.replace(e[0], "");
                        break;
                    }
                }
                return "[not(" + o.join(" and ") + ")]";
            },
            "nth-child": function(e) {
                return Selector.xpath.pseudos.nth("(count(./preceding-sibling::*) + 1) ", e);
            },
            "nth-last-child": function(e) {
                return Selector.xpath.pseudos.nth("(count(./following-sibling::*) + 1) ", e);
            },
            "nth-of-type": function(e) {
                return Selector.xpath.pseudos.nth("position() ", e);
            },
            "nth-last-of-type": function(e) {
                return Selector.xpath.pseudos.nth("(last() + 1 - position()) ", e);
            },
            "first-of-type": function(e) {
                return e[6] = "1", Selector.xpath.pseudos["nth-of-type"](e);
            },
            "last-of-type": function(e) {
                return e[6] = "1", Selector.xpath.pseudos["nth-last-of-type"](e);
            },
            "only-of-type": function(e) {
                var t = Selector.xpath.pseudos;
                return t["first-of-type"](e) + t["last-of-type"](e);
            },
            nth: function(e, t) {
                var n, i, r = t[6];
                if ("even" == r && (r = "2n+0"), "odd" == r && (r = "2n+1"), n = r.match(/^(\d+)$/)) return "[" + e + "= " + n[1] + "]";
                if (n = r.match(/^(-?\d*)?n(([+-])(\d+))?/)) {
                    "-" == n[1] && (n[1] = -1);
                    var s = n[1] ? Number(n[1]) : 1, o = n[2] ? Number(n[2]) : 0;
                    return i = "[((#{fragment} - #{b}) mod #{a} = 0) and ((#{fragment} - #{b}) div #{a} >= 0)]", 
                    new Template(i).evaluate({
                        fragment: e,
                        a: s,
                        b: o
                    });
                }
            }
        }
    },
    criteria: {
        tagName: 'n = h.tagName(n, r, "#{1}", c);   c = false;',
        className: 'n = h.className(n, r, "#{1}", c); c = false;',
        id: 'n = h.id(n, r, "#{1}", c);        c = false;',
        attrPresence: 'n = h.attrPresence(n, r, "#{1}"); c = false;',
        attr: function(e) {
            return e[3] = e[5] || e[6], new Template('n = h.attr(n, r, "#{1}", "#{3}", "#{2}"); c = false;').evaluate(e);
        },
        pseudo: function(e) {
            return e[6] && (e[6] = e[6].replace(/"/g, '\\"')), new Template('n = h.pseudo(n, "#{1}", "#{6}", r, c); c = false;').evaluate(e);
        },
        descendant: 'c = "descendant";',
        child: 'c = "child";',
        adjacent: 'c = "adjacent";',
        laterSibling: 'c = "laterSibling";'
    },
    patterns: {
        laterSibling: /^\s*~\s*/,
        child: /^\s*>\s*/,
        adjacent: /^\s*\+\s*/,
        descendant: /^\s/,
        tagName: /^\s*(\*|[\w\-]+)(\b|$)?/,
        id: /^#([\w\-\*]+)(\b|$)/,
        className: /^\.([\w\-\*]+)(\b|$)/,
        pseudo: /^:((first|last|nth|nth-last|only)(-child|-of-type)|empty|checked|(en|dis)abled|not)(\((.*?)\))?(\b|$|\s|(?=:))/,
        attrPresence: /^\[([\w]+)\]/,
        attr: /\[((?:[\w-]*:)?[\w-]+)\s*(?:([!^$*~|]?=)\s*((['"])([^\]]*?)\4|([^'"][^\]]*?)))?\]/
    },
    handlers: {
        concat: function(e, t) {
            for (var n, i = 0; n = t[i]; i++) e.push(n);
            return e;
        },
        mark: function(e) {
            for (var t, n = 0; t = e[n]; n++) t._counted = !0;
            return e;
        },
        unmark: function(e) {
            for (var t, n = 0; t = e[n]; n++) t._counted = void 0;
            return e;
        },
        index: function(e, t, n) {
            if (e._counted = !0, t) for (var i = e.childNodes, r = i.length - 1, s = 1; r >= 0; r--) node = i[r], 
            1 != node.nodeType || n && !node._counted || (node.nodeIndex = s++); else for (var r = 0, s = 1, i = e.childNodes; node = i[r]; r++) 1 != node.nodeType || n && !node._counted || (node.nodeIndex = s++);
        },
        unique: function(e) {
            if (0 == e.length) return e;
            for (var t, n = [], i = 0, r = e.length; r > i; i++) (t = e[i])._counted || (t._counted = !0, 
            n.push(Element.extend(t)));
            return Selector.handlers.unmark(n);
        },
        descendant: function(e) {
            for (var t, n = Selector.handlers, i = 0, r = []; t = e[i]; i++) n.concat(r, t.getElementsByTagName("*"));
            return r;
        },
        child: function(e) {
            Selector.handlers;
            for (var t, n = 0, i = []; t = e[n]; n++) for (var r, s = 0; r = t.childNodes[s]; s++) 1 == r.nodeType && "!" != r.tagName && i.push(r);
            return i;
        },
        adjacent: function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) {
                var r = this.nextElementSibling(t);
                r && i.push(r);
            }
            return i;
        },
        laterSibling: function(e) {
            for (var t, n = Selector.handlers, i = 0, r = []; t = e[i]; i++) n.concat(r, Element.nextSiblings(t));
            return r;
        },
        nextElementSibling: function(e) {
            for (;e = e.nextSibling; ) if (1 == e.nodeType) return e;
            return null;
        },
        previousElementSibling: function(e) {
            for (;e = e.previousSibling; ) if (1 == e.nodeType) return e;
            return null;
        },
        tagName: function(e, t, n, i) {
            n = n.toUpperCase();
            var r = [], s = Selector.handlers;
            if (e) {
                if (i) {
                    if ("descendant" == i) {
                        for (var o, a = 0; o = e[a]; a++) s.concat(r, o.getElementsByTagName(n));
                        return r;
                    }
                    if (e = this[i](e), "*" == n) return e;
                }
                for (var o, a = 0; o = e[a]; a++) o.tagName.toUpperCase() == n && r.push(o);
                return r;
            }
            return t.getElementsByTagName(n);
        },
        id: function(e, t, n, i) {
            var r = $(n), s = Selector.handlers;
            if (!e && t == document) return r ? [ r ] : [];
            if (e) {
                if (i) if ("child" == i) {
                    for (var o, a = 0; o = e[a]; a++) if (r.parentNode == o) return [ r ];
                } else if ("descendant" == i) {
                    for (var o, a = 0; o = e[a]; a++) if (Element.descendantOf(r, o)) return [ r ];
                } else if ("adjacent" == i) {
                    for (var o, a = 0; o = e[a]; a++) if (Selector.handlers.previousElementSibling(r) == o) return [ r ];
                } else e = s[i](e);
                for (var o, a = 0; o = e[a]; a++) if (o == r) return [ r ];
                return [];
            }
            return r && Element.descendantOf(r, t) ? [ r ] : [];
        },
        className: function(e, t, n, i) {
            return e && i && (e = this[i](e)), Selector.handlers.byClassName(e, t, n);
        },
        byClassName: function(e, t, n) {
            e || (e = Selector.handlers.descendant([ t ]));
            for (var i, r, s = " " + n + " ", o = 0, a = []; i = e[o]; o++) r = i.className, 
            0 != r.length && (r == n || (" " + r + " ").include(s)) && a.push(i);
            return a;
        },
        attrPresence: function(e, t, n) {
            for (var i, r = [], s = 0; i = e[s]; s++) Element.hasAttribute(i, n) && r.push(i);
            return r;
        },
        attr: function(e, t, n, i, r) {
            e || (e = t.getElementsByTagName("*"));
            for (var s, o = Selector.operators[r], a = [], c = 0; s = e[c]; c++) {
                var l = Element.readAttribute(s, n);
                null !== l && o(l, i) && a.push(s);
            }
            return a;
        },
        pseudo: function(e, t, n, i, r) {
            return e && r && (e = this[r](e)), e || (e = i.getElementsByTagName("*")), Selector.pseudos[t](e, n, i);
        }
    },
    pseudos: {
        "first-child": function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) Selector.handlers.previousElementSibling(t) || i.push(t);
            return i;
        },
        "last-child": function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) Selector.handlers.nextElementSibling(t) || i.push(t);
            return i;
        },
        "only-child": function(e) {
            for (var t, n = Selector.handlers, i = 0, r = []; t = e[i]; i++) n.previousElementSibling(t) || n.nextElementSibling(t) || r.push(t);
            return r;
        },
        "nth-child": function(e, t, n) {
            return Selector.pseudos.nth(e, t, n);
        },
        "nth-last-child": function(e, t, n) {
            return Selector.pseudos.nth(e, t, n, !0);
        },
        "nth-of-type": function(e, t, n) {
            return Selector.pseudos.nth(e, t, n, !1, !0);
        },
        "nth-last-of-type": function(e, t, n) {
            return Selector.pseudos.nth(e, t, n, !0, !0);
        },
        "first-of-type": function(e, t, n) {
            return Selector.pseudos.nth(e, "1", n, !1, !0);
        },
        "last-of-type": function(e, t, n) {
            return Selector.pseudos.nth(e, "1", n, !0, !0);
        },
        "only-of-type": function(e, t, n) {
            var i = Selector.pseudos;
            return i["last-of-type"](i["first-of-type"](e, t, n), t, n);
        },
        getIndices: function(e, t, n) {
            return 0 == e ? t > 0 ? [ t ] : [] : $R(1, n).inject([], function(n, i) {
                return 0 == (i - t) % e && (i - t) / e >= 0 && n.push(i), n;
            });
        },
        nth: function(e, t, n, i, r) {
            if (0 == e.length) return [];
            "even" == t && (t = "2n+0"), "odd" == t && (t = "2n+1");
            var s, o = Selector.handlers, a = [], c = [];
            o.mark(e);
            for (var l, h = 0; l = e[h]; h++) l.parentNode._counted || (o.index(l.parentNode, i, r), 
            c.push(l.parentNode));
            if (t.match(/^\d+$/)) {
                t = Number(t);
                for (var l, h = 0; l = e[h]; h++) l.nodeIndex == t && a.push(l);
            } else if (s = t.match(/^(-?\d*)?n(([+-])(\d+))?/)) {
                "-" == s[1] && (s[1] = -1);
                for (var l, u = s[1] ? Number(s[1]) : 1, d = s[2] ? Number(s[2]) : 0, p = Selector.pseudos.getIndices(u, d, e.length), h = 0, f = p.length; l = e[h]; h++) for (var g = 0; f > g; g++) l.nodeIndex == p[g] && a.push(l);
            }
            return o.unmark(e), o.unmark(c), a;
        },
        empty: function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) "!" == t.tagName || t.firstChild && !t.innerHTML.match(/^\s*$/) || i.push(t);
            return i;
        },
        not: function(e, t, n) {
            var i = Selector.handlers, r = new Selector(t).findElements(n);
            i.mark(r);
            for (var s, o = 0, a = []; s = e[o]; o++) s._counted || a.push(s);
            return i.unmark(r), a;
        },
        enabled: function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) t.disabled || i.push(t);
            return i;
        },
        disabled: function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) t.disabled && i.push(t);
            return i;
        },
        checked: function(e) {
            for (var t, n = 0, i = []; t = e[n]; n++) t.checked && i.push(t);
            return i;
        }
    },
    operators: {
        "=": function(e, t) {
            return e == t;
        },
        "!=": function(e, t) {
            return e != t;
        },
        "^=": function(e, t) {
            return e.startsWith(t);
        },
        "$=": function(e, t) {
            return e.endsWith(t);
        },
        "*=": function(e, t) {
            return e.include(t);
        },
        "~=": function(e, t) {
            return (" " + e + " ").include(" " + t + " ");
        },
        "|=": function(e, t) {
            return ("-" + e.toUpperCase() + "-").include("-" + t.toUpperCase() + "-");
        }
    },
    matchElements: function(e, t) {
        var n = new Selector(t).findElements(), i = Selector.handlers;
        i.mark(n);
        for (var r, s = 0, o = []; r = e[s]; s++) r._counted && o.push(r);
        return i.unmark(n), o;
    },
    findElement: function(e, t, n) {
        return "number" == typeof t && (n = t, t = !1), Selector.matchElements(e, t || "*")[n || 0];
    },
    findChildElements: function(e, t) {
        var n = t.join(","), t = [];
        n.scan(/(([\w#:.~>+()\s-]+|\*|\[.*?\])+)\s*(,|$)/, function(e) {
            t.push(e[1].strip());
        });
        for (var i, r = [], s = Selector.handlers, o = 0, a = t.length; a > o; o++) i = new Selector(t[o].strip()), 
        s.concat(r, i.findElements(e));
        return a > 1 ? s.unique(r) : r;
    }
});

var Form = {
    reset: function(e) {
        return $(e).reset(), e;
    },
    serializeElements: function(e, t) {
        var n = e.inject({}, function(e, t) {
            if (!t.disabled && t.name) {
                var n = t.name, i = $(t).getValue();
                null != i && (n in e ? (e[n].constructor != Array && (e[n] = [ e[n] ]), e[n].push(i)) : e[n] = i);
            }
            return e;
        });
        return t ? n : Hash.toQueryString(n);
    }
};

Form.Methods = {
    serialize: function(e, t) {
        return Form.serializeElements(Form.getElements(e), t);
    },
    getElements: function(e) {
        return $A($(e).getElementsByTagName("*")).inject([], function(e, t) {
            return Form.Element.Serializers[t.tagName.toLowerCase()] && e.push(Element.extend(t)), 
            e;
        });
    },
    getInputs: function(e, t, n) {
        e = $(e);
        var i = e.getElementsByTagName("input");
        if (!t && !n) return $A(i).map(Element.extend);
        for (var r = 0, s = [], o = i.length; o > r; r++) {
            var a = i[r];
            t && a.type != t || n && a.name != n || s.push(Element.extend(a));
        }
        return s;
    },
    disable: function(e) {
        return e = $(e), Form.getElements(e).invoke("disable"), e;
    },
    enable: function(e) {
        return e = $(e), Form.getElements(e).invoke("enable"), e;
    },
    findFirstElement: function(e) {
        return $(e).getElements().find(function(e) {
            return "hidden" != e.type && !e.disabled && [ "input", "select", "textarea" ].include(e.tagName.toLowerCase());
        });
    },
    focusFirstElement: function(e) {
        return e = $(e), e.findFirstElement().activate(), e;
    },
    request: function(e, t) {
        e = $(e), t = Object.clone(t || {});
        var n = t.parameters;
        return t.parameters = e.serialize(!0), n && ("string" == typeof n && (n = n.toQueryParams()), 
        Object.extend(t.parameters, n)), e.hasAttribute("method") && !t.method && (t.method = e.method), 
        new Ajax.Request(e.readAttribute("action"), t);
    }
}, Form.Element = {
    focus: function(e) {
        return $(e).focus(), e;
    },
    select: function(e) {
        return $(e).select(), e;
    }
}, Form.Element.Methods = {
    serialize: function(e) {
        if (e = $(e), !e.disabled && e.name) {
            var t = e.getValue();
            if (void 0 != t) {
                var n = {};
                return n[e.name] = t, Hash.toQueryString(n);
            }
        }
        return "";
    },
    getValue: function(e) {
        e = $(e);
        var t = e.tagName.toLowerCase();
        return Form.Element.Serializers[t](e);
    },
    clear: function(e) {
        return $(e).value = "", e;
    },
    present: function(e) {
        return "" != $(e).value;
    },
    activate: function(e) {
        e = $(e);
        try {
            e.focus(), !e.select || "input" == e.tagName.toLowerCase() && [ "button", "reset", "submit" ].include(e.type) || e.select();
        } catch (t) {}
        return e;
    },
    disable: function(e) {
        return e = $(e), e.blur(), e.disabled = !0, e;
    },
    enable: function(e) {
        return e = $(e), e.disabled = !1, e;
    }
};

var Field = Form.Element, $F = Form.Element.Methods.getValue;

if (Form.Element.Serializers = {
    input: function(e) {
        switch (e.type.toLowerCase()) {
          case "checkbox":
          case "radio":
            return Form.Element.Serializers.inputSelector(e);

          default:
            return Form.Element.Serializers.textarea(e);
        }
    },
    inputSelector: function(e) {
        return e.checked ? e.value : null;
    },
    textarea: function(e) {
        return e.value;
    },
    select: function(e) {
        return this["select-one" == e.type ? "selectOne" : "selectMany"](e);
    },
    selectOne: function(e) {
        var t = e.selectedIndex;
        return t >= 0 ? this.optionValue(e.options[t]) : null;
    },
    selectMany: function(e) {
        var t, n = e.length;
        if (!n) return null;
        for (var i = 0, t = []; n > i; i++) {
            var r = e.options[i];
            r.selected && t.push(this.optionValue(r));
        }
        return t;
    },
    optionValue: function(e) {
        return Element.extend(e).hasAttribute("value") ? e.value : e.text;
    }
}, Abstract.TimedObserver = function() {}, Abstract.TimedObserver.prototype = {
    initialize: function(e, t, n) {
        this.frequency = t, this.element = $(e), this.callback = n, this.lastValue = this.getValue(), 
        this.registerCallback();
    },
    registerCallback: function() {
        setInterval(this.onTimerEvent.bind(this), 1e3 * this.frequency);
    },
    onTimerEvent: function() {
        var e = this.getValue(), t = "string" == typeof this.lastValue && "string" == typeof e ? this.lastValue != e : String(this.lastValue) != String(e);
        t && (this.callback(this.element, e), this.lastValue = e);
    }
}, Form.Element.Observer = Class.create(), Form.Element.Observer.prototype = Object.extend(new Abstract.TimedObserver(), {
    getValue: function() {
        return Form.Element.getValue(this.element);
    }
}), Form.Observer = Class.create(), Form.Observer.prototype = Object.extend(new Abstract.TimedObserver(), {
    getValue: function() {
        return Form.serialize(this.element);
    }
}), Abstract.EventObserver = function() {}, Abstract.EventObserver.prototype = {
    initialize: function(e, t) {
        this.element = $(e), this.callback = t, this.lastValue = this.getValue(), "form" == this.element.tagName.toLowerCase() ? this.registerFormCallbacks() : this.registerCallback(this.element);
    },
    onElementEvent: function() {
        var e = this.getValue();
        this.lastValue != e && (this.callback(this.element, e), this.lastValue = e);
    },
    registerFormCallbacks: function() {
        Form.getElements(this.element).each(this.registerCallback.bind(this));
    },
    registerCallback: function(e) {
        if (e.type) switch (e.type.toLowerCase()) {
          case "checkbox":
          case "radio":
            Event.observe(e, "click", this.onElementEvent.bind(this));
            break;

          default:
            Event.observe(e, "change", this.onElementEvent.bind(this));
        }
    }
}, Form.Element.EventObserver = Class.create(), Form.Element.EventObserver.prototype = Object.extend(new Abstract.EventObserver(), {
    getValue: function() {
        return Form.Element.getValue(this.element);
    }
}), Form.EventObserver = Class.create(), Form.EventObserver.prototype = Object.extend(new Abstract.EventObserver(), {
    getValue: function() {
        return Form.serialize(this.element);
    }
}), !window.Event) var Event = new Object();

Object.extend(Event, {
    KEY_BACKSPACE: 8,
    KEY_TAB: 9,
    KEY_RETURN: 13,
    KEY_ESC: 27,
    KEY_LEFT: 37,
    KEY_UP: 38,
    KEY_RIGHT: 39,
    KEY_DOWN: 40,
    KEY_DELETE: 46,
    KEY_HOME: 36,
    KEY_END: 35,
    KEY_PAGEUP: 33,
    KEY_PAGEDOWN: 34,
    element: function(e) {
        return $(e.target || e.srcElement);
    },
    isLeftClick: function(e) {
        return e.which && 1 == e.which || e.button && 1 == e.button;
    },
    pointerX: function(e) {
        return e.pageX || e.clientX + (document.documentElement.scrollLeft || document.body.scrollLeft);
    },
    pointerY: function(e) {
        return e.pageY || e.clientY + (document.documentElement.scrollTop || document.body.scrollTop);
    },
    stop: function(e) {
        e.preventDefault ? (e.preventDefault(), e.stopPropagation()) : (e.returnValue = !1, 
        e.cancelBubble = !0);
    },
    findElement: function(e, t) {
        for (var n = Event.element(e); n.parentNode && (!n.tagName || n.tagName.toUpperCase() != t.toUpperCase()); ) n = n.parentNode;
        return n;
    },
    observers: !1,
    _observeAndCache: function(e, t, n, i) {
        this.observers || (this.observers = []), e.addEventListener ? (this.observers.push([ e, t, n, i ]), 
        e.addEventListener(t, n, i)) : e.attachEvent && (this.observers.push([ e, t, n, i ]), 
        e.attachEvent("on" + t, n));
    },
    unloadCache: function() {
        if (Event.observers) {
            for (var e = 0, t = Event.observers.length; t > e; e++) Event.stopObserving.apply(this, Event.observers[e]), 
            Event.observers[e][0] = null;
            Event.observers = !1;
        }
    },
    observe: function(e, t, n, i) {
        e = $(e), i = i || !1, "keypress" == t && (Prototype.Browser.WebKit || e.attachEvent) && (t = "keydown"), 
        Event._observeAndCache(e, t, n, i);
    },
    stopObserving: function(e, t, n, i) {
        if (e = $(e), i = i || !1, "keypress" == t && (Prototype.Browser.WebKit || e.attachEvent) && (t = "keydown"), 
        e.removeEventListener) e.removeEventListener(t, n, i); else if (e.detachEvent) try {
            e.detachEvent("on" + t, n);
        } catch (r) {}
    }
}), Prototype.Browser.IE && Event.observe(window, "unload", Event.unloadCache, !1);

var Position = {
    includeScrollOffsets: !1,
    prepare: function() {
        this.deltaX = window.pageXOffset || document.documentElement.scrollLeft || document.body.scrollLeft || 0, 
        this.deltaY = window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0;
    },
    realOffset: function(e) {
        var t = 0, n = 0;
        do t += e.scrollTop || 0, n += e.scrollLeft || 0, e = e.parentNode; while (e);
        return [ n, t ];
    },
    cumulativeOffset: function(e) {
        var t = 0, n = 0;
        do t += e.offsetTop || 0, n += e.offsetLeft || 0, e = e.offsetParent; while (e);
        return [ n, t ];
    },
    positionedOffset: function(e) {
        var t = 0, n = 0;
        do if (t += e.offsetTop || 0, n += e.offsetLeft || 0, e = e.offsetParent) {
            if ("BODY" == e.tagName) break;
            var i = Element.getStyle(e, "position");
            if ("relative" == i || "absolute" == i) break;
        } while (e);
        return [ n, t ];
    },
    offsetParent: function(e) {
        if (e.offsetParent) return e.offsetParent;
        if (e == document.body) return e;
        for (;(e = e.parentNode) && e != document.body; ) if ("static" != Element.getStyle(e, "position")) return e;
        return document.body;
    },
    within: function(e, t, n) {
        return this.includeScrollOffsets ? this.withinIncludingScrolloffsets(e, t, n) : (this.xcomp = t, 
        this.ycomp = n, this.offset = this.cumulativeOffset(e), n >= this.offset[1] && n < this.offset[1] + e.offsetHeight && t >= this.offset[0] && t < this.offset[0] + e.offsetWidth);
    },
    withinIncludingScrolloffsets: function(e, t, n) {
        var i = this.realOffset(e);
        return this.xcomp = t + i[0] - this.deltaX, this.ycomp = n + i[1] - this.deltaY, 
        this.offset = this.cumulativeOffset(e), this.ycomp >= this.offset[1] && this.ycomp < this.offset[1] + e.offsetHeight && this.xcomp >= this.offset[0] && this.xcomp < this.offset[0] + e.offsetWidth;
    },
    overlap: function(e, t) {
        return e ? "vertical" == e ? (this.offset[1] + t.offsetHeight - this.ycomp) / t.offsetHeight : "horizontal" == e ? (this.offset[0] + t.offsetWidth - this.xcomp) / t.offsetWidth : void 0 : 0;
    },
    page: function(e) {
        var t = 0, n = 0, i = e;
        do if (t += i.offsetTop || 0, n += i.offsetLeft || 0, i.offsetParent == document.body && "absolute" == Element.getStyle(i, "position")) break; while (i = i.offsetParent);
        i = e;
        do window.opera && "BODY" != i.tagName || (t -= i.scrollTop || 0, n -= i.scrollLeft || 0); while (i = i.parentNode);
        return [ n, t ];
    },
    clone: function(e, t) {
        var n = Object.extend({
            setLeft: !0,
            setTop: !0,
            setWidth: !0,
            setHeight: !0,
            offsetTop: 0,
            offsetLeft: 0
        }, arguments[2] || {});
        e = $(e);
        var i = Position.page(e);
        t = $(t);
        var r = [ 0, 0 ], s = null;
        "absolute" == Element.getStyle(t, "position") && (s = Position.offsetParent(t), 
        r = Position.page(s)), s == document.body && (r[0] -= document.body.offsetLeft, 
        r[1] -= document.body.offsetTop), n.setLeft && (t.style.left = i[0] - r[0] + n.offsetLeft + "px"), 
        n.setTop && (t.style.top = i[1] - r[1] + n.offsetTop + "px"), n.setWidth && (t.style.width = e.offsetWidth + "px"), 
        n.setHeight && (t.style.height = e.offsetHeight + "px");
    },
    absolutize: function(e) {
        if (e = $(e), "absolute" != e.style.position) {
            Position.prepare();
            var t = Position.positionedOffset(e), n = t[1], i = t[0], r = e.clientWidth, s = e.clientHeight;
            e._originalLeft = i - parseFloat(e.style.left || 0), e._originalTop = n - parseFloat(e.style.top || 0), 
            e._originalWidth = e.style.width, e._originalHeight = e.style.height, e.style.position = "absolute", 
            e.style.top = n + "px", e.style.left = i + "px", e.style.width = r + "px", e.style.height = s + "px";
        }
    },
    relativize: function(e) {
        if (e = $(e), "relative" != e.style.position) {
            Position.prepare(), e.style.position = "relative";
            var t = parseFloat(e.style.top || 0) - (e._originalTop || 0), n = parseFloat(e.style.left || 0) - (e._originalLeft || 0);
            e.style.top = t + "px", e.style.left = n + "px", e.style.height = e._originalHeight, 
            e.style.width = e._originalWidth;
        }
    }
};

if (Prototype.Browser.WebKit && (Position.cumulativeOffset = function(e) {
    var t = 0, n = 0;
    do {
        if (t += e.offsetTop || 0, n += e.offsetLeft || 0, e.offsetParent == document.body && "absolute" == Element.getStyle(e, "position")) break;
        e = e.offsetParent;
    } while (e);
    return [ n, t ];
}), Element.addMethods(), !ORYX) var ORYX = {};

ORYX.I18N || (ORYX.I18N = {}), ORYX.I18N.Language = "en_us", ORYX.I18N.Oryx || (ORYX.I18N.Oryx = {}), 
ORYX.I18N.Oryx.title = ORYX.TITLE, ORYX.I18N.Oryx.noBackendDefined = "Caution! \nNo Backend defined.\n The requested model cannot be loaded. Try to load a configuration with a save plugin.", 
ORYX.I18N.Oryx.pleaseWait = '<center>  <img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAMPklEQVR4nGL8////f4YhDAAAAAD//2IaaAdQCgAAAAD//xryHgAAAAD//xryHgAAAAD//xryHgAAAAD//xryHgAAAAD//2IZGGv/QxC8AITRjHA5BiZGBkZGwuELAAAA//+0lKENACAAw9r9fzJkCIJAoZhv0pr5/UZbeqQAFPWNAZ0DCiYY2YH3FgAAAP//rNAxEkAwFEXRm6iMyozC6HQWYP9bEgmGn4yvSFRaG7jzzvsZoOSalq3fB1UhyUU8POIXZHNIcMR9JQbHLSfNMNJOM3XXY19MShgDxlalnTEPAAAA//+kkrEKwjAUAI8OXUoFpeDSraOCYyf//09aiMGUak1C8nwOWdz7Awd33O6Fir8CpWzhFnjOibg6wmLZHhPezgRn8E9DejnEv5GwIfGD5gQiaI4gQt0eOQxXutud02WkOfdUwFcV/pr/AAAA//+ckjsKwkAUACdp0giJSO5gGUhqG+9/DBWMEljNavZ91iLYWHqAgYGZvwp8kd8V0vtFnEYe1xPzeCbeLizhjjzDKiYJ3ClwcAMzcAVTsgquilvCRclLRGMgq1A1Ldt9z6470A5Hqk291ihKPgAAAP//jNFLCgIxEAbhSoeAMOIxPID3v4orGRFGFB+ome4/cSW69AYfVX8f+EV/4OHO43rmfjxwm8Zv2fqEcFIX1kQpBSXoyZDPKJymTg/RFUQI1MgSpiBrBkvYsKJ5pU479uOW1+XEcr0hLwZyNswSbwAAAP//fNMxCsJAEEDRv8uIiZ3gGSwsLexyCO/fi2BYiJqgOzszFrbiFf7j/xX4VVrrm3kqPG5X7uOF11SoyxOrFW2NZo65oWpoa1gkIiWc9D0jZTwC84BwiMDcaVqhKbnOdEuhn0e6pSA5s9kf2Q1ntocTkmAlwrrrERE+AAAA//9slEsKwlAQwDIzzmu1ILrt1vufS0W0rVKo7fu4EEHBKyQkfw2UUn5Jx4VHf6M7H+mvJ8ahY56eZKCIIdUebQLqgToEVh7wUOHuqBmqhpogCKYK8rXQ8g4/p0jOmZgSKWXiPMH9QrPdUbcHTJVlHND1Btz5jOIFAAD//3SUwQqDQAxEX5IlW/3/r9QKLbYeRJMeFlwvvQ/MMMO8vwtEBOv65jlPvJaZ7fvB1Che8WHkMYxXSFNBRVBtzMgI8o7PTLpJO3wmIB2v18oiSJOQapxnwLFjpeBe8Vp7OWr8AAAA//98lb0OgzAMhD87TpFaeP+3pIUpEandwTCw9KTb70enuxnwCFrvrJ83+7Zx9EY1Y37NTNOD7zhQgtEb4YPwgGLU53KK9kxcFFFFrxYlT0kkBf5HgJPm3TnXSiklaRUzy1ZV+QEAAP//bJQ5DkBQFEXPe8ZIJDRWoLX/5SgUGhGF8QtfQYSwgZt7cocXwGQMTdviO0oUBHiej6owjgN1VWL6jm2ezpGaBbvOgCVKM9K8IIwTxO647lWdJwAC8n2uf4YrsUc3RAUVPU2r3joHAAAA//+MlbkNgEAQA4c/JEUipAQaoP8ykKAB3rs9dgmAACKcOLNkayS/CpjZdeaq+CCEIHjnGYcedQvrPGOyU5ii4lBxmN841ok0L6jajrJuSExJs5w4ie/lozcmP/Ql+5vx+AkAAP//YkLX9OfvX4bf0AzFxMTM8OnLZ4Zfv34y3Lj7kOHK7QcM7IISDBxCYpDkA0nIDExsHAx/vn9leHxwA8P7+9cY/jOzMPz79xcix8QEx7CCAVkMG8amBiaGXLgwMDAwAAAAAP//QvEAQiMzAysrK8OfP38Zvnz9wnDt5h2GZy9eMRgaGjIIi4gwsAqIMvDIKMGjm+H/PwZGZhYGhv//GZ4cWM/w5vYlhj//GRj+/P7J8A+aoWHmExMLMHXoGBsAAAAA///CaKwwQjMcMxMzw7efPxjuP3jI8OLFSwZdXR0GcXExhl+/fjGwMDEy8IhJM/DLqzH8h4Y0w///0HbKf4Yn+9YwvHtwg+HPP0aG379+MPz794+go8kFAAAAAP//rNKxDcIwEEDRf45kiJKO0FAZKlbPDpnGyAUiFeXZvmOAtPwV3j8IAIQgmDvvfSfnF9dl4ZESqkqMJ8Zx4hwjl/Rkvt2xpiCCuyHDgPdK2Va+n0LtTquKWT98/Y9+AAAA//+s1j0SQDAUAOGNEUkotFpKrVu7g8swWoXCXx5RqMwo7Rm2+D7A/YBsF6HvB7Z1o6pKtNYoFWGMxTqHcynWGIq6IclywikoFOEKRLHGzxNj17KvC94fiMhrp7+6AQAA///C4gFIxvzw6TPDy5cvGXj5eBkkxcUZ/v79y8DKxsrAysaOqFBYWBi4+YUYRDWNGf7//8cAa4X+//efgZmDi+Hrg2sMz49tYfj9n4Hh988fDP/+QmKBmp4AAAAA//+s1z0OQEAUAOGRt0/Y6CSoJEr3L11Bo3UCJEu9WX/ddjpHmORr5nN5lm1ld466qrA2B0A1RVNFjCIiGKMID2XXUzQtV/CR4XNfSGZx48AxT5wkhOAjpb8iXgAAAP//wuqB/wwMDN++fmUQFRZmUJCTY2BgYGBgZmFhYGFlgziaCVq+Q0ssNjZ2BjFtMwZGZmYG1NqHkYHh3z+GF/tXMXz7+J7h9+8/DH9+/6ZqDAAAAAD//6SYsRGCQBAA15//OSAktwxrcGzC+miBMoihAxKMfO4PzsBhDAxpYZPd2b8WOuiUoqiurDljRYkpIVIjVUWM6UfanX3bMHfGvmOZBgIXdn3jJeNm2Gumvd25Pp5IAKmbr63D+afwAQAA///CWowyMDBAqm1GZgZmaNOVnZ2TgZWVjYGZiRmlbIZV7SwsLAziOmYMjCyskHYMLD/8/8vAzMHN8P7MHoYP9y5Dk9Ivhn9//1KleAUAAAD//8IaBDCHsbKyMHCwczCwc3AysLKxM7CwsDAwYgk1JiYmBoZ/fxn4ZZQZ+GRUUPICxC9MDP9+/WR4tXcVw4/v3xj+/PrN8PfvH6rkBQAAAAD//6zVsRFAUBAG4b3/7h4j14RY+zqQyLUgE2EQC16mhm92tmpoEh5JZqGUhsxA7tUryoyIpB9GzPRR4LnxtmNfF7Z54jJxHv8E/QIAAP//whkDsFhgZmFhYIZ2SnA5npGREVIL//vDICCvzsAjLsvw7/cvlKbzf4b/DEwsLAxvDq1j+Pr2JcPvv/8Y/vz+TXEyAgAAAP//wh0DJLRHkAEbByeDsIYxtImBBP7/Y2Bk5WD4+eoRw5tjmxn+MEBjgcK6AQAAAP//otrQIqwNxfDvH4OQqj4DO78wpHZG9vT/fwxMbJwM745tYfjy7B6kwfcHFgvkeQAAAAD//6Lq2CgkM/9j4BIQZhBQ1mX4i5aMGP7/Z2BkYWX48/E1w+uDayE19K+fDH///mX4R2YsAAAAAP//ovrgLqS3xMAgomnCwMzKhumof/8YmDm4GT6c2snw5eENhj8MjAx/f/9i+PeXvHYSAAAA//+ivgcYGRn+//3LwCejzMAtqcDw7/dPjMzMwMzC8Pfre4bXh9Yy/PnHQFGJBAAAAP//ookHGBn+M7CysUEy89+/kBIKeZz03z8GJg5uho+ndzF8eXid4Q8jE7SJQXqJBAAAAP//osn8ACMjEwPD//8MQmqGDCycPAy/v3xg+P/7FwN0qBnSQWdhZ/j75SPD64NrIJn51y+Gv3//Mvz/T1pSAgAAAP//os0EB7QRxykkyqAeVcQgZuXDwC4ux8DAxMzw/+dXhr/fPjP8/faJgeHfX4Z3h9czfH1yh+EvIxPD3z9/SE5CAAAAAP//oskEByMjI2Rs8x8jg4CCFgOHlDLD1/dvGL68eMTw/dl9hl+vHjL8fv+K4f/vnwwcsuoMTJy8DH///GL4z8oCzwfEjmAAAAAA//+0mbsRACAMQrmLJ/vPGyIWfmobmYCOB3w7OI4RbarNzJW+NkqFUYIBRDSEBXJV1U7e0epFEwAA//+i2RQTLASZWVgZ2BgZGZiZWRhYoGNN/xj+M/xjYoFm2n8MzGwcDKxsbHibK7gAAAAA//+i6RQTzGhIbPxj+PcPgf8j9Y9hYz/MzPgbjNgAAAAA//+i+RwZsvGwwV5ISQMRYWBggA47ktbmggEAAAAA//+i/SQfGiBkHSmOZ2BgYAAAAAD//6L7NCupDiQEAAAAAP//GvIT3QAAAAD//xryHgAAAAD//wMAIbECF8xAqXsAAAAASUVORK5CYII=" width="24px" height="24px"> <b>jBPM Web Designer loading. Please wait...</b></center>', 
ORYX.I18N.Oryx.notLoggedOn = "Not logged on", ORYX.I18N.Oryx.editorOpenTimeout = "The editor does not seem to be started yet. Please check, whether you have a popup blocker enabled and disable it or allow popups for this site. We will never display any commercials on this site.", 
ORYX.I18N.AddDocker || (ORYX.I18N.AddDocker = {}), ORYX.I18N.AddDocker.group = "Docker", 
ORYX.I18N.AddDocker.add = "Add Docker", ORYX.I18N.AddDocker.addDesc = "Add a Docker to an edge, by clicking on it", 
ORYX.I18N.AddDocker.del = "Delete Docker", ORYX.I18N.AddDocker.delDesc = "Delete a Docker", 
ORYX.I18N.ShapeConnector || (ORYX.I18N.ShapeConnector = {}), ORYX.I18N.ShapeConnector.group = "Connector", 
ORYX.I18N.ShapeConnector.add = "Connect Shapes", ORYX.I18N.ShapeConnector.addDesc = "Connect several nodes by marking them in the desired order", 
ORYX.I18N.SSExtensionLoader || (ORYX.I18N.SSExtensionLoader = {}), ORYX.I18N.SSExtensionLoader.group = "Stencil Set", 
ORYX.I18N.SSExtensionLoader.add = "Add Stencil Set Extension", ORYX.I18N.SSExtensionLoader.addDesc = "Add a stencil set extension", 
ORYX.I18N.SSExtensionLoader.loading = "Loading Stencil Set Extension", ORYX.I18N.SSExtensionLoader.noExt = "There are no extensions available or all available extensions are already loaded.", 
ORYX.I18N.SSExtensionLoader.failed1 = "Loading stencil set extensions configuration failed. The response is not a valid configuration file.", 
ORYX.I18N.SSExtensionLoader.failed2 = "Loading stencil set extension configuration file failed. The request returned an error.", 
ORYX.I18N.SSExtensionLoader.panelTitle = "Stencil Set Extensions", ORYX.I18N.SSExtensionLoader.panelText = "Select the stencil set extensions you want to load.", 
ORYX.I18N.AdHocCC || (ORYX.I18N.AdHocCC = {}), ORYX.I18N.AdHocCC.group = "Ad Hoc", 
ORYX.I18N.AdHocCC.compl = "Edit Completion Condition", ORYX.I18N.AdHocCC.complDesc = "Edit an Ad Hoc Activity's Completion Condition", 
ORYX.I18N.AdHocCC.notOne = "Not exactly one element selected!", ORYX.I18N.AdHocCC.nodAdHocCC = "Selected element has no ad hoc completion condition!", 
ORYX.I18N.AdHocCC.selectTask = "Select a task...", ORYX.I18N.AdHocCC.selectState = "Select a state...", 
ORYX.I18N.AdHocCC.addExp = "Add Expression", ORYX.I18N.AdHocCC.selectDataField = "Select a data field...", 
ORYX.I18N.AdHocCC.enterEqual = "Enter a value that must equal...", ORYX.I18N.AdHocCC.and = "and", 
ORYX.I18N.AdHocCC.or = "or", ORYX.I18N.AdHocCC.not = "not", ORYX.I18N.AdHocCC.clearCC = "Clear Completion Condition", 
ORYX.I18N.AdHocCC.editCC = "Edit Ad-Hoc Completion Condtions", ORYX.I18N.AdHocCC.addExecState = "Add Execution State Expression: ", 
ORYX.I18N.AdHocCC.addDataExp = "Add Data Expression: ", ORYX.I18N.AdHocCC.addLogOp = "Add Logical Operators: ", 
ORYX.I18N.AdHocCC.curCond = "Current Completion Condition: ", ORYX.I18N.AMLSupport || (ORYX.I18N.AMLSupport = {}), 
ORYX.I18N.AMLSupport.group = "EPC", ORYX.I18N.AMLSupport.imp = "Import AML file", 
ORYX.I18N.AMLSupport.impDesc = "Import an Aris 7 AML file", ORYX.I18N.AMLSupport.failed = "Importing AML file failed. Please check, if the selected file is a valid AML file. Error message: ", 
ORYX.I18N.AMLSupport.failed2 = "Importing AML file failed: ", ORYX.I18N.AMLSupport.noRights = "You have no rights to import multiple EPC-Diagrams (Login required).", 
ORYX.I18N.AMLSupport.panelText = "Select an AML (.xml) file to import.", ORYX.I18N.AMLSupport.file = "File", 
ORYX.I18N.AMLSupport.importBtn = "Import AML-File", ORYX.I18N.AMLSupport.get = "Get diagrams...", 
ORYX.I18N.AMLSupport.close = "Close", ORYX.I18N.AMLSupport.title = "Title", ORYX.I18N.AMLSupport.selectDiagrams = "Select the diagram(s) you want to import. <br/> If one model is selected, it will be imported in the current editor, if more than one is selected, those models will directly be stored in the repository.", 
ORYX.I18N.AMLSupport.impText = "Import", ORYX.I18N.AMLSupport.impProgress = "Importing...", 
ORYX.I18N.AMLSupport.cancel = "Cancel", ORYX.I18N.AMLSupport.name = "Name", ORYX.I18N.AMLSupport.allImported = "All imported diagrams.", 
ORYX.I18N.AMLSupport.ok = "Ok", ORYX.I18N.Arrangement || (ORYX.I18N.Arrangement = {}), 
ORYX.I18N.Arrangement.groupZ = "Z-Order", ORYX.I18N.Arrangement.btf = "Bring To Front", 
ORYX.I18N.Arrangement.btfDesc = "Bring to Front", ORYX.I18N.Arrangement.btb = "Bring To Back", 
ORYX.I18N.Arrangement.btbDesc = "Bring To Back", ORYX.I18N.Arrangement.bf = "Bring Forward", 
ORYX.I18N.Arrangement.bfDesc = "Bring Forward", ORYX.I18N.Arrangement.bb = "Bring Backward", 
ORYX.I18N.Arrangement.bbDesc = "Bring Backward", ORYX.I18N.Arrangement.groupA = "Alignment", 
ORYX.I18N.Arrangement.ab = "Alignment Bottom", ORYX.I18N.Arrangement.abDesc = "Bottom", 
ORYX.I18N.Arrangement.am = "Alignment Middle", ORYX.I18N.Arrangement.amDesc = "Middle", 
ORYX.I18N.Arrangement.at = "Alignment Top", ORYX.I18N.Arrangement.atDesc = "Top", 
ORYX.I18N.Arrangement.al = "Alignment Left", ORYX.I18N.Arrangement.alDesc = "Left", 
ORYX.I18N.Arrangement.ac = "Alignment Center", ORYX.I18N.Arrangement.acDesc = "Center", 
ORYX.I18N.Arrangement.ar = "Alignment Right", ORYX.I18N.Arrangement.arDesc = "Right", 
ORYX.I18N.Arrangement.as = "Alignment Same Size", ORYX.I18N.Arrangement.asDesc = "Same Size", 
ORYX.I18N.BPELSupport || (ORYX.I18N.BPELSupport = {}), ORYX.I18N.BPELSupport.group = "BPEL", 
ORYX.I18N.BPELSupport.exp = "Export BPEL", ORYX.I18N.BPELSupport.expDesc = "Export diagram to BPEL", 
ORYX.I18N.BPELSupport.imp = "Import BPEL", ORYX.I18N.BPELSupport.impDesc = "Import a BPEL file", 
ORYX.I18N.BPELSupport.selectFile = "Select a BPEL file to import", ORYX.I18N.BPELSupport.file = "file", 
ORYX.I18N.BPELSupport.impPanel = "Import BPEL file", ORYX.I18N.BPELSupport.impBtn = "Import", 
ORYX.I18N.BPELSupport.content = "content", ORYX.I18N.BPELSupport.close = "Close", 
ORYX.I18N.BPELSupport.error = "Error", ORYX.I18N.BPELSupport.progressImp = "Import...", 
ORYX.I18N.BPELSupport.progressExp = "Export...", ORYX.I18N.BPELSupport.impFailed = "An error while importing occurs! <br/>Please check error message: <br/><br/>", 
ORYX.I18N.BPELLayout || (ORYX.I18N.BPELLayout = {}), ORYX.I18N.BPELLayout.group = "BPELLayout", 
ORYX.I18N.BPELLayout.disable = "disable layout", ORYX.I18N.BPELLayout.disDesc = "disable auto layout plug-in", 
ORYX.I18N.BPELLayout.enable = "enable layout", ORYX.I18N.BPELLayout.enDesc = "enable auto layout plug-in", 
ORYX.I18N.BPEL4Chor2BPELSupport || (ORYX.I18N.BPEL4Chor2BPELSupport = {}), ORYX.I18N.BPEL4Chor2BPELSupport.group = "BPEL4Chor", 
ORYX.I18N.BPEL4Chor2BPELSupport.exp = "Export to BPEL", ORYX.I18N.BPEL4Chor2BPELSupport.expDesc = "Export diagram to BPEL", 
ORYX.I18N.BPEL4ChorSupport || (ORYX.I18N.BPEL4ChorSupport = {}), ORYX.I18N.BPEL4ChorSupport.group = "BPEL4Chor", 
ORYX.I18N.BPEL4ChorSupport.exp = "Export BPEL4Chor", ORYX.I18N.BPEL4ChorSupport.expDesc = "Export diagram to BPEL4Chor", 
ORYX.I18N.BPEL4ChorSupport.imp = "Import BPEL4Chor", ORYX.I18N.BPEL4ChorSupport.impDesc = "Import a BPEL4Chor file", 
ORYX.I18N.BPEL4ChorSupport.gen = "BPEL4Chor generator", ORYX.I18N.BPEL4ChorSupport.genDesc = "generate values of some BPEL4Chor properties if they are already known(e.g. sender of messageLink)", 
ORYX.I18N.BPEL4ChorSupport.selectFile = "Select a BPEL4Chor file to import", ORYX.I18N.BPEL4ChorSupport.file = "file", 
ORYX.I18N.BPEL4ChorSupport.impPanel = "Import BPEL4Chor file", ORYX.I18N.BPEL4ChorSupport.impBtn = "Import", 
ORYX.I18N.BPEL4ChorSupport.content = "content", ORYX.I18N.BPEL4ChorSupport.close = "Close", 
ORYX.I18N.BPEL4ChorSupport.error = "Error", ORYX.I18N.BPEL4ChorSupport.progressImp = "Import...", 
ORYX.I18N.BPEL4ChorSupport.progressExp = "Export...", ORYX.I18N.BPEL4ChorSupport.impFailed = "An error while importing occurs! <br/>Please check error message: <br/><br/>", 
ORYX.I18N.Bpel4ChorTransformation || (ORYX.I18N.Bpel4ChorTransformation = {}), ORYX.I18N.Bpel4ChorTransformation.group = "Export", 
ORYX.I18N.Bpel4ChorTransformation.exportBPEL = "Export BPEL4Chor", ORYX.I18N.Bpel4ChorTransformation.exportBPELDesc = "Export diagram to BPEL4Chor", 
ORYX.I18N.Bpel4ChorTransformation.exportXPDL = "Export XPDL4Chor", ORYX.I18N.Bpel4ChorTransformation.exportXPDLDesc = "Export diagram to XPDL4Chor", 
ORYX.I18N.Bpel4ChorTransformation.warning = "Warning", ORYX.I18N.Bpel4ChorTransformation.wrongValue = 'The changed name must have the value "1" to avoid errors during the transformation to BPEL4Chor', 
ORYX.I18N.Bpel4ChorTransformation.loopNone = 'The loop type of the receive task must be "None" to be transformable to BPEL4Chor', 
ORYX.I18N.Bpel4ChorTransformation.error = "Error", ORYX.I18N.Bpel4ChorTransformation.noSource = "1 with id 2 has no source object.", 
ORYX.I18N.Bpel4ChorTransformation.noTarget = "1 with id 2 has no target object.", 
ORYX.I18N.Bpel4ChorTransformation.transCall = "An error occured during the transformation call. 1:2", 
ORYX.I18N.Bpel4ChorTransformation.loadingXPDL4ChorExport = "Export to XPDL4Chor", 
ORYX.I18N.Bpel4ChorTransformation.loadingBPEL4ChorExport = "Export to BPEL4Chor", 
ORYX.I18N.Bpel4ChorTransformation.noGen = "The transformation input could not be generated: 1\n2\n", 
ORYX.I18N.BPMN2PNConverter = {
    name: "Convert to Petri net",
    desc: "Converts BPMN diagrams to Petri nets",
    group: "Export",
    error: "Error",
    errors: {
        server: "Couldn't import BPNM diagram.",
        noRights: "Don't you have read permissions on given model?",
        notSaved: "Model must be saved and reopened for using Petri net exporter!"
    },
    progress: {
        status: "Status",
        importingModel: "Importing BPMN Model",
        fetchingModel: "Fetching",
        convertingModel: "Converting",
        renderingModel: "Rendering"
    }
}, ORYX.I18N.TransformationDownloadDialog || (ORYX.I18N.TransformationDownloadDialog = {}), 
ORYX.I18N.TransformationDownloadDialog.error = "Error", ORYX.I18N.TransformationDownloadDialog.noResult = "The transformation service did not return a result.", 
ORYX.I18N.TransformationDownloadDialog.errorParsing = "Error During the Parsing of the Diagram.", 
ORYX.I18N.TransformationDownloadDialog.transResult = "Transformation Results", ORYX.I18N.TransformationDownloadDialog.showFile = "Show the result file", 
ORYX.I18N.TransformationDownloadDialog.downloadFile = "Download the result file", 
ORYX.I18N.TransformationDownloadDialog.downloadAll = "Download all result files", 
ORYX.I18N.DesynchronizabilityOverlay || (ORYX.I18N.DesynchronizabilityOverlay = {}), 
ORYX.I18N.DesynchronizabilityOverlay.group = "Overlay", ORYX.I18N.DesynchronizabilityOverlay.name = "Desynchronizability Checker", 
ORYX.I18N.DesynchronizabilityOverlay.desc = "Desynchronizability Checker", ORYX.I18N.DesynchronizabilityOverlay.sync = "The net is desynchronizable.", 
ORYX.I18N.DesynchronizabilityOverlay.error = "The net has 1 syntax errors.", ORYX.I18N.DesynchronizabilityOverlay.invalid = "Invalid answer from server.", 
ORYX.I18N.Edit || (ORYX.I18N.Edit = {}), ORYX.I18N.Edit.group = "Edit", ORYX.I18N.Edit.cut = "Cut", 
ORYX.I18N.Edit.cutDesc = "Cut the selection into a Designer clipboard", ORYX.I18N.Edit.copy = "Copy", 
ORYX.I18N.Edit.copyDesc = "Copy the selection into an Designer clipboard", ORYX.I18N.Edit.paste = "Paste", 
ORYX.I18N.Edit.pasteDesc = "Paste the Designer clipboard to the canvas", ORYX.I18N.Edit.del = "Delete", 
ORYX.I18N.Edit.delDesc = "Delete all selected shapes", ORYX.I18N.EPCSupport || (ORYX.I18N.EPCSupport = {}), 
ORYX.I18N.EPCSupport.group = "EPC", ORYX.I18N.EPCSupport.exp = "Export EPC", ORYX.I18N.EPCSupport.expDesc = "Export diagram to EPML", 
ORYX.I18N.EPCSupport.imp = "Import EPC", ORYX.I18N.EPCSupport.impDesc = "Import an EPML file", 
ORYX.I18N.EPCSupport.progressExp = "Exporting model", ORYX.I18N.EPCSupport.selectFile = "Select an EPML (.empl) file to import.", 
ORYX.I18N.EPCSupport.file = "File", ORYX.I18N.EPCSupport.impPanel = "Import EPML File", 
ORYX.I18N.EPCSupport.impBtn = "Import", ORYX.I18N.EPCSupport.close = "Close", ORYX.I18N.EPCSupport.error = "Error", 
ORYX.I18N.EPCSupport.progressImp = "Import...", ORYX.I18N.ERDFSupport || (ORYX.I18N.ERDFSupport = {}), 
ORYX.I18N.ERDFSupport.exp = "Export to ERDF", ORYX.I18N.ERDFSupport.expDesc = "Export to ERDF", 
ORYX.I18N.ERDFSupport.imp = "Import from ERDF", ORYX.I18N.ERDFSupport.impDesc = "Import from ERDF", 
ORYX.I18N.ERDFSupport.impFailed = "Request for import of ERDF failed.", ORYX.I18N.ERDFSupport.impFailed2 = "An error while importing occurs! <br/>Please check error message: <br/><br/>", 
ORYX.I18N.ERDFSupport.error = "Error", ORYX.I18N.ERDFSupport.noCanvas = "The xml document has no Oryx canvas node included!", 
ORYX.I18N.ERDFSupport.noSS = "The Oryx canvas node has no stencil set definition included!", 
ORYX.I18N.ERDFSupport.wrongSS = "The given stencil set does not fit to the current editor!", 
ORYX.I18N.ERDFSupport.selectFile = "Select an ERDF (.xml) file or type in the ERDF to import it!", 
ORYX.I18N.ERDFSupport.file = "File", ORYX.I18N.ERDFSupport.impERDF = "Import ERDF", 
ORYX.I18N.ERDFSupport.impBtn = "Import", ORYX.I18N.ERDFSupport.impProgress = "Importing...", 
ORYX.I18N.ERDFSupport.close = "Close", ORYX.I18N.ERDFSupport.deprTitle = "Really export to eRDF?", 
ORYX.I18N.ERDFSupport.deprText = "Exporting to eRDF is not recommended anymore because the support will be stopped in future versions of the Oryx editor. If possible, export the model to JSON. Do you want to export anyway?", 
ORYX.I18N.jPDLSupport || (ORYX.I18N.jPDLSupport = {}), ORYX.I18N.jPDLSupport.group = "Export", 
ORYX.I18N.jPDLSupport.exp = "Export to jPDL", ORYX.I18N.jPDLSupport.expDesc = "Export to jPDL", 
ORYX.I18N.jPDLSupport.imp = "Import from jPDL", ORYX.I18N.jPDLSupport.impDesc = "Migrate a jPDL File to BPMN2", 
ORYX.I18N.jPDLSupport.impFailedReq = "Request for migration of jPDL failed.", ORYX.I18N.jPDLSupport.impFailedJsonAbort = "Migration aborted.", 
ORYX.I18N.jPDLSupport.loadSseQuestionTitle = "jBPM stencil set extension needs to be loaded", 
ORYX.I18N.jPDLSupport.loadSseQuestionBody = "In order to migrate jPDL, the stencil set extension has to be loaded. Do you want to proceed?", 
ORYX.I18N.jPDLSupport.expFailedReq = "Request for export of model failed.", ORYX.I18N.jPDLSupport.expFailedXml = "Export to jPDL failed. Exporter reported: ", 
ORYX.I18N.jPDLSupport.error = "Error", ORYX.I18N.jPDLSupport.selectFile = "1. Select a jPDL processdefinition.xml file (or type it in)", 
ORYX.I18N.jPDLSupport.selectGpdFile = "2. Select a jPDL gpd.xml file (or type it in)", 
ORYX.I18N.jPDLSupport.file = "Definition file", ORYX.I18N.jPDLSupport.gpdfile = "GPD file", 
ORYX.I18N.jPDLSupport.impJPDL = "Migrate to BPMN2", ORYX.I18N.jPDLSupport.impBtn = "Migrate", 
ORYX.I18N.jPDLSupport.impProgress = "Migrating...", ORYX.I18N.jPDLSupport.close = "Close", 
ORYX.I18N.FromBPMN2Support || (ORYX.I18N.FromBPMN2Support = {}), ORYX.I18N.FromBPMN2Support.selectFile = "Select an BPMN2 file or type in the BPMN2 to import it!", 
ORYX.I18N.FromBPMN2Support.file = "File", ORYX.I18N.FromBPMN2Support.impBPMN2 = "Import BPMN2", 
ORYX.I18N.FromBPMN2Support.impBtn = "Import", ORYX.I18N.FromBPMN2Support.impProgress = "Importing...", 
ORYX.I18N.FromBPMN2Support.close = "Close", ORYX.I18N.FromJSONSupport || (ORYX.I18N.FromJSONSupport = {}), 
ORYX.I18N.FromJSONSupport.selectFile = "Select an JSON file or type in the JSON to import it!", 
ORYX.I18N.FromJSONSupport.file = "File", ORYX.I18N.FromJSONSupport.impBPMN2 = "Import JSON", 
ORYX.I18N.FromJSONSupport.impBtn = "Import", ORYX.I18N.FromJSONSupport.impProgress = "Importing...", 
ORYX.I18N.FromJSONSupport.close = "Close", ORYX.I18N.Bpmn2Bpel || (ORYX.I18N.Bpmn2Bpel = {}), 
ORYX.I18N.Bpmn2Bpel.group = "ExecBPMN", ORYX.I18N.Bpmn2Bpel.show = "Show transformed BPEL", 
ORYX.I18N.Bpmn2Bpel.download = "Download transformed BPEL", ORYX.I18N.Bpmn2Bpel.deploy = "Deploy transformed BPEL", 
ORYX.I18N.Bpmn2Bpel.showDesc = "Transforms BPMN to BPEL and shows the result in a new window.", 
ORYX.I18N.Bpmn2Bpel.downloadDesc = "Transforms BPMN to BPEL and offers to download the result.", 
ORYX.I18N.Bpmn2Bpel.deployDesc = "Transforms BPMN to BPEL and deploys the business process on the BPEL-Engine Apache ODE", 
ORYX.I18N.Bpmn2Bpel.transfFailed = "Request for transformation to BPEL failed.", 
ORYX.I18N.Bpmn2Bpel.ApacheOdeUrlInputTitle = "Apache ODE URL", ORYX.I18N.Bpmn2Bpel.ApacheOdeUrlInputLabelDeploy = "Deploy Process", 
ORYX.I18N.Bpmn2Bpel.ApacheOdeUrlInputLabelCancel = "Cancel", ORYX.I18N.Bpmn2Bpel.ApacheOdeUrlInputPanelText = "Please type-in the URL to the Apache ODE BPEL-Engine. E.g.: http://myserver:8080/ode", 
ORYX.I18N.Save || (ORYX.I18N.Save = {}), ORYX.I18N.Save.group = "File", ORYX.I18N.Save.save = "Save", 
ORYX.I18N.Save.autosave = "Autosave", ORYX.I18N.Save.saveDesc = "Save", ORYX.I18N.Save.autosaveDesc = "Autosave", 
ORYX.I18N.Save.autosaveDesc_on = "Autosave (on)", ORYX.I18N.Save.autosaveDesc_off = "Autosave (off)", 
ORYX.I18N.Save.saveAs = "Save As...", ORYX.I18N.Save.saveAsDesc = "Save As...", 
ORYX.I18N.Save.unsavedData = "There are unsaved data, please save before you leave, otherwise your changes get lost!", 
ORYX.I18N.Save.newProcess = "New Process", ORYX.I18N.Save.saveAsTitle = "Save as...", 
ORYX.I18N.Save.saveBtn = "Save", ORYX.I18N.Save.close = "Close", ORYX.I18N.Save.savedAs = "Saved As", 
ORYX.I18N.Save.saved = "Saved!", ORYX.I18N.Save.failed = "Saving failed.", ORYX.I18N.Save.noRights = "You have no rights to save changes.", 
ORYX.I18N.Save.saving = "Saving", ORYX.I18N.Save.saveAsHint = "The process diagram is stored under:", 
ORYX.I18N.File || (ORYX.I18N.File = {}), ORYX.I18N.File.group = "File", ORYX.I18N.File.print = "Print", 
ORYX.I18N.File.printDesc = "Print current model", ORYX.I18N.File.pdf = "Export as PDF", 
ORYX.I18N.File.pdfDesc = "Export as PDF", ORYX.I18N.File.info = "Info", ORYX.I18N.File.infoDesc = "Info", 
ORYX.I18N.File.genPDF = "Generating PDF", ORYX.I18N.File.genPDFFailed = "Generating PDF failed.", 
ORYX.I18N.File.printTitle = "Print", ORYX.I18N.File.printMsg = "We are currently experiencing problems with the printing function. We recommend using the PDF Export to print the diagram. Do you really want to continue printing?", 
ORYX.I18N.Grouping || (ORYX.I18N.Grouping = {}), ORYX.I18N.Grouping.grouping = "Grouping", 
ORYX.I18N.Grouping.group = "Group", ORYX.I18N.Grouping.groupDesc = "Groups all selected shapes", 
ORYX.I18N.Grouping.ungroup = "Ungroup", ORYX.I18N.Grouping.ungroupDesc = "Deletes the group of all selected Shapes", 
ORYX.I18N.IBPMN2BPMN || (ORYX.I18N.IBPMN2BPMN = {}), ORYX.I18N.IBPMN2BPMN.group = "Export", 
ORYX.I18N.IBPMN2BPMN.name = "IBPMN 2 BPMN Mapping", ORYX.I18N.IBPMN2BPMN.desc = "Convert IBPMN to BPMN", 
ORYX.I18N.Loading || (ORYX.I18N.Loading = {}), ORYX.I18N.Loading.waiting = "Please wait...", 
ORYX.I18N.Pnmlexport || (ORYX.I18N.Pnmlexport = {}), ORYX.I18N.Pnmlexport.group = "Export", 
ORYX.I18N.Pnmlexport.name = "BPMN to PNML", ORYX.I18N.Pnmlexport.desc = "Export as executable PNML and deploy", 
ORYX.I18N.PropertyWindow || (ORYX.I18N.PropertyWindow = {}), ORYX.I18N.PropertyWindow.name = "Name", 
ORYX.I18N.PropertyWindow.value = "Value", ORYX.I18N.PropertyWindow.selected = "selected", 
ORYX.I18N.PropertyWindow.clickIcon = "Click Icon", ORYX.I18N.PropertyWindow.add = "Add", 
ORYX.I18N.PropertyWindow.rem = "Remove", ORYX.I18N.PropertyWindow.complex = "Editor for a Complex Type", 
ORYX.I18N.PropertyWindow.text = "Editor for a Text Type", ORYX.I18N.PropertyWindow.ok = "Ok", 
ORYX.I18N.PropertyWindow.cancel = "Cancel", ORYX.I18N.PropertyWindow.dateFormat = "m/d/y", 
ORYX.I18N.ShapeMenuPlugin || (ORYX.I18N.ShapeMenuPlugin = {}), ORYX.I18N.ShapeMenuPlugin.drag = "Drag", 
ORYX.I18N.ShapeMenuPlugin.clickDrag = "Click or drag", ORYX.I18N.ShapeMenuPlugin.morphMsg = "Morph shape", 
ORYX.I18N.SimplePnmlexport || (ORYX.I18N.SimplePnmlexport = {}), ORYX.I18N.SimplePnmlexport.group = "Export", 
ORYX.I18N.SimplePnmlexport.name = "Export to PNML", ORYX.I18N.SimplePnmlexport.desc = "Export to PNML", 
ORYX.I18N.StepThroughPlugin || (ORYX.I18N.StepThroughPlugin = {}), ORYX.I18N.StepThroughPlugin.group = "Step Through", 
ORYX.I18N.StepThroughPlugin.stepThrough = "Step Through", ORYX.I18N.StepThroughPlugin.stepThroughDesc = "Step through your model", 
ORYX.I18N.StepThroughPlugin.undo = "Undo", ORYX.I18N.StepThroughPlugin.undoDesc = "Undo one Step", 
ORYX.I18N.StepThroughPlugin.error = "Can't step through this diagram.", ORYX.I18N.StepThroughPlugin.executing = "Executing", 
ORYX.I18N.SyntaxChecker || (ORYX.I18N.SyntaxChecker = {}), ORYX.I18N.SyntaxChecker.group = "Verification", 
ORYX.I18N.SyntaxChecker.name = "Validate Process", ORYX.I18N.SyntaxChecker.desc = "Validate Process", 
ORYX.I18N.SyntaxChecker.noErrors = "There are no validation errors.", ORYX.I18N.SyntaxChecker.hasErrors = "Validation error(s) found.", 
ORYX.I18N.SyntaxChecker.invalid = "Invalid answer from server.", ORYX.I18N.SyntaxChecker.checkingMessage = "Validating ...", 
ORYX.I18N.Undo || (ORYX.I18N.Undo = {}), ORYX.I18N.Undo.group = "Undo", ORYX.I18N.Undo.undo = "Undo", 
ORYX.I18N.Undo.undoDesc = "Undo the last action", ORYX.I18N.Undo.redo = "Redo", 
ORYX.I18N.Undo.redoDesc = "Redo the last undone action", ORYX.I18N.Validator || (ORYX.I18N.Validator = {}), 
ORYX.I18N.Validator.checking = "Checking", ORYX.I18N.View || (ORYX.I18N.View = {}), 
ORYX.I18N.View.group = "Zoom", ORYX.I18N.View.zoomIn = "Zoom In", ORYX.I18N.View.zoomInDesc = "Zoom into the model", 
ORYX.I18N.View.zoomOut = "Zoom Out", ORYX.I18N.View.zoomOutDesc = "Zoom out of the model", 
ORYX.I18N.View.zoomStandard = "Zoom Standard", ORYX.I18N.View.zoomStandardDesc = "Zoom to the standard level", 
ORYX.I18N.View.zoomFitToModel = "Zoom fit to model", ORYX.I18N.View.zoomFitToModelDesc = "Zoom to fit the model size", 
ORYX.I18N.View.showInPopout = "Popout", ORYX.I18N.View.showInPopoutDesc = "Show in pop out window", 
ORYX.I18N.View.convertToPDF = "PDF", ORYX.I18N.View.convertToPDFDesc = "Convert to PDF", 
ORYX.I18N.View.convertToPNG = "PNG", ORYX.I18N.View.convertToPNGDesc = "Convert to PNG", 
ORYX.I18N.View.generateTaskForms = "Generate Task Form Templates", ORYX.I18N.View.editProcessForm = "Edit Process Form", 
ORYX.I18N.View.editTaskForm = "Edit Task Form", ORYX.I18N.View.generateTaskFormsDesc = "Generate Task Form Templates", 
ORYX.I18N.View.editProcessFormDesc = "Edit Process Form", ORYX.I18N.View.editTaskFormDesc = "Edit Task Form", 
ORYX.I18N.View.showInfo = "Info", ORYX.I18N.View.showInfoDesc = "Info", ORYX.I18N.View.jbpmgroup = "jBPM", 
ORYX.I18N.View.migratejPDL = "Migrate jPDL 3.2 to BPMN2", ORYX.I18N.View.migratejPDLDesc = "Migrate jPDL 3.2 to BPMN2", 
ORYX.I18N.View.viewDiff = "View diff", ORYX.I18N.View.viewDiffDesc = "View diff between different versions of the process", 
ORYX.I18N.View.viewDiffLoadingVersions = "Loading process versions...", ORYX.I18N.View.connectServiceRepo = "Connect to jBPM service repository", 
ORYX.I18N.View.connectServiceRepoDesc = "Connect to a Service Repository", ORYX.I18N.View.connectServiceRepoDataTitle = "Service Repository Connection", 
ORYX.I18N.View.connectServiceRepoConnecting = "Connecting to a Service Repository...", 
ORYX.I18N.View.installingRepoItem = "Instaling assets from the Service Repository...", 
ORYX.I18N.View.shareProcess = "Share your process", ORYX.I18N.View.shareProcessDesc = "Share your process", 
ORYX.I18N.View.infogroup = "info", ORYX.I18N.XFormsSerialization || (ORYX.I18N.XFormsSerialization = {}), 
ORYX.I18N.XFormsSerialization.group = "XForms Serialization", ORYX.I18N.XFormsSerialization.exportXForms = "XForms Export", 
ORYX.I18N.XFormsSerialization.exportXFormsDesc = "Export XForms+XHTML markup", ORYX.I18N.XFormsSerialization.importXForms = "XForms Import", 
ORYX.I18N.XFormsSerialization.importXFormsDesc = "Import XForms+XHTML markup", ORYX.I18N.XFormsSerialization.noClientXFormsSupport = "No XForms support", 
ORYX.I18N.XFormsSerialization.noClientXFormsSupportDesc = '<h2>Your browser does not support XForms. Please install the <a href="https://addons.mozilla.org/firefox/addon/824" target="_blank">Mozilla XForms Add-on</a> for Firefox.</h2>', 
ORYX.I18N.XFormsSerialization.ok = "Ok", ORYX.I18N.XFormsSerialization.selectFile = "Select a XHTML (.xhtml) file or type in the XForms+XHTML markup to import it!", 
ORYX.I18N.XFormsSerialization.selectCss = "Please insert url of css file", ORYX.I18N.XFormsSerialization.file = "File", 
ORYX.I18N.XFormsSerialization.impFailed = "Request for import of document failed.", 
ORYX.I18N.XFormsSerialization.impTitle = "Import XForms+XHTML document", ORYX.I18N.XFormsSerialization.expTitle = "Export XForms+XHTML document", 
ORYX.I18N.XFormsSerialization.impButton = "Import", ORYX.I18N.XFormsSerialization.impProgress = "Importing...", 
ORYX.I18N.XFormsSerialization.close = "Close", ORYX.I18N.TreeGraphSupport || (ORYX.I18N.TreeGraphSupport = {}), 
ORYX.I18N.TreeGraphSupport.syntaxCheckName = "Syntax Check", ORYX.I18N.TreeGraphSupport.group = "Tree Graph Support", 
ORYX.I18N.TreeGraphSupport.syntaxCheckDesc = "Check the syntax of an tree graph structure", 
ORYX.I18N.QueryEvaluator || (ORYX.I18N.QueryEvaluator = {}), ORYX.I18N.QueryEvaluator.name = "Query Evaluator", 
ORYX.I18N.QueryEvaluator.group = "Verification", ORYX.I18N.QueryEvaluator.desc = "Evaluate query", 
ORYX.I18N.QueryEvaluator.noResult = "Query resulted in no match.", ORYX.I18N.QueryEvaluator.invalidResponse = "Invalid answer from server.", 
ORYX.I18N.PropertyWindow.title = "Properties", ORYX.I18N.ShapeRepository || (ORYX.I18N.ShapeRepository = {}), 
ORYX.I18N.ShapeRepository.title = "Shape Repository", ORYX.I18N.Save.dialogDesciption = "Please enter a name, a description and a comment.", 
ORYX.I18N.Save.dialogLabelTitle = "Title", ORYX.I18N.Save.dialogLabelDesc = "Description", 
ORYX.I18N.Save.dialogLabelType = "Type", ORYX.I18N.Save.dialogLabelComment = "Revision comment", 
ORYX.I18N.Validator.name = "BPMN Validator", ORYX.I18N.Validator.description = "Validation for BPMN", 
ORYX.I18N.SSExtensionLoader.labelImport = "Import", ORYX.I18N.SSExtensionLoader.labelCancel = "Cancel", 
ORYX.I18N.BPMN2XPDL || (ORYX.I18N.BPMN2XPDL = {}), ORYX.I18N.BPMN2XPDL.group = "Export", 
ORYX.I18N.BPMN2XPDL.xpdlExport = "Export to XPDL", ORYX.I18N.BPMN2XPDL.xpdlImport = "Import from XPDL", 
ORYX.I18N.BPMN2XPDL.importGroup = "Import", ORYX.I18N.BPMN2XPDL.selectFile = "Select a XPDL (.xml) file or type in the XPDL to import it!", 
ORYX.I18N.BPMN2XPDL.file = "File", ORYX.I18N.BPMN2XPDL.impXPDL = "Import XPDL", 
ORYX.I18N.BPMN2XPDL.impBtn = "Import", ORYX.I18N.BPMN2XPDL.impProgress = "Importing...", 
ORYX.I18N.BPMN2XPDL.close = "Close", ORYX.I18N.ResourcesSoDAdd || (ORYX.I18N.ResourcesSoDAdd = {}), 
ORYX.I18N.ResourcesSoDAdd.name = "Define Separation of Duties Contraint", ORYX.I18N.ResourcesSoDAdd.group = "Resource Perspective", 
ORYX.I18N.ResourcesSoDAdd.desc = "Define a Separation of Duties constraint for the selected tasks", 
ORYX.I18N.ResourcesSoDShow || (ORYX.I18N.ResourcesSoDShow = {}), ORYX.I18N.ResourcesSoDShow.name = "Show Separation of Duties Constraints", 
ORYX.I18N.ResourcesSoDShow.group = "Resource Perspective", ORYX.I18N.ResourcesSoDShow.desc = "Show Separation of Duties constraints of the selected task", 
ORYX.I18N.ResourcesBoDAdd || (ORYX.I18N.ResourcesBoDAdd = {}), ORYX.I18N.ResourcesBoDAdd.name = "Define Binding of Duties Constraint", 
ORYX.I18N.ResourcesBoDAdd.group = "Resource Perspective", ORYX.I18N.ResourcesBoDAdd.desc = "Define a Binding of Duties Constraint for the selected tasks", 
ORYX.I18N.ResourcesBoDShow || (ORYX.I18N.ResourcesBoDShow = {}), ORYX.I18N.ResourcesBoDShow.name = "Show Binding of Duties Constraints", 
ORYX.I18N.ResourcesBoDShow.group = "Resource Perspective", ORYX.I18N.ResourcesBoDShow.desc = "Show Binding of Duties constraints of the selected task", 
ORYX.I18N.ResourceAssignment || (ORYX.I18N.ResourceAssignment = {}), ORYX.I18N.ResourceAssignment.name = "Resource Assignment", 
ORYX.I18N.ResourceAssignment.group = "Resource Perspective", ORYX.I18N.ResourceAssignment.desc = "Assign resources to the selected task(s)", 
ORYX.I18N.ClearSodBodHighlights || (ORYX.I18N.ClearSodBodHighlights = {}), ORYX.I18N.ClearSodBodHighlights.name = "Clear Highlights and Overlays", 
ORYX.I18N.ClearSodBodHighlights.group = "Resource Perspective", ORYX.I18N.ClearSodBodHighlights.desc = "Remove all Separation and Binding of Duties Highlights/ Overlays", 
ORYX.I18N.Perspective || (ORYX.I18N.Perspective = {}), ORYX.I18N.Perspective.no = "No Perspective", 
ORYX.I18N.Perspective.noTip = "Unload the current perspective", ORYX.I18N.JSONSupport = {
    imp: {
        name: "Import from JSON",
        desc: "Imports a model from JSON",
        group: "Export",
        selectFile: "Select an JSON (.json) file or type in JSON to import it!",
        file: "File",
        btnImp: "Import",
        btnClose: "Close",
        progress: "Importing ...",
        syntaxError: "Syntax error"
    },
    exp: {
        name: "Export to JSON",
        desc: "Exports current model to JSON",
        group: "Export"
    }
}, ORYX.I18N.TBPMSupport = {
    imp: {
        name: "Import from PNG/JPEG",
        desc: "Imports a model from a TPBM photo",
        group: "Export",
        selectFile: "Select an image (.png/.jpeg) file!",
        file: "File",
        btnImp: "Import",
        btnClose: "Close",
        progress: "Importing ...",
        syntaxError: "Syntax error",
        impFailed: "Request for import of document failed.",
        confirm: "Confirm import of highlighted shapes!"
    }
}, ORYX.I18N.BPMN2XHTML || (ORYX.I18N.BPMN2XHTML = {}), ORYX.I18N.BPMN2XHTML.group = "Export", 
ORYX.I18N.BPMN2XHTML.XHTMLExport = "Export XHTML Documentation", ORYX.I18N.JSONImport || (ORYX.I18N.JSONImport = {}), 
ORYX.I18N.JSONImport.title = "JSON Import", ORYX.I18N.JSONImport.wrongSS = "The stencil set of the imported file ({0}) does not match to the loaded stencil set ({1}).", 
ORYX.I18N.JSONImport.invalidJSON = "The JSON to import is invalid.", ORYX.I18N.Feedback || (ORYX.I18N.Feedback = {}), 
ORYX.I18N.Feedback.name = "Feedback", ORYX.I18N.Feedback.desc = "Contact us for any kind of feedback!", 
ORYX.I18N.Feedback.pTitle = "Contact us for any kind of feedback!", ORYX.I18N.Feedback.pName = "Name", 
ORYX.I18N.Feedback.pEmail = "E-Mail", ORYX.I18N.Feedback.pSubject = "Subject", ORYX.I18N.Feedback.pMsg = "Description/Message", 
ORYX.I18N.Feedback.pEmpty = "* Please provide as detailed information as possible so that we can understand your request.\n* For bug reports, please list the steps how to reproduce the problem and describe the output you expected.", 
ORYX.I18N.Feedback.pAttach = "Attach current model", ORYX.I18N.Feedback.pAttachDesc = "This information can be helpful for debugging purposes. If your model contains some sensitive data, remove it before or uncheck this behavior.", 
ORYX.I18N.Feedback.pBrowser = "Information about your browser and environment", 
ORYX.I18N.Feedback.pBrowserDesc = "This information has been auto-detected from your browser. It can be helpful if you encountered a bug associated with browser-specific behavior.", 
ORYX.I18N.Feedback.submit = "Send Message", ORYX.I18N.Feedback.sending = "Sending message ...", 
ORYX.I18N.Feedback.success = "Success", ORYX.I18N.Feedback.successMsg = "Thank you for your feedback!", 
ORYX.I18N.Feedback.failure = "Failure", ORYX.I18N.Feedback.failureMsg = "Unfortunately, the message could not be sent. This is our fault! Please try again or contact someone at http://code.google.com/p/oryx-editor/", 
ORYX.I18N.Feedback.name = "Feedback", ORYX.I18N.Feedback.failure = "Failure", ORYX.I18N.Feedback.failureMsg = "Unfortunately, the message could not be sent. This is our fault! Please try again or contact someone at http://code.google.com/p/oryx-editor/", 
ORYX.I18N.Feedback.submit = "Send Message", ORYX.I18N.Feedback.emailDesc = "Your e-mail address?", 
ORYX.I18N.Feedback.titleDesc = "Summarize your message with a short title", ORYX.I18N.Feedback.descriptionDesc = "Describe your idea, question, or problem.", 
ORYX.I18N.Feedback.info = '<p>Oryx is a research platform intended to support scientists in the field of business process management and beyond with a flexible, extensible tool to validate research theses and conduct experiments.</p><p>We are happy to provide you with the <a href="http://bpt.hpi.uni-potsdam.de/Oryx/ReleaseNotes" target="_blank"> latest technology and advancements</a> of our platform. <a href="http://bpt.hpi.uni-potsdam.de/Oryx/DeveloperNetwork" target="_blank">We</a> work hard to provide you with a reliable system, even though you may experience small hiccups from time to time.</p><p>If you have ideas how to improve Oryx, have a question related to the platform, or want to report a problem: <strong>Please, let us know. Here.</strong></p>', 
ORYX.I18N.Feedback.subjects = [ {
    id: "question",
    name: "Question",
    description: "Ask your question here! \nPlease give us as much information as possible, so we don't have to bother you with more questions, before we can give an answer.",
    info: ""
}, {
    id: "problem",
    name: "Problem",
    description: "We're sorry for the inconvenience. Give us feedback on the problem, and we'll try to solve it for you. Describe it as detailed as possible, please.",
    info: ""
}, {
    id: "idea",
    name: "Idea",
    description: "Share your ideas and thoughts here!",
    info: ""
} ], ORYX.I18N.BPMN2DTRPXMI || (ORYX.I18N.BPMN2DTRPXMI = {}), ORYX.I18N.BPMN2DTRPXMI.group = "Export", 
ORYX.I18N.BPMN2DTRPXMI.DTRPXMIExport = "Export to XMI (Design Thinking)", ORYX.I18N.BPMN2DTRPXMI.DTRPXMIExportDescription = "Exports current model to XMI (requires stencil set extension 'BPMN Subset for Design Thinking')", 
ORYX.I18N.RDFExport || (ORYX.I18N.RDFExport = {}), ORYX.I18N.RDFExport.group = "Export", 
ORYX.I18N.RDFExport.rdfExport = "Export to RDF", ORYX.I18N.RDFExport.rdfExportDescription = "Exports current model to the XML serialization defined for the Resource Description Framework (RDF)", 
ORYX.I18N.SyntaxChecker.BPMN || (ORYX.I18N.SyntaxChecker.BPMN = {}), ORYX.I18N.SyntaxChecker.BPMN_NO_SOURCE = "An edge must have a source.", 
ORYX.I18N.SyntaxChecker.BPMN_NO_TARGET = "An edge must have a target.", ORYX.I18N.SyntaxChecker.BPMN_DIFFERENT_PROCESS = "Source and target node must be contained in the same process.", 
ORYX.I18N.SyntaxChecker.BPMN_SAME_PROCESS = "Source and target node must be contained in different pools.", 
ORYX.I18N.SyntaxChecker.BPMN_FLOWOBJECT_NOT_CONTAINED_IN_PROCESS = "A flow object must be contained in a process.", 
ORYX.I18N.SyntaxChecker.BPMN_ENDEVENT_WITHOUT_INCOMING_CONTROL_FLOW = "An end event must have an incoming sequence flow.", 
ORYX.I18N.SyntaxChecker.BPMN_STARTEVENT_WITHOUT_OUTGOING_CONTROL_FLOW = "A start event must have an outgoing sequence flow.", 
ORYX.I18N.SyntaxChecker.BPMN_STARTEVENT_WITH_INCOMING_CONTROL_FLOW = "Start events must not have incoming sequence flows.", 
ORYX.I18N.SyntaxChecker.BPMN_ATTACHEDINTERMEDIATEEVENT_WITH_INCOMING_CONTROL_FLOW = "Attached intermediate events must not have incoming sequence flows.", 
ORYX.I18N.SyntaxChecker.BPMN_ATTACHEDINTERMEDIATEEVENT_WITHOUT_OUTGOING_CONTROL_FLOW = "Attached intermediate events must have exactly one outgoing sequence flow.", 
ORYX.I18N.SyntaxChecker.BPMN_ENDEVENT_WITH_OUTGOING_CONTROL_FLOW = "End events must not have outgoing sequence flows.", 
ORYX.I18N.SyntaxChecker.BPMN_EVENTBASEDGATEWAY_BADCONTINUATION = "Event-based gateways must not be followed by gateways or subprocesses.", 
ORYX.I18N.SyntaxChecker.BPMN_NODE_NOT_ALLOWED = "Node type is not allowed.", ORYX.I18N.SyntaxChecker.IBPMN || (ORYX.I18N.SyntaxChecker.IBPMN = {}), 
ORYX.I18N.SyntaxChecker.IBPMN_NO_ROLE_SET = "Interactions must have a sender and a receiver role set", 
ORYX.I18N.SyntaxChecker.IBPMN_NO_INCOMING_SEQFLOW = "This node must have incoming sequence flow.", 
ORYX.I18N.SyntaxChecker.IBPMN_NO_OUTGOING_SEQFLOW = "This node must have outgoing sequence flow.", 
ORYX.I18N.SyntaxChecker.InteractionNet || (ORYX.I18N.SyntaxChecker.InteractionNet = {}), 
ORYX.I18N.SyntaxChecker.InteractionNet_SENDER_NOT_SET = "Sender not set", ORYX.I18N.SyntaxChecker.InteractionNet_RECEIVER_NOT_SET = "Receiver not set", 
ORYX.I18N.SyntaxChecker.InteractionNet_MESSAGETYPE_NOT_SET = "Message type not set", 
ORYX.I18N.SyntaxChecker.InteractionNet_ROLE_NOT_SET = "Role not set", ORYX.I18N.SyntaxChecker.EPC || (ORYX.I18N.SyntaxChecker.EPC = {}), 
ORYX.I18N.SyntaxChecker.EPC_NO_SOURCE = "Each edge must have a source.", ORYX.I18N.SyntaxChecker.EPC_NO_TARGET = "Each edge must have a target.", 
ORYX.I18N.SyntaxChecker.EPC_NOT_CONNECTED = "Node must be connected with edges.", 
ORYX.I18N.SyntaxChecker.EPC_NOT_CONNECTED_2 = "Node must be connected with more edges.", 
ORYX.I18N.SyntaxChecker.EPC_TOO_MANY_EDGES = "Node has too many connected edges.", 
ORYX.I18N.SyntaxChecker.EPC_NO_CORRECT_CONNECTOR = "Node is no correct connector.", 
ORYX.I18N.SyntaxChecker.EPC_MANY_STARTS = "There must be only one start event.", 
ORYX.I18N.SyntaxChecker.EPC_FUNCTION_AFTER_OR = "There must be no functions after a splitting OR/XOR.", 
ORYX.I18N.SyntaxChecker.EPC_PI_AFTER_OR = "There must be no process interface after a splitting OR/XOR.", 
ORYX.I18N.SyntaxChecker.EPC_FUNCTION_AFTER_FUNCTION = "There must be no function after a function.", 
ORYX.I18N.SyntaxChecker.EPC_EVENT_AFTER_EVENT = "There must be no event after an event.", 
ORYX.I18N.SyntaxChecker.EPC_PI_AFTER_FUNCTION = "There must be no process interface after a function.", 
ORYX.I18N.SyntaxChecker.EPC_FUNCTION_AFTER_PI = "There must be no function after a process interface.", 
ORYX.I18N.SyntaxChecker.PetriNet || (ORYX.I18N.SyntaxChecker.PetriNet = {}), ORYX.I18N.SyntaxChecker.PetriNet_NOT_BIPARTITE = "The graph is not bipartite", 
ORYX.I18N.SyntaxChecker.PetriNet_NO_LABEL = "Label not set for a labeled transition", 
ORYX.I18N.SyntaxChecker.PetriNet_NO_ID = "There is a node without id", ORYX.I18N.SyntaxChecker.PetriNet_SAME_SOURCE_AND_TARGET = "Two flow relationships have the same source and target", 
ORYX.I18N.SyntaxChecker.PetriNet_NODE_NOT_SET = "A node is not set for a flowrelationship", 
ORYX.I18N.Edge = "Edge", ORYX.I18N.Node = "Node", ORYX.I18N.SyntaxChecker.notice = "Move the mouse over a red cross icon to see the error message.", 
ORYX.I18N.Validator.result = "Validation Result", ORYX.I18N.Validator.noErrors = "No validation errors found.", 
ORYX.I18N.Validator.bpmnDeadlockTitle = "Deadlock", ORYX.I18N.Validator.bpmnDeadlock = "This node results in a deadlock. There are situations where not all incoming branches are activated.", 
ORYX.I18N.Validator.bpmnUnsafeTitle = "Lack of synchronization", ORYX.I18N.Validator.bpmnUnsafe = "This model suffers from lack of synchronization. The marked element is activated from multiple incoming branches.", 
ORYX.I18N.Validator.bpmnLeadsToNoEndTitle = "Validation Result", ORYX.I18N.Validator.bpmnLeadsToNoEnd = "The process will never reach a final state.", 
ORYX.I18N.Validator.syntaxErrorsTitle = "Syntax Error", ORYX.I18N.Validator.syntaxErrorsMsg = "The process cannot be validated because it contains syntax errors.", 
ORYX.I18N.Validator.error = "Validation failed", ORYX.I18N.Validator.errorDesc = 'We are sorry, but the validation of your process failed. It would help us identifying the problem, if you sent us your process model via the "Send Feedback" function.', 
ORYX.I18N.Validator.epcIsSound = "<p><b>The EPC is sound, no problems found!</b></p>", 
ORYX.I18N.Validator.epcNotSound = "<p><b>The EPC is <i>NOT</i> sound!</b></p>", 
ORYX.I18N.RESIZE || (ORYX.I18N.RESIZE = {}), ORYX.I18N.RESIZE.tipGrow = "Increase canvas size:", 
ORYX.I18N.RESIZE.tipShrink = "Decrease canvas size:", ORYX.I18N.RESIZE.N = "Top", 
ORYX.I18N.RESIZE.W = "Left", ORYX.I18N.RESIZE.S = "Down", ORYX.I18N.RESIZE.E = "Right", 
ORYX.I18N.PluginLoad || (ORYX.I18N.PluginLoad = {}), ORYX.I18N.PluginLoad.AddPluginButtonName = "Add Plugins", 
ORYX.I18N.PluginLoad.AddPluginButtonDesc = "Add additional Plugins dynamically", 
ORYX.I18N.PluginLoad.loadErrorTitle = "Loading Error", ORYX.I18N.PluginLoad.loadErrorDesc = "Unable to load Plugin. \n Error:\n", 
ORYX.I18N.PluginLoad.WindowTitle = "Add additional Plugins", ORYX.I18N.PluginLoad.NOTUSEINSTENCILSET = "Not allowed in this Stencilset!", 
ORYX.I18N.PluginLoad.REQUIRESTENCILSET = "Require another Stencilset!", ORYX.I18N.PluginLoad.NOTFOUND = "Pluginname not found!", 
ORYX.I18N.PluginLoad.YETACTIVATED = "Plugin is yet activated!", ORYX.I18N.Layouting || (ORYX.I18N.Layouting = {}), 
ORYX.I18N.Layouting.doing = "Layouting...", ORYX.I18N.SyntaxChecker.MULT_ERRORS = "Multiple Errors", 
ORYX.I18N.PropertyWindow || (ORYX.I18N.PropertyWindow = {}), ORYX.I18N.PropertyWindow.oftenUsed = "Common", 
ORYX.I18N.PropertyWindow.moreProps = "Extra", ORYX.I18N.PropertyWindow.simulationProps = "Simulation", 
ORYX.I18N.Bpmn2_0Serialization || (ORYX.I18N.Bpmn2_0Serialization = {}), ORYX.I18N.Bpmn2_0Serialization.show = "Show BPMN 2.0 DI XML", 
ORYX.I18N.Bpmn2_0Serialization.showDesc = "Show BPMN 2.0 DI XML of the current BPMN 2.0 model", 
ORYX.I18N.Bpmn2_0Serialization.download = "Download BPMN 2.0 DI XML", ORYX.I18N.Bpmn2_0Serialization.downloadDesc = "Download BPMN 2.0 DI XML of the current BPMN 2.0 model", 
ORYX.I18N.Bpmn2_0Serialization.serialFailed = "An error occurred while generating the BPMN 2.0 DI XML Serialization.", 
ORYX.I18N.Bpmn2_0Serialization.group = "BPMN 2.0", ORYX.I18N.SyntaxChecker.BPMN2 || (ORYX.I18N.SyntaxChecker.BPMN2 = {}), 
ORYX.I18N.SyntaxChecker.BPMN2_DATA_INPUT_WITH_INCOMING_DATA_ASSOCIATION = "A Data Input must not have any incoming Data Associations.", 
ORYX.I18N.SyntaxChecker.BPMN2_DATA_OUTPUT_WITH_OUTGOING_DATA_ASSOCIATION = "A Data Output must not have any outgoing Data Associations.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_TARGET_WITH_TOO_MANY_INCOMING_SEQUENCE_FLOWS = "Targets of Event-based Gateways may only have one incoming Sequence Flow.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_WITH_TOO_LESS_OUTGOING_SEQUENCE_FLOWS = "An Event-based Gateway must have two or more outgoing Sequence Flows.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_EVENT_TARGET_CONTRADICTION = "If Message Intermediate Events are used in the configuration, then Receive Tasks must not be used and vice versa.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_WRONG_TRIGGER = "Only the following Intermediate Event triggers are valid: Message, Signal, Timer, Conditional and Multiple.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_WRONG_CONDITION_EXPRESSION = "The outgoing Sequence Flows of the Event Gateway must not have a condition expression.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_NOT_INSTANTIATING = "The Gateway does not meet the conditions to instantiate the process. Please use a start event or an instantiating attribute for the gateway.", 
ORYX.I18N.SyntaxChecker.BPMN2_GATEWAYDIRECTION_MIXED_FAILURE = "The Gateway must have both multiple incoming and outgoing Sequence Flows.", 
ORYX.I18N.SyntaxChecker.BPMN2_GATEWAYDIRECTION_CONVERGING_FAILURE = "The Gateway must have multiple incoming but most NOT have multiple outgoing Sequence Flows.", 
ORYX.I18N.SyntaxChecker.BPMN2_GATEWAYDIRECTION_DIVERGING_FAILURE = "The Gateway must NOT have multiple incoming but must have multiple outgoing Sequence Flows.", 
ORYX.I18N.SyntaxChecker.BPMN2_GATEWAY_WITH_NO_OUTGOING_SEQUENCE_FLOW = "A Gateway must have a minimum of one outgoing Sequence Flow.", 
ORYX.I18N.SyntaxChecker.BPMN2_RECEIVE_TASK_WITH_ATTACHED_EVENT = "Receive Tasks used in Event Gateway configurations must not have any attached Intermediate Events.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_SUBPROCESS_BAD_CONNECTION = "An Event Subprocess must not have any incoming or outgoing Sequence Flow.", 
ORYX.I18N.SyntaxChecker.BPMN_MESSAGE_FLOW_NOT_CONNECTED = "At least one side of the Message Flow has to be connected.", 
ORYX.I18N.Bpmn2_0Serialization["import"] = "Import from BPMN 2.0 DI XML", ORYX.I18N.Bpmn2_0Serialization.importDesc = "Import a BPMN 2.0 model from a file or XML String", 
ORYX.I18N.Bpmn2_0Serialization.selectFile = "Select a (*.bpmn) file or type in BPMN 2.0 DI XML to import it!", 
ORYX.I18N.Bpmn2_0Serialization.file = "File:", ORYX.I18N.Bpmn2_0Serialization.name = "Import from BPMN 2.0 DI XML", 
ORYX.I18N.Bpmn2_0Serialization.btnImp = "Import", ORYX.I18N.Bpmn2_0Serialization.progress = "Importing BPMN 2.0 DI XML ...", 
ORYX.I18N.Bpmn2_0Serialization.btnClose = "Close", ORYX.I18N.Bpmn2_0Serialization.error = "An error occurred while importing BPMN 2.0 DI XML", 
ORYX.I18N.SyntaxChecker.BPMN2_TOO_MANY_INITIATING_MESSAGES = "A Choreography Activity may only have one initiating message.", 
ORYX.I18N.SyntaxChecker.BPMN_MESSAGE_FLOW_NOT_ALLOWED = "A Message Flow is not allowed here.", 
ORYX.I18N.SyntaxChecker.BPMN2_EVENT_BASED_WITH_TOO_LESS_INCOMING_SEQUENCE_FLOWS = "An Event-based Gateway that is not instantiating must have a minimum of one incoming Sequence Flow.", 
ORYX.I18N.SyntaxChecker.BPMN2_TOO_FEW_INITIATING_PARTICIPANTS = "A Choreography Activity must have one initiating Participant (white).", 
ORYX.I18N.SyntaxChecker.BPMN2_TOO_MANY_INITIATING_PARTICIPANTS = "A Choreography Acitivity must not have more than one initiating Participant (white).", 
ORYX.I18N.SyntaxChecker.COMMUNICATION_AT_LEAST_TWO_PARTICIPANTS = "The communication must be connected to at least two participants.", 
ORYX.I18N.SyntaxChecker.MESSAGEFLOW_START_MUST_BE_PARTICIPANT = "The message flow's source must be a participant.", 
ORYX.I18N.SyntaxChecker.MESSAGEFLOW_END_MUST_BE_PARTICIPANT = "The message flow's target must be a participant.", 
ORYX.I18N.SyntaxChecker.CONV_LINK_CANNOT_CONNECT_CONV_NODES = "The conversation link must connect a communication or sub conversation node with a participant.", 
ORYX.I18N.Bpmn2_0Serialization.xpdlShow = "Show XPDL 2.2", ORYX.I18N.Bpmn2_0Serialization.xpdlShowDesc = "Shows the XPDL 2.2 based on BPMN 2.0 XML (by XSLT)", 
ORYX.I18N.Bpmn2_0Serialization.xpdlDownload = "Download as XPDL 2.2", ORYX.I18N.Bpmn2_0Serialization.xpdlDownloadDesc = "Download the XPDL 2.2 based on BPMN 2.0 XML (by XSLT)", 
ORYX.I18N.cpntoolsSupport || (ORYX.I18N.cpntoolsSupport = {}), ORYX.I18N.cpntoolsSupport.serverConnectionFailed = "Connection to server failed.", 
ORYX.I18N.cpntoolsSupport.importTask = "Select an CPN file (.cpn) or type in the CPN XML structure in order to import it!", 
ORYX.I18N.cpntoolsSupport.File = "File:", ORYX.I18N.cpntoolsSupport.cpn = "CPN", 
ORYX.I18N.cpntoolsSupport.title = "CPN Oryx", ORYX.I18N.cpntoolsSupport.importLable = "Import", 
ORYX.I18N.cpntoolsSupport.close = "Close", ORYX.I18N.cpntoolsSupport.wrongCPNFile = "Not chosen correct CPN - File.", 
ORYX.I18N.cpntoolsSupport.noPageSelection = "No page has been selected.", ORYX.I18N.cpntoolsSupport.group = "Export", 
ORYX.I18N.cpntoolsSupport.importProgress = "Importing ...", ORYX.I18N.cpntoolsSupport.exportProgress = "Exporting ...", 
ORYX.I18N.cpntoolsSupport.exportDescription = "Export to CPN Tools", ORYX.I18N.cpntoolsSupport.importDescription = "Import from CPN Tools", 
ORYX.I18N.BPMN2YAWLMapper || (ORYX.I18N.BPMN2YAWLMapper = {}), ORYX.I18N.BPMN2YAWLMapper.group = "Export", 
ORYX.I18N.BPMN2YAWLMapper.name = "YAWL Export", ORYX.I18N.BPMN2YAWLMapper.desc = 'Map this diagram to YAWL and export it, please ensure "BPMN Subset for mapping to YAWL" is loaded', 
XMLNS = {
    ATOM: "http://www.w3.org/2005/Atom",
    XHTML: "http://www.w3.org/1999/xhtml",
    ERDF: "http://purl.org/NET/erdf/profile",
    RDFS: "http://www.w3.org/2000/01/rdf-schema#",
    RDF: "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    RAZIEL: "http://b3mn.org/Raziel",
    SCHEMA: ""
};

var Kickstart = {
    started: !1,
    callbacks: [],
    alreadyLoaded: [],
    PATH: "",
    load: function() {
        Kickstart.kick();
    },
    kick: function() {
        Kickstart.started || (Kickstart.started = !0, Kickstart.callbacks.each(function(e) {
            window.setTimeout(e, 1);
        }));
    },
    register: function(callback) {
        with (Kickstart) started ? window.setTimeout(callback, 1) : Kickstart.callbacks.push(callback);
    },
    require: function(e) {
        return Kickstart.alreadyLoaded.member(e) ? !1 : Kickstart.include(e);
    },
    include: function(e) {
        var t = document.getElementsByTagNameNS(XMLNS.XHTML, "head")[0], n = document.createElementNS(XMLNS.XHTML, "script");
        return n.setAttributeNS(XMLNS.XHTML, "type", "text/javascript"), n.src = Kickstart.PATH + e, 
        t.appendChild(n), Kickstart.alreadyLoaded.push(e), !0;
    }
};

if (!ORYX) var ORYX = {};

ORYX.CONFIG || (ORYX.CONFIG = {}), ORYX.CONFIG.WEB_URL = "org.jbpm.designer.jBPMDesigner", 
ORYX.CONFIG.MENU_INDEX = {
    File: 1,
    Edit: 2,
    "Z-Order": 3,
    Undo: 4,
    Docker: 5,
    Help: "ZZZZZZ"
}, ORYX.CONFIG.UUID_URL = function(e, t) {
    return void 0 === e && (e = ORYX.UUID), void 0 === t && (t = ORYX.PROFILE), ORYX.PATH + "uuidRepository?uuid=" + e + "&profile=" + t + "&pp=" + ORYX.PREPROCESSING;
}, ORYX.CONFIG.TRANSFORMER_URL = function(e, t) {
    return void 0 === e && (e = ORYX.UUID), void 0 === t && (t = ORYX.PROFILE), ORYX.PATH + "transformer?uuid=" + e + "&profile=" + t;
}, ORYX.CONFIG.TASKFORMS_URL = function(e, t) {
    return void 0 === e && (e = ORYX.UUID), void 0 === t && (t = ORYX.PROFILE), ORYX.PATH + "taskforms?uuid=" + e + "&profile=" + t;
}, ORYX.CONFIG.UUID_AUTOSAVE_INTERVAL = 12e4, ORYX.CONFIG.UUID_AUTOSAVE_DEFAULT = !1, 
ORYX.CONFIG.VERSION_URL = ORYX.CONFIG.ROOT_PATH + "VERSION", ORYX.CONFIG.LICENSE_URL = ORYX.CONFIG.ROOT_PATH + "LICENSE", 
ORYX.CONFIG.SERVER_HANDLER_ROOT = "", ORYX.CONFIG.STENCILSET_HANDLER = ORYX.CONFIG.SERVER_HANDLER_ROOT + "", 
ORYX.CONFIG.MODE_READONLY = "readonly", ORYX.CONFIG.MODE_FULLSCREEN = "fullscreen", 
ORYX.CONFIG.SHOW_GRIDLINE = !0, ORYX.CONFIG.DISABLE_GRADIENT = !1, ORYX.CONFIG.PLUGINS_ENABLED = !0, 
ORYX.CONFIG.PLUGINS_CONFIG = ORYX.CONFIG.ROOT_PATH + "plugins", ORYX.CONFIG.PROFILE_PATH = ORYX.CONFIG.ROOT_PATH + "profiles/", 
ORYX.CONFIG.PLUGINS_FOLDER = "Plugins/", ORYX.CONFIG.PDF_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "pdf", 
ORYX.CONFIG.PNML_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "pnml", ORYX.CONFIG.SIMPLE_PNML_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "simplepnmlexporter", 
ORYX.CONFIG.DESYNCHRONIZABILITY_URL = ORYX.CONFIG.ROOT_PATH + "desynchronizability", 
ORYX.CONFIG.IBPMN2BPMN_URL = ORYX.CONFIG.ROOT_PATH + "ibpmn2bpmn", ORYX.CONFIG.BPMN2YAWL_URL = ORYX.CONFIG.ROOT_PATH + "bpmn2yawl", 
ORYX.CONFIG.QUERYEVAL_URL = ORYX.CONFIG.ROOT_PATH + "query", ORYX.CONFIG.SYNTAXCHECKER_URL = ORYX.CONFIG.ROOT_PATH + "syntaxchecker", 
ORYX.CONFIG.VALIDATOR_URL = ORYX.CONFIG.ROOT_PATH + "validator", ORYX.CONFIG.AUTO_LAYOUTER_URL = ORYX.CONFIG.ROOT_PATH + "layouter", 
ORYX.CONFIG.SS_EXTENSIONS_FOLDER = ORYX.CONFIG.ROOT_PATH + "stencilsets/extensions/", 
ORYX.CONFIG.SS_EXTENSIONS_CONFIG = ORYX.CONFIG.ROOT_PATH + "stencilsets/extensions/extensions.json", 
ORYX.CONFIG.ORYX_NEW_URL = "/new", ORYX.CONFIG.STEP_THROUGH = ORYX.CONFIG.ROOT_PATH + "stepthrough", 
ORYX.CONFIG.STEP_THROUGH_CHECKER = ORYX.CONFIG.ROOT_PATH + "stepthroughchecker", 
ORYX.CONFIG.XFORMS_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "xformsexport", ORYX.CONFIG.XFORMS_EXPORT_ORBEON_URL = ORYX.CONFIG.ROOT_PATH + "xformsexport-orbeon", 
ORYX.CONFIG.XFORMS_IMPORT_URL = ORYX.CONFIG.ROOT_PATH + "xformsimport", ORYX.CONFIG.BPEL_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "bpelexporter", 
ORYX.CONFIG.BPEL4CHOR_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "bpel4chorexporter", 
ORYX.CONFIG.BPEL4CHOR2BPEL_EXPORT_URL = ORYX.CONFIG.ROOT_PATH + "bpel4chor2bpelexporter", 
ORYX.CONFIG.TREEGRAPH_SUPPORT = ORYX.CONFIG.ROOT_PATH + "treegraphsupport", ORYX.CONFIG.XPDL4CHOR2BPEL4CHOR_TRANSFORMATION_URL = ORYX.CONFIG.ROOT_PATH + "xpdl4chor2bpel4chor", 
ORYX.CONFIG.RESOURCE_LIST = ORYX.CONFIG.ROOT_PATH + "resourceList", ORYX.CONFIG.BPMN_LAYOUTER = ORYX.CONFIG.ROOT_PATH + "bpmnlayouter", 
ORYX.CONFIG.EPC_LAYOUTER = ORYX.CONFIG.ROOT_PATH + "epclayouter", ORYX.CONFIG.BPMN2MIGRATION = ORYX.CONFIG.ROOT_PATH + "bpmn2migration", 
ORYX.CONFIG.BPMN20_SCHEMA_VALIDATION_ON = !0, ORYX.CONFIG.JPDLIMPORTURL = ORYX.CONFIG.ROOT_PATH + "jpdlimporter", 
ORYX.CONFIG.JPDLEXPORTURL = ORYX.CONFIG.ROOT_PATH + "jpdlexporter", ORYX.CONFIG.CPNTOOLSEXPORTER = ORYX.CONFIG.ROOT_PATH + "cpntoolsexporter", 
ORYX.CONFIG.CPNTOOLSIMPORTER = ORYX.CONFIG.ROOT_PATH + "cpntoolsimporter", ORYX.CONFIG.BPMN2XPDLPATH = ORYX.CONFIG.ROOT_PATH + "bpmn2xpdl", 
ORYX.CONFIG.TBPMIMPORT = ORYX.CONFIG.ROOT_PATH + "tbpmimport", ORYX.CONFIG.NAMESPACE_ORYX = "http://www.b3mn.org/oryx", 
ORYX.CONFIG.NAMESPACE_SVG = "http://www.w3.org/2000/svg", ORYX.CONFIG.CANVAS_WIDTH = 3e3, 
ORYX.CONFIG.CANVAS_HEIGHT = 2e3, ORYX.CONFIG.CANVAS_RESIZE_INTERVAL = 300, ORYX.CONFIG.SELECTED_AREA_PADDING = 4, 
ORYX.CONFIG.CANVAS_BACKGROUND_COLOR = "none", ORYX.CONFIG.GRID_DISTANCE = 30, ORYX.CONFIG.GRID_ENABLED = !0, 
ORYX.CONFIG.ZOOM_OFFSET = .1, ORYX.CONFIG.DEFAULT_SHAPE_MARGIN = 60, ORYX.CONFIG.SCALERS_SIZE = 7, 
ORYX.CONFIG.MINIMUM_SIZE = 20, ORYX.CONFIG.MAXIMUM_SIZE = 1e4, ORYX.CONFIG.OFFSET_MAGNET = 15, 
ORYX.CONFIG.OFFSET_EDGE_LABEL_TOP = 14, ORYX.CONFIG.OFFSET_EDGE_LABEL_BOTTOM = 12, 
ORYX.CONFIG.OFFSET_EDGE_BOUNDS = 5, ORYX.CONFIG.COPY_MOVE_OFFSET = 30, ORYX.CONFIG.SHOW_GRIDLINE = !0, 
ORYX.CONFIG.BORDER_OFFSET = 14, ORYX.CONFIG.MAX_NUM_SHAPES_NO_GROUP = 14, ORYX.CONFIG.SHAPEMENU_CREATE_OFFSET_CORNER = 30, 
ORYX.CONFIG.SHAPEMENU_CREATE_OFFSET = 45, ORYX.CONFIG.SHAPEMENU_RIGHT = "Oryx_Right", 
ORYX.CONFIG.SHAPEMENU_BOTTOM = "Oryx_Bottom", ORYX.CONFIG.SHAPEMENU_LEFT = "Oryx_Left", 
ORYX.CONFIG.SHAPEMENU_TOP = "Oryx_Top", ORYX.CONFIG.MORPHITEM_DISABLED = "Oryx_MorphItem_disabled", 
ORYX.CONFIG.TYPE_STRING = "string", ORYX.CONFIG.TYPE_BOOLEAN = "boolean", ORYX.CONFIG.TYPE_INTEGER = "integer", 
ORYX.CONFIG.TYPE_FLOAT = "float", ORYX.CONFIG.TYPE_COLOR = "color", ORYX.CONFIG.TYPE_DATE = "date", 
ORYX.CONFIG.TYPE_CHOICE = "choice", ORYX.CONFIG.TYPE_DYNAMICCHOICE = "dynamicchoice", 
ORYX.CONFIG.TYPE_DYNAMICDATAINPUT = "dynamicdatainput", ORYX.CONFIG.TYPE_DYNAMICDATAOUTPUT = "dynamicdatoutput", 
ORYX.CONFIG.TYPE_DYNAMICGATEWAYCONNECTIONS = "dynamicgatewayconnections", ORYX.CONFIG.TYPE_URL = "url", 
ORYX.CONFIG.TYPE_DIAGRAM_LINK = "diagramlink", ORYX.CONFIG.TYPE_COMPLEX = "complex", 
ORYX.CONFIG.TYPE_TEXT = "text", ORYX.CONFIG.TYPE_VARDEF = "vardef", ORYX.CONFIG.TYPE_EXPRESSION = "expression", 
ORYX.CONFIG.TYPE_ACTION = "action", ORYX.CONFIG.TYPE_GLOBAL = "global", ORYX.CONFIG.TYPE_IMPORT = "import", 
ORYX.CONFIG.TYPE_DATAINPUT = "datainput", ORYX.CONFIG.TYPE_DATAOUTPUT = "dataoutput", 
ORYX.CONFIG.TYPE_DATAINPUT_SINGLE = "datainputsingle", ORYX.CONFIG.TYPE_DATAOUTPUT_SINGLE = "dataoutputsingle", 
ORYX.CONFIG.TYPE_DATAASSIGNMENT = "dataassignment", ORYX.CONFIG.TYPE_VISUALDATAASSIGNMENTS = "visualdataassignment", 
ORYX.CONFIG.TYPE_CALLEDELEMENT = "calledelement", ORYX.CONFIG.TYPE_CUSTOM = "custom", 
ORYX.CONFIG.TYPE_REASSIGNMENT = "reassignment", ORYX.CONFIG.TYPE_NOTIFICATIONS = "notifications", 
ORYX.CONFIG.LABEL_LINE_DISTANCE = 2, ORYX.CONFIG.LABEL_DEFAULT_LINE_HEIGHT = 12, 
ORYX.CONFIG.ENABLE_MORPHMENU_BY_HOVER = !0, ORYX.CONFIG.EDITOR_ALIGN_BOTTOM = 1, 
ORYX.CONFIG.EDITOR_ALIGN_MIDDLE = 2, ORYX.CONFIG.EDITOR_ALIGN_TOP = 4, ORYX.CONFIG.EDITOR_ALIGN_LEFT = 8, 
ORYX.CONFIG.EDITOR_ALIGN_CENTER = 16, ORYX.CONFIG.EDITOR_ALIGN_RIGHT = 32, ORYX.CONFIG.EDITOR_ALIGN_SIZE = 48, 
ORYX.CONFIG.EVENT_MOUSEDOWN = "mousedown", ORYX.CONFIG.EVENT_MOUSEUP = "mouseup", 
ORYX.CONFIG.EVENT_MOUSEOVER = "mouseover", ORYX.CONFIG.EVENT_MOUSEOUT = "mouseout", 
ORYX.CONFIG.EVENT_MOUSEMOVE = "mousemove", ORYX.CONFIG.EVENT_DBLCLICK = "dblclick", 
ORYX.CONFIG.EVENT_CLICK = "click", ORYX.CONFIG.EVENT_KEYDOWN = "keydown", ORYX.CONFIG.EVENT_KEYUP = "keyup", 
ORYX.CONFIG.EVENT_LOADED = "editorloaded", ORYX.CONFIG.EVENT_EXECUTE_COMMANDS = "executeCommands", 
ORYX.CONFIG.EVENT_STENCIL_SET_LOADED = "stencilSetLoaded", ORYX.CONFIG.EVENT_SELECTION_CHANGED = "selectionchanged", 
ORYX.CONFIG.EVENT_SHAPEADDED = "shapeadded", ORYX.CONFIG.EVENT_PROPERTY_CHANGED = "propertyChanged", 
ORYX.CONFIG.EVENT_DRAGDROP_START = "dragdrop.start", ORYX.CONFIG.EVENT_SHAPE_MENU_CLOSE = "shape.menu.close", 
ORYX.CONFIG.EVENT_DRAGDROP_END = "dragdrop.end", ORYX.CONFIG.EVENT_RESIZE_START = "resize.start", 
ORYX.CONFIG.EVENT_RESIZE_END = "resize.end", ORYX.CONFIG.EVENT_DRAGDOCKER_DOCKED = "dragDocker.docked", 
ORYX.CONFIG.EVENT_HIGHLIGHT_SHOW = "highlight.showHighlight", ORYX.CONFIG.EVENT_HIGHLIGHT_HIDE = "highlight.hideHighlight", 
ORYX.CONFIG.EVENT_LOADING_ENABLE = "loading.enable", ORYX.CONFIG.EVENT_LOADING_DISABLE = "loading.disable", 
ORYX.CONFIG.EVENT_LOADING_STATUS = "loading.status", ORYX.CONFIG.EVENT_OVERLAY_SHOW = "overlay.show", 
ORYX.CONFIG.EVENT_OVERLAY_HIDE = "overlay.hide", ORYX.CONFIG.EVENT_DICTIONARY_ADD = "dictionary.add", 
ORYX.CONFIG.EVENT_TASKFORM_EDIT = "taskform.edit", ORYX.CONFIG.EVENT_ARRANGEMENT_TOP = "arrangement.setToTop", 
ORYX.CONFIG.EVENT_ARRANGEMENT_BACK = "arrangement.setToBack", ORYX.CONFIG.EVENT_ARRANGEMENT_FORWARD = "arrangement.setForward", 
ORYX.CONFIG.EVENT_ARRANGEMENT_BACKWARD = "arrangement.setBackward", ORYX.CONFIG.EVENT_PROPWINDOW_PROP_CHANGED = "propertyWindow.propertyChanged", 
ORYX.CONFIG.EVENT_LAYOUT_ROWS = "layout.rows", ORYX.CONFIG.EVENT_LAYOUT_BPEL = "layout.BPEL", 
ORYX.CONFIG.EVENT_LAYOUT_BPEL_VERTICAL = "layout.BPEL.vertical", ORYX.CONFIG.EVENT_LAYOUT_BPEL_HORIZONTAL = "layout.BPEL.horizontal", 
ORYX.CONFIG.EVENT_LAYOUT_BPEL_SINGLECHILD = "layout.BPEL.singlechild", ORYX.CONFIG.EVENT_LAYOUT_BPEL_AUTORESIZE = "layout.BPEL.autoresize", 
ORYX.CONFIG.EVENT_AUTOLAYOUT_LAYOUT = "autolayout.layout", ORYX.CONFIG.EVENT_UNDO_EXECUTE = "undo.execute", 
ORYX.CONFIG.EVENT_UNDO_ROLLBACK = "undo.rollback", ORYX.CONFIG.EVENT_BUTTON_UPDATE = "toolbar.button.update", 
ORYX.CONFIG.EVENT_LAYOUT = "layout.dolayout", ORYX.CONFIG.EVENT_COLOR_CHANGE = "color.change", 
ORYX.CONFIG.EVENT_DOCKERDRAG = "dragTheDocker", ORYX.CONFIG.EVENT_SHOW_PROPERTYWINDOW = "propertywindow.show", 
ORYX.CONFIG.EVENT_DRAG_TRACKER_DRAG = "dragTracker.drag", ORYX.CONFIG.EVENT_DRAG_TRACKER_RESIZE = "dragTracker.resize", 
ORYX.CONFIG.EVENT_DROP_SHAPE = "drop.shape", ORYX.CONFIG.EVENT_SHAPE_DELETED = "shape.deleted", 
ORYX.CONFIG.EVENT_FACADE_SELECTION_DELETION_REQUEST = "facade_selection.deletion.request", 
ORYX.CONFIG.EVENT_NODEXML_SHOW = "nodexml.show", ORYX.CONFIG.EVENT_VOICE_COMMAND = "voice.command", 
ORYX.CONFIG.EVENT_SIMULATION_SHOW_RESULTS = "simulation.showresults", ORYX.CONFIG.EVENT_SIMULATION_DISPLAY_GRAPH = "simulation.displaygraph", 
ORYX.CONFIG.EVENT_SIMULATION_BUILD_PATH_SVG = "simulation.buildpathsvg", ORYX.CONFIG.EVENT_SIMULATION_CLEAR_PATH_SVG = "simulation.clearpathsvg", 
ORYX.CONFIG.EVENT_SIMULATION_PATH_SVG_GENERATED = "simulation.pathsvggenerated", 
ORYX.CONFIG.EVENT_SIMULATION_ANNOTATE_PROCESS = "simulation.annotateprocess", ORYX.CONFIG.EVENT_SIMULATION_SHOW_ANNOTATED_PROCESS = "simulation.showannotatedprocess", 
ORYX.CONFIG.EVENT_NOTIFICATION_SHOW = "notification.show", ORYX.CONFIG.VOICE_COMMAND_GENERATE_FORMS = "voice.command.generate.forms", 
ORYX.CONFIG.VOICE_COMMAND_VALIDATE = "voice.command.validate", ORYX.CONFIG.VOICE_COMMAND_GENERATE_IMAGE = "voice.command.generate.image", 
ORYX.CONFIG.VOICE_COMMAND_VIEW_SOURCE = "voice.command.view.source", ORYX.CONFIG.VOICE_COMMAND_ADD_TASK = "voice.command.add.task", 
ORYX.CONFIG.VOICE_COMMAND_ADD_GATEWAY = "voice.command.add.gateway", ORYX.CONFIG.VOICE_COMMAND_ADD_START_EVENT = "voice.command.add.start.event", 
ORYX.CONFIG.VOICE_COMMAND_ADD_END_EVENT = "voice.command.add.end.event", ORYX.CONFIG.VOICE_COMMAND_TASK_TYPE_USER = "voice.command.task.type.user", 
ORYX.CONFIG.VOICE_COMMAND_TASK_TYPE_SCRIPT = "voice.command.task.type.script", ORYX.CONFIG.VOICE_COMMAND_GATEWAY_TYPE_PARALLEL = "voice.command.gateway.type.parallel", 
ORYX.CONFIG.VOICE_ENTRY_GENERATE_FORMS = "create forms", ORYX.CONFIG.VOICE_ENTRY_VALIDATE = "validate", 
ORYX.CONFIG.VOICE_ENTRY_GENERATE_IMAGE = "create image", ORYX.CONFIG.VOICE_ENTRY_VIEW_SOURCE = "show bpmn", 
ORYX.CONFIG.VOICE_ENTRY_ADD_TASK = "task,test,text,that,map,10,chat,pet", ORYX.CONFIG.VOICE_ENTRY_ADD_GATEWAY = "gateway", 
ORYX.CONFIG.VOICE_ENTRY_ADD_START_EVENT = "start,bart,dark", ORYX.CONFIG.VOICE_ENTRY_ADD_END_EVENT = "end,and", 
ORYX.CONFIG.VOICE_ENTRY_TASK_TYPE_USER = "user,used", ORYX.CONFIG.VOICE_ENTRY_TASK_TYPE_SCRIPT = "script,strip,red", 
ORYX.CONFIG.VOICE_ENTRY_GATEWAY_TYPE_PARALLEL = "parallel", ORYX.CONFIG.CREATE_PATTERN = "create.pattern", 
ORYX.CONFIG.SELECTION_HIGHLIGHT_SIZE = 5, ORYX.CONFIG.SELECTION_HIGHLIGHT_COLOR = "#4444FF", 
ORYX.CONFIG.SELECTION_HIGHLIGHT_COLOR2 = "#9999FF", ORYX.CONFIG.SELECTION_HIGHLIGHT_STYLE_CORNER = "corner", 
ORYX.CONFIG.SELECTION_HIGHLIGHT_STYLE_RECTANGLE = "rectangle", ORYX.CONFIG.SELECTION_VALID_COLOR = "#00FF00", 
ORYX.CONFIG.SELECTION_INVALID_COLOR = "#FF0000", ORYX.CONFIG.DOCKER_DOCKED_COLOR = "#00FF00", 
ORYX.CONFIG.DOCKER_UNDOCKED_COLOR = "#FF0000", ORYX.CONFIG.DOCKER_SNAP_OFFSET = 10, 
ORYX.CONFIG.EDIT_OFFSET_PASTE = 10, ORYX.CONFIG.KEY_CODE_X = 88, ORYX.CONFIG.KEY_CODE_C = 67, 
ORYX.CONFIG.KEY_CODE_V = 86, ORYX.CONFIG.KEY_CODE_DELETE = 46, ORYX.CONFIG.KEY_CODE_META = 224, 
ORYX.CONFIG.KEY_CODE_BACKSPACE = 8, ORYX.CONFIG.KEY_CODE_LEFT = 37, ORYX.CONFIG.KEY_CODE_RIGHT = 39, 
ORYX.CONFIG.KEY_CODE_UP = 38, ORYX.CONFIG.KEY_CODE_DOWN = 40, ORYX.CONFIG.KEY_Code_enter = 12, 
ORYX.CONFIG.KEY_Code_left = 37, ORYX.CONFIG.KEY_Code_right = 39, ORYX.CONFIG.KEY_Code_top = 38, 
ORYX.CONFIG.KEY_Code_bottom = 40, ORYX.CONFIG.META_KEY_META_CTRL = "metactrl", ORYX.CONFIG.META_KEY_ALT = "alt", 
ORYX.CONFIG.META_KEY_SHIFT = "shift", ORYX.CONFIG.KEY_ACTION_DOWN = "down", ORYX.CONFIG.KEY_ACTION_UP = "up", 
ORYX.CONFIG.PANEL_RIGHT_COLLAPSED = !0, ORYX.CONFIG.PANEL_LEFT_COLLAPSED = !0, ORYX.CONFIG.STENCIL_MAX_ORDER = 999, 
ORYX.CONFIG.STENCIL_GROUP_ORDER = function() {
    var e = {};
    return e["http://b3mn.org/stencilset/bpmn2.0#"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"].Activities = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"].Artifacts = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Catching Intermediate Events"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Connecting Objects"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Data Objects"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["End Events"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"].Gateways = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Service Tasks"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Start Events"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"].Swimlanes = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Throwing Intermediate Events"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Start Events"] = 1, e["http://b3mn.org/stencilset/bpmn2.0#"]["Catching Intermediate Events"] = 2, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Throwing Intermediate Events"] = 3, e["http://b3mn.org/stencilset/bpmn2.0#"]["End Events"] = 4, 
    e["http://b3mn.org/stencilset/bpmn2.0#"].Gateways = 5, e["http://b3mn.org/stencilset/bpmn2.0#"].Activities = 6, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Service Tasks"] = 7, e["http://b3mn.org/stencilset/bpmn2.0#"]["Connecting Objects"] = 8, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Data Objects"] = 9, e["http://b3mn.org/stencilset/bpmn2.0#"].Swimlanes = 10, 
    e["http://b3mn.org/stencilset/bpmn2.0#"].Artifacts = 11, e["http://b3mn.org/stencilset/bpmn2.0#"].Task = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Reusable Subprocess"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Multiple instances"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Embedded Subprocess"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Data-based Exclusive (XOR) Gateway"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Start Event"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Timer Intermediate Event"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Signal Intermediate Event"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["End Event"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Error End Event"] = {}, e["http://b3mn.org/stencilset/bpmn2.0#"]["Sequence Flow"] = {}, 
    e["http://b3mn.org/stencilset/bpmn2.0#"].Task = 7, e["http://b3mn.org/stencilset/bpmn2.0#"]["Reusable Subprocess"] = 8, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Multiple instances"] = 9, e["http://b3mn.org/stencilset/bpmn2.0#"]["Embedded Subprocess"] = 10, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Data-based Exclusive (XOR) Gateway"] = 6, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Start Event"] = 1, e["http://b3mn.org/stencilset/bpmn2.0#"]["Timer Intermediate Event"] = 2, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Signal Intermediate Event"] = 3, e["http://b3mn.org/stencilset/bpmn2.0#"]["End Event"] = 4, 
    e["http://b3mn.org/stencilset/bpmn2.0#"]["Error End Event"] = 5, e["http://b3mn.org/stencilset/bpmn2.0#"]["Sequence Flow"] = 11, 
    e;
};

var ORYX_LOGLEVEL_TRACE = 5, ORYX_LOGLEVEL_DEBUG = 4, ORYX_LOGLEVEL_INFO = 3, ORYX_LOGLEVEL_WARN = 2, ORYX_LOGLEVEL_ERROR = 1, ORYX_LOGLEVEL_FATAL = 0;

if (!ORYX_LOGLEVEL) var ORYX_LOGLEVEL = 1;

var ORYX_CONFIGURATION_DELAY = 100, ORYX_CONFIGURATION_WAIT_ATTEMPTS = 10;

if (!ORYX) var ORYX = {};

ORYX = Object.extend(ORYX, {
    PATH: ORYX.CONFIG.ROOT_PATH,
    alreadyLoaded: [],
    configrationRetries: 0,
    availablePlugins: [],
    Log: {
        __appenders: [ {
            append: function(e) {
                "undefined" != typeof console && console.log(e);
            }
        } ],
        trace: function() {
            ORYX_LOGLEVEL >= ORYX_LOGLEVEL_TRACE && ORYX.Log.__log("TRACE", arguments);
        },
        debug: function() {
            ORYX_LOGLEVEL >= ORYX_LOGLEVEL_DEBUG && ORYX.Log.__log("DEBUG", arguments);
        },
        info: function() {
            ORYX_LOGLEVEL >= ORYX_LOGLEVEL_INFO && ORYX.Log.__log("INFO", arguments);
        },
        warn: function() {
            ORYX_LOGLEVEL >= ORYX_LOGLEVEL_WARN && ORYX.Log.__log("WARN", arguments);
        },
        error: function() {
            ORYX_LOGLEVEL >= ORYX_LOGLEVEL_ERROR && ORYX.Log.__log("ERROR", arguments);
        },
        fatal: function() {
            ORYX_LOGLEVEL >= ORYX_LOGLEVEL_FATAL && ORYX.Log.__log("FATAL", arguments);
        },
        __log: function(e, t) {
            t[0] = new Date().getTime() + " " + e + " " + t[0];
            var n = printf.apply(null, t);
            ORYX.Log.__appenders.each(function(e) {
                e.append(n);
            });
        },
        addAppender: function(e) {
            ORYX.Log.__appenders.push(e);
        }
    },
    load: function() {
        var e = new Ext.Window({
            renderTo: Ext.getBody(),
            id: "oryx-loading-panel",
            bodyStyle: "padding: 8px;background:white",
            title: ORYX.I18N.Oryx.title,
            width: "auto",
            height: "auto",
            modal: !0,
            resizable: !1,
            closable: !1,
            html: '<span style="font-size:11px;">' + ORYX.I18N.Oryx.pleaseWait + "</span>"
        });
        if (e.show(), ORYX.Log.debug("Oryx begins loading procedure."), "undefined" == typeof Prototype || "undefined" == typeof Element || "undefined" == typeof Element.Methods || parseFloat(Prototype.Version.split(".")[0] + "." + Prototype.Version.split(".")[1]) < 1.5) throw "Application requires the Prototype JavaScript framework >= 1.5.3";
        ORYX.Log.debug("Prototype > 1.5 found."), init();
    }
}), ORYX.Log.debug("Registering Oryx with Kickstart"), Kickstart.register(ORYX.load);

var Clazz = function() {};

Clazz.prototype.construct = function() {}, Clazz.extend = function(e) {
    var t = function() {
        arguments[0] !== Clazz && this.construct.apply(this, arguments);
    }, n = new this(Clazz), i = this.prototype;
    for (var r in e) {
        var s = e[r];
        s instanceof Function && (s.$ = i), n[r] = s;
    }
    return t.prototype = n, t.extend = this.extend, t;
};

var idCounter = 0, ID_PREFIX = "resource";

if (!ORYX) var ORYX = {};

ORYX.Editor = {
    DOMEventListeners: new Hash(),
    selection: [],
    zoomLevel: 1,
    construct: function(e) {
        this._eventsQueue = [], this.loadedPlugins = [], this.pluginsData = [], this.simulationChartData = "", 
        this.simulationEventData = "", this.simulationEventAggregationData = "", this.simulationInstancesData = "", 
        this.simulationHTCostData = "", this.simulationChartTitle = "", this.simulationChartId = "", 
        this.simulationChartNodeName = "", this.simulationPathData = "", this.simulationPathId = "", 
        this.simulationPathSVG = "", this.localStorageSVG = "", this.imagePreviewSVG = "", 
        this.modelMetaData = e;
        var t = e;
        if (e.model && (t = e.model), this.id = t.resourceId, this.id || (this.id = t.id, 
        this.id || (this.id = ORYX.Editor.provideId())), null === t.fullscreen && (t.fullscreen = !0), 
        this.fullscreen = t.fullscreen, this._initEventListener(), ORYX.CONFIG.BACKEND_SWITCH) {
            var n = (t.stencilset.namespace || t.stencilset.url).replace("#", "%23");
            ORYX.Core.StencilSet.loadStencilSet(ORYX.CONFIG.STENCILSET_HANDLER + n, this.id);
        } else {
            var n = t.stencilset.url;
            ORYX.Core.StencilSet.loadStencilSet(n, this.id);
        }
        ORYX.CONFIG.SSEXTS && ORYX.CONFIG.SSEXTS.each(function(e) {
            this.loadSSExtension(e);
        }.bind(this)), this._createCanvas(t.stencil ? t.stencil.id : null, t.properties), 
        this._generateGUI(e);
        var i = !1, r = !1, s = function() {
            i && r && this._finishedLoading();
        }.bind(this);
        ORYX.Editor.makeExtModalWindowKeysave(this._getPluginFacade()), window.setTimeout(function() {
            this.loadPlugins(), i = !0, s();
        }.bind(this), 100), window.setTimeout(function() {
            this.loadSerialized(t), this.getCanvas().update(), r = !0, s();
        }.bind(this), 200);
    },
    _finishedLoading: function() {
        Ext.getCmp("oryx-loading-panel") && Ext.getCmp("oryx-loading-panel").hide(), this.layout.doLayout(), 
        new Ext.dd.DropTarget(this.getCanvas().rootNode.parentNode), ORYX.CONFIG.PANEL_RIGHT_COLLAPSED === !0 && this.layout_regions.east.collapse(), 
        ORYX.CONFIG.PANEL_LEFT_COLLAPSED === !0 && this.layout_regions.west.collapse(), 
        this.handleEvents({
            type: ORYX.CONFIG.EVENT_LOADED
        });
    },
    _initEventListener: function() {
        document.documentElement.addEventListener(ORYX.CONFIG.EVENT_KEYDOWN, this.catchKeyDownEvents.bind(this), !0), 
        document.documentElement.addEventListener(ORYX.CONFIG.EVENT_KEYUP, this.catchKeyUpEvents.bind(this), !0), 
        this._keydownEnabled = !0, this._keyupEnabled = !0, this.DOMEventListeners[ORYX.CONFIG.EVENT_MOUSEDOWN] = [], 
        this.DOMEventListeners[ORYX.CONFIG.EVENT_MOUSEUP] = [], this.DOMEventListeners[ORYX.CONFIG.EVENT_MOUSEOVER] = [], 
        this.DOMEventListeners[ORYX.CONFIG.EVENT_MOUSEOUT] = [], this.DOMEventListeners[ORYX.CONFIG.EVENT_SELECTION_CHANGED] = [], 
        this.DOMEventListeners[ORYX.CONFIG.EVENT_MOUSEMOVE] = [];
    },
    _chartSelected: function(e) {
        this._getPluginFacade().raiseEvent({
            type: ORYX.CONFIG.EVENT_SIMULATION_DISPLAY_GRAPH,
            value: e
        });
    },
    _generateGUI: function() {
        var e = 660, t = this.getCanvas().rootNode.parentNode;
        this.centerContentPanel = Ext.create("Ext.panel.Panel", {
            autoScroll: !0,
            cmargins: {
                left: 0,
                right: 0
            },
            border: !1,
            items: {
                layout: "fit",
                autoHeight: !0,
                el: t
            }
        }), this.resultsChartPanel = Ext.create("Ext.panel.Panel", {
            border: !1,
            id: "simchart",
            html: "<svg></svg>"
        });
        var n = {
            id: "maintabs",
            region: "center",
            cls: "x-panel-editor-center",
            autoScroll: !1,
            cmargins: {
                left: 0,
                right: 0
            },
            activeTab: 0,
            border: !1,
            tabPosition: "top",
            anchor: "100%",
            deferredRender: !1,
            listeners: {
                tabchange: function(e) {
                    this.centerContentTabPannel.doLayout(), e.doLayout();
                }.bind(this)
            },
            items: [ {
                layout: "fit",
                title: "Process Modelling",
                id: "processmodellingtab",
                items: [ this.centerContentPanel ]
            } ]
        };
        this.centerContentTabPannel = Ext.create("Ext.tab.Panel", n), 1 == ORYX.READONLY && Ext.getCmp("maintabs").remove("simulationtab");
        var i = ORYX.CONFIG.PANEL_LEFT_WIDTH || 400;
        1 == ORYX.READONLY && (i = 10), this.layout_regions = {
            north: Ext.create("Ext.panel.Panel", {
                region: "north",
                cls: "x-panel-editor-north",
                autoEl: "div",
                border: !1
            }),
            east: Ext.create("Ext.panel.Panel", {
                region: "east",
                layout: "anchor",
                autoEl: "div",
                border: !1,
                cls: "x-panel-editor-east",
                width: i,
                autoScroll: !0,
                split: !0,
                animate: !0,
                collapsible: !0,
                titleCollapse: !0,
                title: "Properties",
                plugins: new ORYX.Plugins.PanelCollapsedTitlePlugin()
            }),
            south: Ext.create("Ext.panel.Panel", {
                region: "south",
                cls: "x-panel-editor-south",
                autoEl: "div",
                border: !1
            }),
            west: Ext.create("Ext.panel.Panel", {
                region: "west",
                layout: "anchor",
                autoEl: "div",
                border: !1,
                cls: "x-panel-editor-west",
                width: ORYX.CONFIG.PANEL_LEFT_WIDTH || 200,
                autoScroll: !0,
                split: !0,
                animate: !0,
                collapsible: !0,
                titleCollapse: !0,
                title: "Shape Repository",
                plugins: new ORYX.Plugins.PanelCollapsedTitlePlugin()
            }),
            center: this.centerContentTabPannel
        };
        for (var r in this.layout_regions) "center" != r && ORYX.READONLY === !0 && this.layout_regions[r].setVisible(!1);
        var s = {
            layout: "border",
            items: [ this.layout_regions.north, this.layout_regions.east, this.layout_regions.south, this.layout_regions.west, this.layout_regions.center ]
        };
        this.contentviewport = Ext.create("Ext.container.Viewport", s), this.fullscreen ? (alert("This should not happen: trying to render a Viewport"), 
        this.layout = Ext.create("Ext.container.Viewport", s)) : (s.renderTo = this.id, 
        s.height = e, this.layout = Ext.create("Ext.panel.Panel", s)), this._generateHeader(), 
        t.parentNode.setAttributeNS(null, "align", "center"), t.setAttributeNS(null, "align", "left"), 
        this.getCanvas().setSize({
            width: ORYX.CONFIG.CANVAS_WIDTH,
            height: ORYX.CONFIG.CANVAS_HEIGHT
        });
    },
    _generateHeader: function() {
        var e = Ext.create("Ext.panel.Panel", {
            height: 0,
            autoHeight: !1,
            border: !1,
            html: ""
        }), t = ORYX.MashupAPI && ORYX.MashupAPI.isUsed;
        t ? ORYX.MashupAPI.key : "";
        var n = t ? ORYX.MashupAPI.canRun : !1, i = t ? ORYX.MashupAPI.isModelRemote : !0, r = i ? "<img src='" + ORYX.PATH + "images/page_white_put.png'/>" : "", s = t ? "<span class='mashupinfo'><img src='" + ORYX.PATH + "images/" + (n ? "plugin_error" : "plugin") + ".png'/>" + r + "</span>" : "", o = function(t) {
            var n = ORYX.I18N.Oryx.notLoggedOn, i = t && t.identifier && "public" != t.identifier ? decodeURI(t.identifier.gsub('"', "")).replace(/\+/g, " ") : "";
            i.length <= 0 && (i = n);
            var r = "<div id='oryx_editor_header'><a href=\"" + ORYX.CONFIG.WEB_URL + '" target="_blank">' + "<img src='" + ORYX.BASE_FILE_PATH + 'images/oryx.small.gif\' border="0" />' + "</a>" + "<span class='openid " + (n == i ? "not" : "") + "'>" + i + s + "</span>" + "<div style='clear: both;'/>" + "</div>";
            e.body ? e.body.dom.innerHTML = r : e.html = r;
        };
        ORYX.Editor.Cookie.onChange(o), o(ORYX.Editor.Cookie.getParams()), this.addToRegion("north", e);
    },
    addToRegion: function(e, t, n) {
        if (e.toLowerCase && this.layout_regions[e.toLowerCase()]) {
            var i = this.layout_regions[e.toLowerCase()];
            if (i.add(t), ORYX.Log.debug("original dimensions of region %0: %1 x %2", i.region, i.width, i.height), 
            !i.width && t.initialConfig && t.initialConfig.width && (ORYX.Log.debug("resizing width of region %0: %1", i.region, t.initialConfig.width), 
            i.setWidth(t.initialConfig.width)), t.initialConfig && t.initialConfig.height) {
                ORYX.Log.debug("resizing height of region %0: %1", i.region, t.initialConfig.height);
                var r = i.height || 0;
                i.height = t.initialConfig.height + r, i.setHeight(t.initialConfig.height + r);
            }
            return "string" == typeof n && i.setTitle(n), i.ownerCt.doLayout(), 1 == ORYX.READONLY && "center" != i.region || i.show(), 
            Ext.isMac && ORYX.Editor.resizeFix(), i;
        }
        return null;
    },
    getAvailablePlugins: function() {
        var e = ORYX.availablePlugins.clone();
        return e.each(function(e) {
            e.engaged = this.loadedPlugins.find(function(e) {
                return e.type == this.name;
            }.bind(e)) ? !0 : !1;
        }.bind(this)), e;
    },
    loadScript: function(e, t) {
        var n = document.createElement("script");
        n.type = "text/javascript", n.readyState ? n.onreadystatechange = function() {
            ("loaded" == n.readyState || "complete" == n.readyState) && (n.onreadystatechange = null, 
            t());
        } : n.onload = function() {
            t();
        }, n.src = e, document.getElementsByTagName("head")[0].appendChild(n);
    },
    activatePluginByName: function(name, callback, loadTry) {
        var match = this.getAvailablePlugins().find(function(e) {
            return e.name == name;
        });
        if (!match || match.engaged && "false" !== match.engaged) callback(!1, match ? "NOTFOUND" : "YETACTIVATED"); else {
            var loadedStencilSetsNamespaces = this.getStencilSets().keys(), facade = this._getPluginFacade(), newPlugin, me = this;
            if (ORYX.Log.debug("Initializing plugin '%0'", match.name), match.requires && match.requires.namespaces && !match.requires.namespaces.any(function(e) {
                return loadedStencilSetsNamespaces.indexOf(e) >= 0;
            })) callback(!1, "REQUIRESTENCILSET"), ORYX.Log.info("Plugin need a stencilset which is not loaded'", match.name); else if (match.notUsesIn && match.notUsesIn.namespaces && match.notUsesIn.namespaces.any(function(e) {
                return loadedStencilSetsNamespaces.indexOf(e) >= 0;
            })) callback(!1, "NOTUSEINSTENCILSET"), ORYX.Log.info("Plugin need a stencilset which is not loaded'", match.name); else try {
                var className = eval(match.name), newPlugin = new className(facade, match);
                newPlugin.type = match.name, newPlugin.registryChanged && newPlugin.registryChanged(me.pluginsData), 
                newPlugin.onSelectionChanged && me.registerOnEvent(ORYX.CONFIG.EVENT_SELECTION_CHANGED, newPlugin.onSelectionChanged.bind(newPlugin)), 
                this.loadedPlugins.push(newPlugin), this.loadedPlugins.each(function(e) {
                    e.registryChanged && e.registryChanged(this.pluginsData);
                }.bind(me)), callback(!0);
            } catch (e) {
                if (ORYX.Log.warn("Plugin %0 is not available", match.name), loadTry) return callback(!1, "INITFAILED"), 
                void 0;
                this.loadScript("plugins/scripts/" + match.source, this.activatePluginByName.bind(this, match.name, callback, !0));
            }
        }
    },
    loadPlugins: function() {
        var me = this, newPlugins = [], loadedStencilSetsNamespaces = this.getStencilSets().keys(), facade = this._getPluginFacade();
        ORYX.MashupAPI && ORYX.MashupAPI.loadablePlugins && ORYX.MashupAPI.loadablePlugins instanceof Array && (ORYX.availablePlugins = $A(ORYX.availablePlugins).findAll(function(e) {
            return ORYX.MashupAPI.loadablePlugins.include(e.name);
        }), ORYX.MashupAPI.loadablePlugins.each(function(e) {
            ORYX.availablePlugins.find(function(t) {
                return t.name == e;
            }) || ORYX.availablePlugins.push({
                name: e
            });
        })), ORYX.availablePlugins.each(function(value) {
            if (ORYX.Log.debug("Initializing plugin '%0'", value.name), value.requires && value.requires.namespaces && !value.requires.namespaces.any(function(e) {
                return loadedStencilSetsNamespaces.indexOf(e) >= 0;
            }) || value.notUsesIn && value.notUsesIn.namespaces && value.notUsesIn.namespaces.any(function(e) {
                return loadedStencilSetsNamespaces.indexOf(e) >= 0;
            }) || !value.engaged && void 0 !== value.engaged) ORYX.Log.info("Plugin need a stencilset which is not loaded'", value.name); else try {
                var className = eval(value.name);
                if (className) {
                    var plugin = new className(facade, value);
                    plugin.type = value.name, newPlugins.push(plugin), plugin.engaged = !0;
                }
            } catch (e) {
                ORYX.Log.warn("Plugin %0 is not available", value.name);
            }
        }), newPlugins.each(function(e) {
            e.registryChanged && e.registryChanged(me.pluginsData), e.onSelectionChanged && me.registerOnEvent(ORYX.CONFIG.EVENT_SELECTION_CHANGED, e.onSelectionChanged.bind(e));
        }), this.loadedPlugins = newPlugins, Ext.isMac && ORYX.Editor.resizeFix(), this.registerPluginsOnKeyEvents(), 
        this.setSelection();
    },
    _createCanvas: function(e, t) {
        e ? -1 === e.search(/^http/) && (e = this.getStencilSets().values()[0].namespace() + e) : e = this.getStencilSets().values()[0].findRootStencilName();
        var n = ORYX.Core.StencilSet.stencil(e);
        n || ORYX.Log.fatal("Initialisation failed, because the stencil with the type %0 is not part of one of the loaded stencil sets.", e);
        var i = ORYX.Editor.graft("http://www.w3.org/1999/xhtml", null, [ "div" ]);
        if (i.addClassName("ORYX_Editor"), this._canvas = new ORYX.Core.Canvas({
            width: ORYX.CONFIG.CANVAS_WIDTH,
            height: ORYX.CONFIG.CANVAS_HEIGHT,
            eventHandlerCallback: this.handleEvents.bind(this),
            id: this.id,
            parentNode: i
        }, n), t) {
            var r = [];
            for (var s in t) r.push({
                prefix: "oryx",
                name: s,
                value: t[s]
            });
            this._canvas.deserialize(r);
        }
    },
    _getPluginFacade: function() {
        return this._pluginFacade || (this._pluginFacade = {
            activatePluginByName: this.activatePluginByName.bind(this),
            getAvailablePlugins: this.getAvailablePlugins.bind(this),
            offer: this.offer.bind(this),
            getStencilSets: this.getStencilSets.bind(this),
            getRules: this.getRules.bind(this),
            loadStencilSet: this.loadStencilSet.bind(this),
            createShape: this.createShape.bind(this),
            deleteShape: this.deleteShape.bind(this),
            getSelection: this.getSelection.bind(this),
            setSelection: this.setSelection.bind(this),
            updateSelection: this.updateSelection.bind(this),
            getCanvas: this.getCanvas.bind(this),
            importJSON: this.importJSON.bind(this),
            importERDF: this.importERDF.bind(this),
            getERDF: this.getERDF.bind(this),
            getJSON: this.getJSON.bind(this),
            getSerializedJSON: this.getSerializedJSON.bind(this),
            checkParsingErrors: this.checkParsingErrors.bind(this),
            showParsingErrors: this.showParsingErrors.bind(this),
            executeCommands: this.executeCommands.bind(this),
            registerOnEvent: this.registerOnEvent.bind(this),
            unregisterOnEvent: this.unregisterOnEvent.bind(this),
            raiseEvent: this.handleEvents.bind(this),
            enableEvent: this.enableEvent.bind(this),
            disableEvent: this.disableEvent.bind(this),
            eventCoordinates: this.eventCoordinates.bind(this),
            addToRegion: this.addToRegion.bind(this),
            getModelMetaData: this.getModelMetaData.bind(this)
        }), this._pluginFacade;
    },
    executeCommands: function(e) {
        if (e instanceof Array && e.length > 0 && e.all(function(e) {
            return e instanceof ORYX.Core.Command;
        })) {
            this.handleEvents({
                type: ORYX.CONFIG.EVENT_EXECUTE_COMMANDS,
                commands: e
            });
            var t;
            return e.each(function(e) {
                t = e.execute();
            }), t;
        }
    },
    getJSON: function() {
        var e = this.getCanvas().toJSON();
        return e.ssextensions = this.getStencilSets().values()[0].extensions().keys(), e;
    },
    getSerializedJSON: function() {
        return Ext.encode(this.getJSON());
    },
    checkParsingErrors: function() {
        var e = ORYX.EDITOR.getSerializedJSON(), t = new XMLHttpRequest(), n = ORYX.PATH + "uuidRepository", i = "action=checkErrors&pp=" + ORYX.PREPROCESSING + "&profile=" + ORYX.PROFILE + "&data=" + encodeURIComponent(e);
        if (t.open("POST", n, !1), t.setRequestHeader("Content-type", "application/x-www-form-urlencoded"), 
        t.send(i), 200 == t.status) {
            if ("true" == t.responseText) return "true";
            var r = DataManager.serialize(ORYX.EDITOR.getCanvas().getSVGRepresentation(!1)), s = DataManager.serialize(ORYX.EDITOR.getCanvas().getRootNode().cloneNode(!0)), e = ORYX.EDITOR.getSerializedJSON(), o = jsonPath(e.evalJSON(), "$.properties.id");
            return Ext.Ajax.request({
                url: ORYX.PATH + "transformer",
                method: "POST",
                success: function() {},
                failure: function() {
                    Ext.Msg.minWidth = 400, this.facade.raiseEvent({
                        type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
                        ntype: "error",
                        msg: "Failed to save process SVG.",
                        title: ""
                    });
                },
                params: {
                    fsvg: r,
                    rsvg: s,
                    uuid: ORYX.UUID,
                    profile: ORYX.PROFILE,
                    transformto: "svg",
                    processid: o
                }
            }), "false";
        }
        return "true";
    },
    showParsingErrors: function() {
        Ext.Msg.minWidth = 360, Ext.MessageBox.alert("Unable to perform action", "Unable to perform user action due to error(s).<br/>Validate your process before saving, and view server logs to see error details.");
    },
    getERDF: function() {
        var e = DataManager.serializeDOM(this._getPluginFacade());
        return e = '<?xml version="1.0" encoding="utf-8"?><html xmlns="http://www.w3.org/1999/xhtml" xmlns:b3mn="http://b3mn.org/2007/b3mn" xmlns:ext="http://b3mn.org/2007/ext" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:atom="http://b3mn.org/2007/atom+xhtml"><head profile="http://purl.org/NET/erdf/profile"><link rel="schema.dc" href="http://purl.org/dc/elements/1.1/" /><link rel="schema.dcTerms" href="http://purl.org/dc/terms/ " /><link rel="schema.b3mn" href="http://b3mn.org" /><link rel="schema.oryx" href="http://oryx-editor.org/" /><link rel="schema.raziel" href="http://raziel.org/" /><base href="' + location.href.split("?")[0] + '" />' + "</head><body>" + e + "</body></html>";
    },
    importJSON: function(e, t) {
        try {
            e = this.renewResourceIds(e);
        } catch (n) {
            throw n;
        }
        if (!e.stencilset) return this.facade.raiseEvent({
            type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
            ntype: "error",
            msg: ORYX.I18N.JSONImport.invalidJSON,
            title: ORYX.I18N.JSONImport.title
        }), null;
        if (e.stencilset.namespace && e.stencilset.namespace !== this.getCanvas().getStencil().stencilSet().namespace()) return this.facade.raiseEvent({
            type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
            ntype: "error",
            msg: String.format(ORYX.I18N.JSONImport.wrongSS, e.stencilset.namespace, this.getCanvas().getStencil().stencilSet().namespace()),
            title: ORYX.I18N.JSONImport.title
        }), null;
        var i = ORYX.Core.Command.extend({
            construct: function(e, t, n, i) {
                this.jsonObject = e, this.noSelection = n, this.facade = i, this.shapes, this.connections = [], 
                this.parents = new Hash(), this.selection = this.facade.getSelection(), this.loadSerialized = t;
            },
            execute: function() {
                this.shapes ? (this.shapes.each(function(e) {
                    this.parents[e.id].add(e);
                }.bind(this)), this.connections.each(function(e) {
                    e[0].setDockedShape(e[1]), e[0].setReferencePoint(e[2]);
                })) : (this.shapes = this.loadSerialized(this.jsonObject), this.shapes.each(function(e) {
                    if (e.getDockers) {
                        var t = e.getDockers();
                        t && (t.length > 0 && this.connections.push([ t.first(), t.first().getDockedShape(), t.first().referencePoint ]), 
                        t.length > 1 && this.connections.push([ t.last(), t.last().getDockedShape(), t.last().referencePoint ]));
                    }
                    this.parents[e.id] = e.parent;
                }.bind(this))), this.facade.getCanvas().update(), this.noSelection ? this.facade.updateSelection() : this.facade.setSelection(this.shapes);
            },
            rollback: function() {
                var e = this.facade.getSelection();
                this.shapes.each(function(t) {
                    e = e.without(t), this.facade.deleteShape(t);
                }.bind(this)), this.facade.getCanvas().update(), this.facade.setSelection(e);
            }
        }), r = new i(e, this.loadSerialized.bind(this), t, this._getPluginFacade());
        return this.executeCommands([ r ]), r.shapes.clone();
    },
    renewResourceIds: function(e) {
        if ("string" === Ext.type(e)) try {
            var t = e;
            e = Ext.decode(e);
        } catch (n) {
            throw new SyntaxError(n.message);
        } else var t = Ext.encode(e);
        var i = function(e) {
            return e ? e.map(function(e) {
                return i(e.childShapes).concat(e.resourceId);
            }).flatten() : [];
        }, r = i(e.childShapes);
        return r.each(function(e) {
            var n = ORYX.Editor.provideId();
            t = t.gsub('"' + e + '"', '"' + n + '"');
        }), Ext.decode(t);
    },
    importERDF: function(e) {
        var t = this.parseToSerializeObjects(e);
        return t ? this.importJSON(t, !0) : void 0;
    },
    parseToSerializeObjects: function(e) {
        e.normalize && e.normalize();
        try {
            var t = "", n = ORYX.PATH + "lib/extract-rdf.xsl";
            new Ajax.Request(n, {
                asynchronous: !1,
                method: "get",
                onSuccess: function(e) {
                    t = e.responseText;
                }.bind(this),
                onFailure: function(e) {
                    ORYX.Log.error("XSL load failed" + e);
                }.bind(this)
            });
            var i = new DOMParser(), r = e, s = i.parseFromString(t, "text/xml"), o = new XSLTProcessor();
            document.implementation.createDocument("", "", null), o.importStylesheet(s);
            var a = o.transformToFragment(r, document), c = new XMLSerializer().serializeToString(a);
        } catch (l) {
            this.facade.raiseEvent({
                type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
                ntype: "error",
                msg: "Error: " + l,
                title: ""
            });
            var c = "";
        }
        c = c.startsWith("<?xml") ? c : '<?xml version="1.0" encoding="UTF-8"?>' + c;
        var h = new Ajax.Request(ORYX.CONFIG.ROOT_PATH + "rdf2json", {
            method: "POST",
            asynchronous: !1,
            onSuccess: function(e) {
                Ext.decode(e.responseText);
            },
            parameters: {
                rdf: c
            }
        });
        return Ext.decode(h.transport.responseText);
    },
    loadSerialized: function(e) {
        this.getCanvas(), this.loadSSExtensions(e.ssextensions);
        var t = this.getCanvas().addShapeObjects(e.childShapes, this.handleEvents.bind(this));
        if (e.properties) for (key in e.properties) {
            var n = e.properties[key];
            "string" != typeof n && (n = Ext.encode(n)), this.getCanvas().setProperty("oryx-" + key, n);
        }
        return this.getCanvas().updateSize(), t;
    },
    loadSSExtensions: function(e) {
        e && e.each(function(e) {
            this.loadSSExtension(e);
        }.bind(this));
    },
    loadSSExtension: function(e) {
        if (e) {
            var t = this.getStencilSets()[e["extends"]];
            t && (t.addExtension(e), this.getRules().initializeRules(t), this._getPluginFacade().raiseEvent({
                type: ORYX.CONFIG.EVENT_STENCIL_SET_LOADED
            }));
        }
    },
    disableEvent: function(e) {
        if (e == ORYX.CONFIG.EVENT_KEYDOWN && (this._keydownEnabled = !1), e == ORYX.CONFIG.EVENT_KEYUP && (this._keyupEnabled = !1), 
        this.DOMEventListeners.keys().member(e)) {
            var t = this.DOMEventListeners.remove(e);
            this.DOMEventListeners["disable_" + e] = t;
        }
    },
    enableEvent: function(e) {
        if (e == ORYX.CONFIG.EVENT_KEYDOWN && (this._keydownEnabled = !0), e == ORYX.CONFIG.EVENT_KEYUP && (this._keyupEnabled = !0), 
        this.DOMEventListeners.keys().member("disable_" + e)) {
            var t = this.DOMEventListeners.remove("disable_" + e);
            this.DOMEventListeners[e] = t;
        }
    },
    registerOnEvent: function(e, t) {
        this.DOMEventListeners.keys().member(e) || (this.DOMEventListeners[e] = []), this.DOMEventListeners[e].push(t);
    },
    unregisterOnEvent: function(e, t) {
        this.DOMEventListeners.keys().member(e) && (this.DOMEventListeners[e] = this.DOMEventListeners[e].without(t));
    },
    getSelection: function() {
        return this.selection;
    },
    getStencilSets: function() {
        return ORYX.Core.StencilSet.stencilSets(this.id);
    },
    getRules: function() {
        return ORYX.Core.StencilSet.rules(this.id);
    },
    loadStencilSet: function(e) {
        try {
            ORYX.Core.StencilSet.loadStencilSet(e, this.id), this.handleEvents({
                type: ORYX.CONFIG.EVENT_STENCIL_SET_LOADED
            });
        } catch (t) {
            ORYX.Log.warn("Requesting stencil set file failed. (" + t + ")");
        }
    },
    offer: function(e) {
        this.pluginsData.member(e) || this.pluginsData.push(e);
    },
    registerPluginsOnKeyEvents: function() {
        this.pluginsData.each(function(e) {
            e.keyCodes && e.keyCodes.each(function(t) {
                var n = "key.event";
                n += "." + t.keyAction, t.metaKeys && (t.metaKeys.indexOf(ORYX.CONFIG.META_KEY_META_CTRL) > -1 && (n += "." + ORYX.CONFIG.META_KEY_META_CTRL), 
                t.metaKeys.indexOf(ORYX.CONFIG.META_KEY_ALT) > -1 && (n += "." + ORYX.CONFIG.META_KEY_ALT), 
                t.metaKeys.indexOf(ORYX.CONFIG.META_KEY_SHIFT) > -1 && (n += "." + ORYX.CONFIG.META_KEY_SHIFT)), 
                t.keyCode && (n += "." + t.keyCode), ORYX.Log.debug("Register Plugin on Key Event: %0", n), 
                this.registerOnEvent(n, e.functionality);
            }.bind(this));
        }.bind(this));
    },
    setSelection: function(e, t, n) {
        e || (e = []), e = e.compact().findAll(function(e) {
            return e instanceof ORYX.Core.Shape;
        }), e.first() instanceof ORYX.Core.Canvas && (e = []), (n || e.length !== this.selection.length || !this.selection.all(function(t) {
            return e.include(t);
        })) && (this.selection = e, this._subSelection = t, this.handleEvents({
            type: ORYX.CONFIG.EVENT_SELECTION_CHANGED,
            elements: e,
            subSelection: t
        }));
    },
    updateSelection: function() {
        this.setSelection(this.selection, this._subSelection, !0);
    },
    getCanvas: function() {
        return this._canvas;
    },
    createShape: function(e) {
        var t;
        if (e && e.serialize && e.serialize instanceof Array) {
            var n = e.serialize.find(function(e) {
                return "oryx-type" == e.prefix + "-" + e.name;
            }), i = ORYX.Core.StencilSet.stencil(n.value);
            return t = "node" == i.type() ? new ORYX.Core.Node({
                eventHandlerCallback: this.handleEvents.bind(this)
            }, i) : new ORYX.Core.Edge({
                eventHandlerCallback: this.handleEvents.bind(this)
            }, i), this.getCanvas().add(t), t.deserialize(e.serialize), t;
        }
        if (!e || !e.type || !e.namespace) throw "To create a new shape you have to give an argument with type and namespace";
        var r = this.getCanvas(), s = e.type, o = ORYX.Core.StencilSet.stencilSet(e.namespace);
        t = "node" == o.stencil(s).type() ? new ORYX.Core.Node({
            eventHandlerCallback: this.handleEvents.bind(this)
        }, o.stencil(s)) : new ORYX.Core.Edge({
            eventHandlerCallback: this.handleEvents.bind(this)
        }, o.stencil(s)), e.template && (t._jsonStencil.properties = e.template._jsonStencil.properties, 
        t.postProcessProperties()), e.parent && t instanceof ORYX.Core.Node ? e.parent.add(t) : r.add(t);
        var a, c = e.position ? e.position : {
            x: 100,
            y: 200
        };
        if (e.connectingType && e.connectedShape && !(t instanceof ORYX.Core.Edge)) {
            a = new ORYX.Core.Edge({
                eventHandlerCallback: this.handleEvents.bind(this)
            }, o.stencil(e.connectingType)), a.dockers.first().setDockedShape(e.connectedShape);
            var l = e.connectedShape.getDefaultMagnet(), h = l ? l.bounds.center() : e.connectedShape.bounds.midPoint();
            a.dockers.first().setReferencePoint(h), a.dockers.last().setDockedShape(t), a.dockers.last().setReferencePoint(t.getDefaultMagnet().bounds.center()), 
            r.add(a);
        }
        if (t instanceof ORYX.Core.Edge && e.connectedShape) t.dockers.first().setDockedShape(e.connectedShape), 
        e.connectedShape instanceof ORYX.Core.Node ? (t.dockers.first().setReferencePoint(e.connectedShape.getDefaultMagnet().bounds.center()), 
        t.dockers.last().bounds.centerMoveTo(c)) : t.dockers.first().setReferencePoint(e.connectedShape.bounds.midPoint()); else {
            var u = t.bounds;
            t instanceof ORYX.Core.Node && 1 == t.dockers.length && (u = t.dockers.first().bounds), 
            u.centerMoveTo(c);
            var d = u.upperLeft();
            u.moveBy(-Math.min(d.x, 0), -Math.min(d.y, 0));
            var p = u.lowerRight();
            u.moveBy(-Math.max(p.x - r.bounds.width(), 0), -Math.max(p.y - r.bounds.height(), 0));
        }
        return t instanceof ORYX.Core.Edge && t._update(!1), t instanceof ORYX.Core.Edge || this.setSelection([ t ]), 
        a && a.alignDockers && a.alignDockers(), t.alignDockers && t.alignDockers(), t;
    },
    deleteShape: function(e) {
        e && e.parent && (e.parent.remove(e), e.getOutgoingShapes().each(function(t) {
            var n = t.getDockers().first();
            n && n.getDockedShape() == e && n.setDockedShape(void 0);
        }.bind(this)), e.getIncomingShapes().each(function(t) {
            var n = t.getDockers().last();
            n && n.getDockedShape() == e && n.setDockedShape(void 0);
        }.bind(this)), e.getDockers().each(function(e) {
            e.setDockedShape(void 0);
        }), this._getPluginFacade().raiseEvent({
            type: ORYX.CONFIG.EVENT_SHAPE_DELETED,
            value: e
        }));
    },
    getModelMetaData: function() {
        return this.modelMetaData;
    },
    _executeEventImmediately: function(e) {
        this.DOMEventListeners.keys().member(e.event.type) && this.DOMEventListeners[e.event.type].each(function(t) {
            t(e.event, e.arg);
        }.bind(this));
    },
    _executeEvents: function() {
        for (this._queueRunning = !0; this._eventsQueue.length > 0; ) {
            var e = this._eventsQueue.shift();
            this._executeEventImmediately(e);
        }
        this._queueRunning = !1;
    },
    handleEvents: function(e, t) {
        switch (ORYX.Log.trace("Dispatching event type %0 on %1", e.type, t), e.type) {
          case ORYX.CONFIG.EVENT_MOUSEDOWN:
            this._handleMouseDown(e, t);
            break;

          case ORYX.CONFIG.EVENT_MOUSEMOVE:
            this._handleMouseMove(e, t);
            break;

          case ORYX.CONFIG.EVENT_MOUSEUP:
            this._handleMouseUp(e, t);
            break;

          case ORYX.CONFIG.EVENT_MOUSEOVER:
            this._handleMouseHover(e, t);
            break;

          case ORYX.CONFIG.EVENT_MOUSEOUT:
            this._handleMouseOut(e, t);
        }
        return e.forceExecution ? this._executeEventImmediately({
            event: e,
            arg: t
        }) : this._eventsQueue.push({
            event: e,
            arg: t
        }), this._queueRunning || this._executeEvents(), !1;
    },
    catchKeyUpEvents: function(e) {
        if (this._keyupEnabled && (e || (e = window.event), ![ "INPUT", "TEXTAREA" ].include(e.target.tagName.toUpperCase()))) {
            var t = this.createKeyCombEvent(e, ORYX.CONFIG.KEY_ACTION_UP);
            ORYX.Log.debug("Key Event to handle: %0", t), this.handleEvents({
                type: t,
                event: e
            });
        }
    },
    catchKeyDownEvents: function(e) {
        if (this._keydownEnabled && (e || (e = window.event), ![ "INPUT", "TEXTAREA" ].include(e.target.tagName.toUpperCase()))) {
            var t = this.createKeyCombEvent(e, ORYX.CONFIG.KEY_ACTION_DOWN);
            ORYX.Log.debug("Key Event to handle: %0", t), this.handleEvents({
                type: t,
                event: e
            });
        }
    },
    createKeyCombEvent: function(e, t) {
        var n = e.which || e.keyCode, i = "key.event";
        return t && (i += "." + t), (e.ctrlKey || e.metaKey) && (i += "." + ORYX.CONFIG.META_KEY_META_CTRL), 
        e.altKey && (i += "." + ORYX.CONFIG.META_KEY_ALT), e.shiftKey && (i += "." + ORYX.CONFIG.META_KEY_SHIFT), 
        i + "." + n;
    },
    _handleMouseDown: function(e, t) {
        var n = this.getCanvas();
        n.focus();
        var i, r = e.currentTarget, s = t, o = null !== s && void 0 !== s && s.isSelectable, a = null !== s && void 0 !== s && s.isMovable, c = e.shiftKey || e.ctrlKey, l = 0 === this.selection.length, h = this.selection.member(s);
        if (o && l) this.setSelection([ s ]), ORYX.Log.trace("Rule #1 applied for mouse down on %0", r.id); else if (!o || l || c || h) if (o && c && !h) i = this.selection.clone(), 
        i.push(s), this.setSelection(i), ORYX.Log.trace("Rule #4 applied for mouse down on %0", r.id); else if (o && h && c) i = this.selection.clone(), 
        this.setSelection(i.without(s)), ORYX.Log.trace("Rule #6 applied for mouse down on %0", s.id); else {
            if (!o && !a) return this.setSelection([]), ORYX.Log.trace("Rule #2 applied for mouse down on %0", r.id), 
            void 0;
            o || !a || s instanceof ORYX.Core.Controls.Docker ? o && h && !c && (this._subSelection = this._subSelection != s ? s : void 0, 
            this.setSelection(this.selection, this._subSelection), ORYX.Log.trace("Rule #8 applied for mouse down on %0", r.id)) : ORYX.Log.trace("Rule #7 applied for mouse down on %0", r.id);
        } else this.setSelection([ s ]), ORYX.Log.trace("Rule #3 applied for mouse down on %0", r.id);
    },
    _handleMouseMove: function() {},
    _handleMouseUp: function(e, t) {
        this.getCanvas(), this.eventCoordinates(e);
    },
    _handleMouseHover: function() {},
    _handleMouseOut: function() {},
    eventCoordinates: function(e) {
        var t = this.getCanvas(), n = t.node.ownerSVGElement.createSVGPoint();
        n.x = e.clientX, n.y = e.clientY;
        var i = t.node.getScreenCTM();
        return n.matrixTransform(i.inverse());
    }
}, ORYX.Editor = Clazz.extend(ORYX.Editor), ORYX.Editor.createByUrl = function(e, t) {
    t || (t = {}), new Ajax.Request(e, {
        method: "GET",
        onSuccess: function(e) {
            var n = Ext.decode(e.responseText);
            n = Ext.applyIf(n, t), new ORYX.Editor(n), "function" == typeof t.onSuccess && t.onSuccess(e);
        }.bind(this),
        onFailure: function(e) {
            "function" == typeof t.onFailure && t.onFailure(e);
        }.bind(this)
    });
}, ORYX.Editor.graft = function(e, t, n, i) {
    i = i || t && t.ownerDocument || document;
    var r;
    if (void 0 === n) throw "Can't graft an undefined value";
    if (n.constructor == String) r = i.createTextNode(n); else for (var s = 0; s < n.length; s++) {
        if (0 === s && n[s].constructor == String) {
            var o;
            if (o = n[s].match(/^([a-z][a-z0-9]*)\.([^\s\.]+)$/i)) {
                r = i.createElementNS(e, o[1]), r.setAttributeNS(null, "class", o[2]);
                continue;
            }
            if (o = n[s].match(/^([a-z][a-z0-9]*)$/i)) {
                r = i.createElementNS(e, o[1]);
                continue;
            }
            r = i.createElementNS(e, "span"), r.setAttribute(null, "class", "namelessFromLOL");
        }
        if (void 0 === n[s]) throw "Can't graft an undefined value in a list!";
        if (n[s].constructor == String || n[s].constructor == Array) this.graft(e, r, n[s], i); else if (n[s].constructor == Number) this.graft(e, r, n[s].toString(), i); else if (n[s].constructor == Object) for (var a in n[s]) r.setAttributeNS(null, a, n[s][a]);
    }
    return t && t.appendChild(r), r;
}, ORYX.Editor.provideId = function() {
    var e, t = [], n = "0123456789ABCDEF";
    for (e = 0; 36 > e; e++) t[e] = Math.floor(16 * Math.random());
    for (t[14] = 4, t[19] = 8 | 3 & t[19], e = 0; 36 > e; e++) t[e] = n[t[e]];
    return t[8] = t[13] = t[18] = t[23] = "-", "_" + t.join("");
}, ORYX.Editor.resizeFix = function() {
    ORYX.Editor._resizeFixTimeout || (ORYX.Editor._resizeFixTimeout = window.setTimeout(function() {
        window.resizeBy(1, 1), window.resizeBy(-1, -1), ORYX.Editor._resizefixTimeout = null;
    }, 100));
}, ORYX.Editor.Cookie = {
    callbacks: [],
    onChange: function(e, t) {
        this.callbacks.push(e), this.start(t);
    },
    start: function(e) {
        if (!this.pe) {
            var t = document.cookie;
            this.pe = new PeriodicalExecuter(function() {
                t != document.cookie && (t = document.cookie, this.callbacks.each(function(e) {
                    e(this.getParams());
                }.bind(this)));
            }.bind(this), (e || 1e4) / 1e3);
        }
    },
    stop: function() {
        this.pe && (this.pe.stop(), this.pe = null);
    },
    getParams: function() {
        var e = {}, t = document.cookie;
        return t.split("; ").each(function(t) {
            e[t.split("=")[0]] = t.split("=")[1];
        }), e;
    },
    toString: function() {
        return document.cookie;
    }
}, ORYX.Editor.SVGClassElementsAreAvailable = !0, ORYX.Editor.setMissingClasses = function() {
    try {} catch (e) {
        ORYX.Editor.SVGClassElementsAreAvailable = !1, SVGSVGElement = document.createElementNS("http://www.w3.org/2000/svg", "svg").toString(), 
        SVGGElement = document.createElementNS("http://www.w3.org/2000/svg", "g").toString(), 
        SVGPathElement = document.createElementNS("http://www.w3.org/2000/svg", "path").toString(), 
        SVGTextElement = document.createElementNS("http://www.w3.org/2000/svg", "text").toString(), 
        SVGRectElement = document.createElementNS("http://www.w3.org/2000/svg", "rect").toString(), 
        SVGImageElement = document.createElementNS("http://www.w3.org/2000/svg", "image").toString(), 
        SVGCircleElement = document.createElementNS("http://www.w3.org/2000/svg", "circle").toString(), 
        SVGEllipseElement = document.createElementNS("http://www.w3.org/2000/svg", "ellipse").toString(), 
        SVGLineElement = document.createElementNS("http://www.w3.org/2000/svg", "line").toString(), 
        SVGPolylineElement = document.createElementNS("http://www.w3.org/2000/svg", "polyline").toString(), 
        SVGPolygonElement = document.createElementNS("http://www.w3.org/2000/svg", "polygon").toString();
    }
}, ORYX.Editor.checkClassType = function(e, t) {
    return ORYX.Editor.SVGClassElementsAreAvailable ? e instanceof t : e == t;
}, ORYX.Editor.makeExtModalWindowKeysave = function(e) {
    Ext.override(Ext.Window, {
        beforeShow: function() {
            if (delete this.el.lastXY, delete this.el.lastLT, void 0 === this.x || void 0 === this.y) {
                var t = this.el.getAlignToXY(this.container, "c-c"), n = this.el.translatePoints(t[0], t[1]);
                this.x = void 0 === this.x ? n.left : this.x, this.y = void 0 === this.y ? n.top : this.y;
            }
            this.el.setLeftTop(this.x, this.y), this.expandOnShow && this.expand(!1), this.modal && (e.disableEvent(ORYX.CONFIG.EVENT_KEYDOWN), 
            Ext.getBody().addClass("x-body-masked"), this.mask.setSize(Ext.lib.Dom.getViewWidth(!0), Ext.lib.Dom.getViewHeight(!0)), 
            this.mask.show());
        },
        afterHide: function() {
            this.proxy.hide(), (this.monitorResize || this.modal || this.constrain || this.constrainHeader) && Ext.EventManager.removeResizeListener(this.onWindowResize, this), 
            this.modal && (this.mask.hide(), e.enableEvent(ORYX.CONFIG.EVENT_KEYDOWN), Ext.getBody().removeClass("x-body-masked")), 
            this.keyMap && this.keyMap.disable(), this.fireEvent("hide", this);
        },
        beforeDestroy: function() {
            this.modal && e.enableEvent(ORYX.CONFIG.EVENT_KEYDOWN), Ext.destroy(this.resizer, this.dd, this.proxy, this.mask), 
            Ext.Window.superclass.beforeDestroy.call(this);
        }
    });
}, ORYX.Utils = {
    getParamFromUrl: function(e) {
        e = e.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var t = "[\\?&]" + e + "=([^&#]*)", n = new RegExp(t), i = n.exec(window.location.href);
        return null == i ? null : i[1];
    },
    adjustGradient: function(e, t) {
        if (ORYX.CONFIG.DISABLE_GRADIENT && e) {
            var n = t.getAttributeNS(null, "stop-color") || "#ffffff";
            $A(e.getElementsByTagName("stop")).each(function(e) {
                e != t && e.setAttributeNS(null, "stop-color", n);
            });
        }
    }
};

var ERDF = {
    LITERAL: 1,
    RESOURCE: 2,
    DELIMITERS: [ ".", "-" ],
    HASH: "#",
    HYPHEN: "-",
    schemas: [],
    callback: void 0,
    log: void 0,
    init: function(e) {
        ERDF.callback = e, ERDF.registerSchema("schema", XMLNS.SCHEMA), ERDF.registerSchema("rdfs", XMLNS.RDFS);
    },
    run: function() {
        return ERDF._checkProfile() && ERDF.parse();
    },
    parse: function() {
        ERDF.__startTime = new Date();
        var e = document.getElementsByTagNameNS(XMLNS.XHTML, "body"), t = {
            type: ERDF.RESOURCE,
            value: ""
        }, n = ERDF._parseDocumentMetadata() && ERDF._parseFromTag(e[0], t);
        return ERDF.__stopTime = new Date(), (ERDF.__stopTime - ERDF.__startTime) / 1e3, 
        n;
    },
    _parseDocumentMetadata: function() {
        var e = document.getElementsByTagNameNS(XMLNS.XHTML, "head"), t = e[0].getElementsByTagNameNS(XMLNS.XHTML, "link"), n = e[0].getElementsByTagNameNS(XMLNS.XHTML, "meta");
        return $A(t).each(function(e) {
            var t = e.getAttribute("rel"), n = e.getAttribute("rev"), i = e.getAttribute("href");
            ERDF._parseTriplesFrom(ERDF.RESOURCE, "", t, ERDF.RESOURCE, i), ERDF._parseTriplesFrom(ERDF.RESOURCE, i, n, ERDF.RESOURCE, "");
        }), $A(n).each(function(e) {
            var t = e.getAttribute("name"), n = e.getAttribute("content");
            ERDF._parseTriplesFrom(ERDF.RESOURCE, "", t, ERDF.LITERAL, n);
        }), !0;
    },
    _parseFromTag: function(e, t, n) {
        if (e.namespaceURI == XMLNS.XHTML) {
            n || (n = 0);
            var i = e.getAttribute("id");
            if (e.nodeName.endsWith(":a") || "a" == e.nodeName) {
                var r = e.getAttribute("rel"), s = e.getAttribute("rev"), o = e.getAttribute("href"), a = e.getAttribute("title"), c = e.textContent;
                ERDF._parseTriplesFrom(t.type, t.value, r, ERDF.RESOURCE, o, function(e) {
                    var t = a ? a : c;
                    ERDF._parseTriplesFrom(e.object.type, e.object.value, "rdfs.label", ERDF.LITERAL, t);
                }), ERDF._parseTriplesFrom(t.type, t.value, s, ERDF.RESOURCE, ""), ERDF._parseTypeTriplesFrom(t.type, t.value, r);
            } else if (e.nodeName.endsWith(":img") || "img" == e.nodeName) {
                var r = e.getAttribute("class"), o = e.getAttribute("src"), l = e.getAttribute("alt");
                ERDF._parseTriplesFrom(t.type, t.value, r, ERDF.RESOURCE, o, function(e) {
                    var t = l;
                    ERDF._parseTriplesFrom(e.object.type, e.object.value, "rdfs.label", ERDF.LITERAL, t);
                });
            }
            var r = e.getAttribute("class"), a = e.getAttribute("title"), c = e.textContent, h = a ? a : c;
            ERDF._parseTriplesFrom(t.type, t.value, r, ERDF.LITERAL, h), i && (t = {
                type: ERDF.RESOURCE,
                value: ERDF.HASH + i
            }), ERDF._parseTypeTriplesFrom(t.type, t.value, r);
            var u = e.childNodes;
            u && $A(u).each(function(e) {
                e.nodeType == e.ELEMENT_NODE && ERDF._parseFromTag(e, t, n + 1);
            });
        }
    },
    _parseTriplesFrom: function(e, t, n, i, r, s) {
        n && n.toLowerCase().split(" ").each(function(e) {
            var n = ERDF.schemas.find(function(t) {
                return !1 || ERDF.DELIMITERS.find(function(n) {
                    return e.startsWith(t.prefix + n);
                });
            });
            if (n && r) {
                e = e.substring(n.prefix.length + 1, e.length);
                var o = ERDF.registerTriple(new ERDF.Resource(t), {
                    prefix: n.prefix,
                    name: e
                }, i == ERDF.RESOURCE ? new ERDF.Resource(r) : new ERDF.Literal(r));
                s && s(o);
            }
        });
    },
    _parseTypeTriplesFrom: function(e, t, n, i) {
        n && n.toLowerCase().split(" ").each(function(n) {
            var r = ERDF.schemas.find(function(e) {
                return !1 || ERDF.DELIMITERS.find(function(t) {
                    return n.startsWith(ERDF.HYPHEN + e.prefix + t);
                });
            });
            if (r && t) {
                n = n.substring(r.prefix.length + 2, n.length);
                var s = ERDF.registerTriple(e == ERDF.RESOURCE ? new ERDF.Resource(t) : new ERDF.Literal(t), {
                    prefix: "rdf",
                    name: "type"
                }, new ERDF.Resource(r.namespace + n));
                i && i(s);
            }
        });
    },
    _checkProfile: function() {
        var e = document.getElementsByTagNameNS(XMLNS.XHTML, "head"), t = e[0].getAttribute("profile");
        return t && t.split(" ").member(XMLNS.ERDF) ? !0 : !1;
    },
    __stripHashes: function(e) {
        return e && "#" == e.substring(0, 1) ? e.substring(1, e.length) : e;
    },
    registerSchema: function(e, t) {
        ERDF.schemas.push({
            prefix: e,
            namespace: t
        });
    },
    registerTriple: function(e, t, n) {
        "schema" == t.prefix.toLowerCase() && this.registerSchema(t.name, n.value);
        var i = new ERDF.Triple(e, t, n);
        return ERDF.callback(i), i;
    },
    __enhanceObject: function() {
        this.isResource = function() {
            return this.type == ERDF.RESOURCE;
        }, this.isLocal = function() {
            return this.isResource() && this.value.startsWith("#");
        }, this.isCurrentDocument = function() {
            return this.isResource() && "" == this.value;
        }, this.getId = function() {
            return this.isLocal() ? ERDF.__stripHashes(this.value) : !1;
        }, this.isLiteral = function() {
            return this.type == ERDF.LIITERAL;
        };
    },
    serialize: function(e) {
        return e ? e.constructor == String ? e : e.constructor == Boolean ? e ? "true" : "false" : e.toString() : "";
    }
};

ERDF.Triple = function(e, t, n) {
    this.subject = e, this.predicate = t, this.object = n, this.toString = function() {
        return "[ERDF.Triple] " + this.subject.toString() + " " + this.predicate.prefix + ":" + this.predicate.name + " " + this.object.toString();
    };
}, ERDF.Resource = function(e) {
    this.type = ERDF.RESOURCE, this.value = e, ERDF.__enhanceObject.apply(this), this.toString = function() {
        return "&lt;" + this.value + "&gt;";
    };
}, ERDF.Literal = function(e) {
    this.type = ERDF.LITERAL, this.value = ERDF.serialize(e), ERDF.__enhanceObject.apply(this), 
    this.toString = function() {
        return '"' + this.value + '"';
    };
};

var USE_ASYNCHRONOUS_REQUESTS = !0, DISCARD_UNUSED_TRIPLES = !0, PREFER_SPANS_OVER_DIVS = !0, PREFER_TITLE_OVER_TEXTNODE = !1, RESOURCE_ID_PREFIX = "resource", SHOW_DEBUG_ALERTS_WHEN_SAVING = !1, SHOW_EXTENDED_DEBUG_INFORMATION = !1, USE_ARESS_WORKAROUNDS = !0, RESOURCE_CREATED = 1, RESOURCE_REMOVED = 2, RESOURCE_SAVED = 4, RESOURCE_RELOADED = 8, RESOURCE_SYNCHRONIZED = 16, TRIPLE_REMOVE = 1, TRIPLE_ADD = 2, TRIPLE_RELOAD = 4, TRIPLE_SAVE = 8, PROCESSDATA_REF = "processdata", DataManager = {
    init: function() {
        ERDF.init(DataManager._registerTriple), DataManager.__synclocal();
    },
    _triples: [],
    _registerTriple: function(e) {
        DataManager._triples.push(e);
    },
    __synclocal: function() {
        DataManager._triples = [], ERDF.run();
    },
    __synchronizeShape: function(e) {
        var t = ResourceManager.getResource(e.resourceId), n = e.serialize();
        return n.each(function(t) {
            var n = "resource" == t.type, i = new ERDF.Triple(new ERDF.Resource(e.resourceId), {
                prefix: t.prefix,
                name: t.name
            }, n ? new ERDF.Resource(t.value) : new ERDF.Literal(t.value));
            DataManager.setObject(i);
        }), t;
    },
    __storeShape: function(e) {
        var t = DataManager.__synchronizeShape(e);
        t.save();
    },
    __forceExistance: function(e) {
        if ($(e.resourceId)) {
            var t = $(e.resourceId), n = $A(t.childNodes);
            n.each(function(e) {
                t.removeChild(e);
            });
        } else $$("." + PROCESSDATA_REF)[0] || DataManager.graft(XMLNS.XHTML, document.getElementsByTagNameNS(XMLNS.XHTML, "body").item(0), [ "div", {
            "class": PROCESSDATA_REF,
            style: "display:none;"
        } ]), DataManager.graft(XMLNS.XHTML, $$("." + PROCESSDATA_REF)[0], [ "div", {
            id: e.resourceId,
            "class": e instanceof ORYX.Core.Canvas ? "-oryx-canvas" : void 0
        } ]);
    },
    __persistShape: function(e) {
        var t = e.serialize(), n = new ERDF.Resource(e.resourceId);
        DataManager.removeTriples(DataManager.query(n, void 0, void 0)), t.each(function(e) {
            var t = "resource" == e.type ? new ERDF.Resource(e.value) : new ERDF.Literal(e.value);
            DataManager.addTriple(new ERDF.Triple(n, {
                prefix: e.prefix,
                name: e.name
            }, t));
        });
    },
    __persistDOM: function(e) {
        var t = e.getCanvas(), n = t.getChildShapes(!0), i = "";
        return n.each(function(e) {
            DataManager.__forceExistance(e);
        }), DataManager.__renderCanvas(e), i += DataManager.serialize($(ERDF.__stripHashes(e.getCanvas().resourceId)), !0), 
        n.each(function(e) {
            DataManager.__persistShape(e), i += DataManager.serialize($(ERDF.__stripHashes(e.resourceId)), !0);
        }), i;
    },
    __renderCanvas: function(e) {
        var t = e.getCanvas(), n = e.getStencilSets(), i = t.getChildShapes(!0);
        DataManager.__forceExistance(t), DataManager.__persistShape(t);
        var r = new ERDF.Resource(t.resourceId);
        DataManager.removeTriples(DataManager.query(r, void 0, void 0)), DataManager.addTriple(new ERDF.Triple(r, {
            prefix: "oryx",
            name: "mode"
        }, new ERDF.Literal("writable"))), DataManager.addTriple(new ERDF.Triple(r, {
            prefix: "oryx",
            name: "mode"
        }, new ERDF.Literal("fullscreen"))), n.values().each(function(e) {
            DataManager.addTriple(new ERDF.Triple(r, {
                prefix: "oryx",
                name: "stencilset"
            }, new ERDF.Resource(e.source().replace(/&/g, "%26")))), DataManager.addTriple(new ERDF.Triple(r, {
                prefix: "oryx",
                name: "ssnamespace"
            }, new ERDF.Resource(e.namespace()))), e.extensions().keys().each(function(e) {
                DataManager.addTriple(new ERDF.Triple(r, {
                    prefix: "oryx",
                    name: "ssextension"
                }, new ERDF.Literal(e)));
            });
        }), i.each(function(e) {
            DataManager.addTriple(new ERDF.Triple(r, {
                prefix: "oryx",
                name: "render"
            }, new ERDF.Resource("#" + e.resourceId)));
        });
    },
    __counter: 0,
    __provideId: function() {
        for (;$(RESOURCE_ID_PREFIX + DataManager.__counter); ) DataManager.__counter++;
        return RESOURCE_ID_PREFIX + DataManager.__counter;
    },
    serializeDOM: function(e) {
        return DataManager.__persistDOM(e);
    },
    syncGlobal: function(e) {
        return DataManager.__syncglobal(e);
    },
    __syncglobal: function(e) {
        var t = e.getCanvas(), n = t.getChildShapes(!0);
        n.select(function(e) {
            return !$(e.resourceId);
        }).each(function(e) {
            if (USE_ARESS_WORKAROUNDS) {
                var t = e.properties["raziel-type"], n = '<div xmlns="http://www.w3.org/1999/xhtml"><span class="raziel-type">' + t + "</span></div>", i = ResourceManager.__createResource(n);
                e.resourceId = i.id();
            } else {
                var i = ResourceManager.__createResource();
                e.resourceId = i.id();
            }
        }), n.each(function(e) {
            DataManager.__storeShape(e);
        });
    },
    serialize: function(e, t) {
        if (e.nodeType == e.ELEMENT_NODE) {
            var n = $A(e.childNodes), i = $A(e.attributes), r = new String(e.getAttribute("class")), s = r.split(" ").member("transient");
            if (s) return "";
            var o = "<" + e.nodeName;
            return t || (o += ' xmlns="' + (e.namespaceURI ? e.namespaceURI : XMLNS.XHTML) + '" xmlns:oryx="http://oryx-editor.org"'), 
            i.each(function(e) {
                o += " " + e.nodeName + '="' + e.nodeValue + '"';
            }), 0 == n.length ? o += "/>" : (o += ">", n.each(function(e) {
                o += DataManager.serialize(e, !0);
            }), o += "</" + e.nodeName + ">"), o;
        }
        return e.nodeType == e.TEXT_NODE ? e.nodeValue : void 0;
    },
    addTriple: function(e) {
        if (!e.subject.type == ERDF.LITERAL) throw "Cannot add the triple " + e.toString() + " because the subject is not a resource.";
        var t = ERDF.__stripHashes(e.subject.value), n = $(t);
        if (!n) throw "Cannot add the triple " + e.toString() + ' because the subject "' + t + '" is not in the document.';
        return e.object.type == ERDF.LITERAL ? DataManager.graft(XMLNS.XHTML, n, [ "span", {
            "class": e.predicate.prefix + "-" + e.predicate.name
        }, e.object.value.escapeHTML() ]) : DataManager.graft(XMLNS.XHTML, n, [ "a", {
            rel: e.predicate.prefix + "-" + e.predicate.name,
            href: e.object.value
        } ]), !0;
    },
    removeTriples: function(e) {
        var t = e.select(function(e) {
            return DataManager.__removeTriple(e);
        });
        return t;
    },
    removeTriple: function(e) {
        var t = DataManager.__removeTriple(e);
        return t;
    },
    __removeTriple: function(e) {
        if (!e.subject.type == ERDF.LITERAL) throw "Cannot remove the triple " + e.toString() + " because the subject is not a resource.";
        var t = ERDF.__stripHashes(e.subject.value), n = $(t);
        if (!n) throw "Cannot remove the triple " + e.toString() + " because the subject is not in the document.";
        if (e.object.type == ERDF.LITERAL) {
            var i = DataManager.__removeTripleRecursively(e, n);
            return i;
        }
    },
    __removeTripleRecursively: function(e, t) {
        if (t.nodeType != t.ELEMENT_NODE) return !1;
        var n = new String(t.getAttribute("class")), i = $A(t.childNodes);
        if (n.include(e.predicate.prefix + "-" + e.predicate.name)) {
            var r = t.textContent;
            return e.object.type == ERDF.LITERAL && e.object.value == r && t.parentNode.removeChild(t), 
            !0;
        }
        return i.each(function(t) {
            DataManager.__removeTripleRecursively(e, t);
        }), !1;
    },
    graft: function(e, t, n, i) {
        i = i || t && t.ownerDocument || document;
        var r;
        if (void 0 === n) echo("Can't graft an undefined value"); else if (n.constructor == String) r = i.createTextNode(n); else for (var s = 0; s < n.length; s++) {
            if (0 === s && n[s].constructor == String) {
                var o = n[s].match(/^([a-z][a-z0-9]*)\.([^\s\.]+)$/i);
                if (o) {
                    r = i.createElementNS(e, o[1]), r.setAttributeNS(null, "class", o[2]);
                    continue;
                }
                if (o = n[s].match(/^([a-z][a-z0-9]*)$/i)) {
                    r = i.createElementNS(e, o[1]);
                    continue;
                }
                r = i.createElementNS(e, "span"), r.setAttribute(null, "class", "namelessFromLOL");
            }
            if (void 0 === n[s]) echo("Can't graft an undefined value in a list!"); else if (n[s].constructor == String || n[s].constructor == Array) this.graft(e, r, n[s], i); else if (n[s].constructor == Number) this.graft(e, r, n[s].toString(), i); else if (n[s].constructor == Object) for (var a in n[s]) r.setAttributeNS(null, a, n[s][a]); else {
                if (n[s].constructor != Boolean) throw "Object " + n[s] + " is inscrutable as an graft arglet.";
                this.graft(e, r, n[s] ? "true" : "false", i);
            }
        }
        return t && t.appendChild(r), Element.extend(r);
    },
    setObject: function(e) {
        var t = DataManager.query(e.subject, e.predicate, void 0);
        return DataManager.removeTriples(t), DataManager.addTriple(e), !0;
    },
    query: function(e, t, n) {
        return DataManager._triples.select(function(i) {
            var r = e ? i.subject.type == e.type && i.subject.value == e.value : !0;
            return t && (r = r && (t.prefix ? i.predicate.prefix == t.prefix : !0), r = r && (t.name ? i.predicate.name == t.name : !0)), 
            r = r && (n ? i.object.type == n.type && i.object.value == n.value : !0);
        });
    }
};

Kickstart.register(DataManager.init);

var chain = new MetaTagHandler(), command = new DMCommand(TRIPLE_ADD, new ERDF.Triple(new ERDF.Resource(""), "rdf:tool", new ERDF.Literal("")));

if (ResourceManager = {
    __corrupt: !1,
    __latelyCreatedResource: void 0,
    __listeners: $H(),
    __token: 1,
    addListener: function(e, t) {
        if (!(e instanceof Function)) throw "Resource event listener is not a function!";
        if (!t) throw "Invalid mask for resource event listener registration.";
        var n = {
            listener: e,
            mask: t
        }, i = ResourceManager.__token++;
        return ResourceManager.__listeners[i] = n, i;
    },
    removeListener: function(e) {
        return ResourceManager.__listners.remove(e);
    },
    __Event: function(e, t) {
        this.action = e, this.resourceId = t;
    },
    __dispatchEvent: function(e) {
        ResourceManager.__listeners.values().each(function(t) {
            return e.action & t.mask ? t.listener(e) : void 0;
        });
    },
    getResource: function(e) {
        e = ERDF.__stripHashes(e);
        var t = DataManager.query(new ERDF.Resource("#" + e), {
            prefix: "raziel",
            name: "entry"
        }, void 0);
        if (1 == t.length && t[0].object.isResource()) {
            var n = t[0].object.value;
            return new ResourceManager.__Resource(e, n);
        }
        throw "Resource with id " + e + " not recognized as such. " + (t.length > 1 ? " There is more than one raziel:entry URL." : " There is no raziel:entry URL.");
    },
    __createResource: function(e) {
        var t = DataManager.query(new ERDF.Resource(""), {
            prefix: "raziel",
            name: "collection"
        }, void 0);
        if (1 == t.length && t[0].object.isResource()) {
            var n = t[0].object.value, i = void 0, r = e ? e : '<div xmlns="http://www.w3.org/1999/xhtml"></div>';
            return ResourceManager.__request("POST", n, r, function() {
                var e = this.responseXML, t = e.childNodes[0], n = t.getAttribute("id");
                $$("." + PROCESSDATA_REF)[0] || DataManager.graft(XMLNS.XHTML, document.getElementsByTagNameNS(XMLNS.XHTML, "body").item(0), [ "div", {
                    "class": PROCESSDATA_REF,
                    style: "display:none;"
                } ]), $$("." + PROCESSDATA_REF)[0].appendChild(t.cloneNode(!0)), DataManager.__synclocal(), 
                i = new ResourceManager.getResource(n), ResourceManager.__resourceActionSucceeded(this, RESOURCE_CREATED, void 0);
            }, function() {
                ResourceManager.__resourceActionFailed(this, RESOURCE_CREATED, void 0);
            }, !1), i;
        }
        throw "Could not create resource! raziel:collection URL is missing!";
    },
    __Resource: function(e, t) {
        this.__id = e, this.__url = t, this.id = function() {
            return this.__id;
        }, this.url = function() {
            return this.__url;
        }, this.reload = function() {
            var e = this.__url, t = this.__id;
            ResourceManager.__request("GET", e, null, function() {
                ResourceManager.__resourceActionSucceeded(this, RESOURCE_RELOADED, t);
            }, function() {
                ResourceManager.__resourceActionFailed(this, RESURCE_RELOADED, t);
            }, USE_ASYNCHRONOUS_REQUESTS);
        }, this.save = function(e) {
            var t = this.__url, n = this.__id;
            data = DataManager.serialize($(n)), ResourceManager.__request("PUT", t, data, function() {
                ResourceManager.__resourceActionSucceeded(this, e ? RESOURCE_SAVED | RESOURCE_SYNCHRONIZED : RESOURCE_SAVED, n);
            }, function() {
                ResourceManager.__resourceActionFailed(this, e ? RESOURCE_SAVED | RESOURCE_SYNCHRONIZED : RESOURCE.SAVED, n);
            }, USE_ASYNCHRONOUS_REQUESTS);
        }, this.remove = function() {
            var e = this.__url, t = this.__id;
            ResourceManager.__request("DELETE", e, null, function() {
                ResourceManager.__resourceActionSucceeded(this, RESOURCE_REMOVED, t);
            }, function() {
                ResourceManager.__resourceActionFailed(this, RESOURCE_REMOVED, t);
            }, USE_ASYNCHRONOUS_REQUESTS);
        };
    },
    request: function(e, t) {
        var n = {
            method: "get",
            asynchronous: !0,
            parameters: {}
        };
        Object.extend(n, t || {});
        var i = Hash.toQueryString(n.parameters);
        return i && (e += (e.include("?") ? "&" : "?") + i), ResourceManager.__request(n.method, e, n.data, n.onSuccess instanceof Function ? function() {
            n.onSuccess(this);
        } : void 0, n.onFailure instanceof Function ? function() {
            n.onFailure(this);
        } : void 0, n.asynchronous && USE_ASYNCHRONOUS_REQUESTS, n.headers);
    },
    __request: function(e, t, n, i, r, s, o) {
        var a = Try.these(function() {
            return new XMLHttpRequest();
        }, function() {
            return new ActiveXObject("Msxml2.XMLHTTP");
        }, function() {
            return new ActiveXObject("Microsoft.XMLHTTP");
        });
        if (!a) {
            if (!this.__corrupt) throw "This browser does not provide any AJAX functionality. You will not be able to use the software provided with the page you are viewing. Please consider installing appropriate extensions.";
            return this.__corrupt = !0, !1;
        }
        i instanceof Function && (a.onload = i), r instanceof Function && (a.onerror = r);
        var c = $H(o);
        c.keys().each(function(e) {
            a.setRequestHeader(e, c[e]);
        });
        try {
            SHOW_DEBUG_ALERTS_WHEN_SAVING && alert(e + " " + t + "\n" + SHOW_EXTENDED_DEBUG_INFORMATION ? n : ""), 
            a.open(e, t, s ? !0 : !1), a.send(n);
        } catch (l) {
            return !1;
        }
        return !0;
    },
    __resourceActionSucceeded: function(e, t, n) {
        var i = e.status, r = e.responseText;
        if (SHOW_DEBUG_ALERTS_WHEN_SAVING && alert(i + " " + url + "\n" + SHOW_EXTENDED_DEBUG_INFORMATION ? data : ""), 
        i >= 300) throw "The server responded with an error: " + i + "\n" + (SHOW_EXTENDED_DEBUG_INFORMATION ? +data : "If you need additional information here, including the data sent by the server, consider setting SHOW_EXTENDED_DEBUG_INFORMATION to true.");
        switch (t) {
          case RESOURCE_REMOVED:
            var r = e.responseXML, s = r.childNodes[0], n = s.getAttribute("id"), o = document.getElementById(n);
            o.parentNode.removeChild(o);
            break;

          case RESOURCE_CREATED:
            break;

          case RESOURCE_SAVED | RESOURCE_SYNCHRONIZED:
            DataManager.__synclocal();

          case RESOURCE_SAVED:
            break;

          case RESOURCE_RELOADED:
            var r = e.responseXML, s = r.childNodes[0], n = s.getAttribute("id"), o = document.getElementById(n);
            o.parentNode.removeChild(o), $$(PROCESSDATA_REF)[0] || DataManager.graft(XMLNS.XHTML, document.getElementsByTagNameNS(XMLNS.XHTML, "body").item(0), [ "div", {
                "class": PROCESSDATA_REF,
                style: "display:none;"
            } ]), $$(PROCESSDATA_REF)[0].appendChild(s.cloneNode(!0)), DataManager.__synclocal();
            break;

          default:
            DataManager.__synclocal();
        }
        ResourceManager.__dispatchEvent(new ResourceManager.__Event(t, n));
    },
    __resourceActionFailed: function() {
        throw "Fatal: Resource action failed. There is something horribly wrong with either the server, the transport protocol or your online status. Sure you're online?";
    }
}, !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.SVG || (ORYX.Core.SVG = {}), ORYX.Core.SVG.EditPathHandler = Clazz.extend({
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.x = 0, this.y = 0, this.oldX = 0, 
        this.oldY = 0, this.deltaWidth = 1, this.deltaHeight = 1, this.d = "";
    },
    init: function(e, t, n, i, r, s) {
        this.x = e, this.y = t, this.oldX = n, this.oldY = i, this.deltaWidth = r, this.deltaHeight = s, 
        this.d = "";
    },
    editPointsAbs: function(e) {
        if (e instanceof Array) {
            for (var t, n, i = [], r = 0; r < e.length; r++) t = (parseFloat(e[r]) - this.oldX) * this.deltaWidth + this.x, 
            r++, n = (parseFloat(e[r]) - this.oldY) * this.deltaHeight + this.y, i.push(t), 
            i.push(n);
            return i;
        }
    },
    editPointsRel: function(e) {
        if (e instanceof Array) {
            for (var t, n, i = [], r = 0; r < e.length; r++) t = parseFloat(e[r]) * this.deltaWidth, 
            r++, n = parseFloat(e[r]) * this.deltaHeight, i.push(t), i.push(n);
            return i;
        }
    },
    arcAbs: function(e, t, n, i, r, s, o) {
        var a = this.editPointsAbs([ s, o ]), c = this.editPointsRel([ e, t ]);
        this.d = this.d.concat(" A" + c[0] + " " + c[1] + " " + n + " " + i + " " + r + " " + a[0] + " " + a[1] + " ");
    },
    arcRel: function(e, t, n, i, r, s, o) {
        var a = this.editPointsRel([ e, t, s, o ]);
        this.d = this.d.concat(" a" + a[0] + " " + a[1] + " " + n + " " + i + " " + r + " " + a[2] + " " + a[3] + " ");
    },
    curvetoCubicAbs: function(e, t, n, i, r, s) {
        var o = this.editPointsAbs([ e, t, n, i, r, s ]);
        this.d = this.d.concat(" C" + o[0] + " " + o[1] + " " + o[2] + " " + o[3] + " " + o[4] + " " + o[5] + " ");
    },
    curvetoCubicRel: function(e, t, n, i, r, s) {
        var o = this.editPointsRel([ e, t, n, i, r, s ]);
        this.d = this.d.concat(" c" + o[0] + " " + o[1] + " " + o[2] + " " + o[3] + " " + o[4] + " " + o[5] + " ");
    },
    linetoHorizontalAbs: function(e) {
        var t = this.editPointsAbs([ e, 0 ]);
        this.d = this.d.concat(" H" + t[0] + " ");
    },
    linetoHorizontalRel: function(e) {
        var t = this.editPointsRel([ e, 0 ]);
        this.d = this.d.concat(" h" + t[0] + " ");
    },
    linetoAbs: function(e, t) {
        var n = this.editPointsAbs([ e, t ]);
        this.d = this.d.concat(" L" + n[0] + " " + n[1] + " ");
    },
    linetoRel: function(e, t) {
        var n = this.editPointsRel([ e, t ]);
        this.d = this.d.concat(" l" + n[0] + " " + n[1] + " ");
    },
    movetoAbs: function(e, t) {
        var n = this.editPointsAbs([ e, t ]);
        this.d = this.d.concat(" M" + n[0] + " " + n[1] + " ");
    },
    movetoRel: function(e, t) {
        var n;
        n = "" === this.d ? this.editPointsAbs([ e, t ]) : this.editPointsRel([ e, t ]), 
        this.d = this.d.concat(" m" + n[0] + " " + n[1] + " ");
    },
    curvetoQuadraticAbs: function(e, t, n, i) {
        var r = this.editPointsAbs([ e, t, n, i ]);
        this.d = this.d.concat(" Q" + r[0] + " " + r[1] + " " + r[2] + " " + r[3] + " ");
    },
    curvetoQuadraticRel: function(e, t, n, i) {
        var r = this.editPointsRel([ e, t, n, i ]);
        this.d = this.d.concat(" q" + r[0] + " " + r[1] + " " + r[2] + " " + r[3] + " ");
    },
    curvetoCubicSmoothAbs: function(e, t, n, i) {
        var r = this.editPointsAbs([ e, t, n, i ]);
        this.d = this.d.concat(" S" + r[0] + " " + r[1] + " " + r[2] + " " + r[3] + " ");
    },
    curvetoCubicSmoothRel: function(e, t, n, i) {
        var r = this.editPointsRel([ e, t, n, i ]);
        this.d = this.d.concat(" s" + r[0] + " " + r[1] + " " + r[2] + " " + r[3] + " ");
    },
    curvetoQuadraticSmoothAbs: function(e, t) {
        var n = this.editPointsAbs([ e, t ]);
        this.d = this.d.concat(" T" + n[0] + " " + n[1] + " ");
    },
    curvetoQuadraticSmoothRel: function(e, t) {
        var n = this.editPointsRel([ e, t ]);
        this.d = this.d.concat(" t" + n[0] + " " + n[1] + " ");
    },
    linetoVerticalAbs: function(e) {
        var t = this.editPointsAbs([ 0, e ]);
        this.d = this.d.concat(" V" + t[1] + " ");
    },
    linetoVerticalRel: function(e) {
        var t = this.editPointsRel([ 0, e ]);
        this.d = this.d.concat(" v" + t[1] + " ");
    },
    closePath: function() {
        this.d = this.d.concat(" z");
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.SVG || (ORYX.Core.SVG = {}), ORYX.Core.SVG.MinMaxPathHandler = Clazz.extend({
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.minX = void 0, this.minY = void 0, 
        this.maxX = void 0, this.maxY = void 0, this._lastAbsX = void 0, this._lastAbsY = void 0;
    },
    calculateMinMax: function(e) {
        if (e instanceof Array) for (var t, n, i = 0; i < e.length; i++) t = parseFloat(e[i]), 
        i++, n = parseFloat(e[i]), this.minX = void 0 !== this.minX ? Math.min(this.minX, t) : t, 
        this.maxX = void 0 !== this.maxX ? Math.max(this.maxX, t) : t, this.minY = void 0 !== this.minY ? Math.min(this.minY, n) : n, 
        this.maxY = void 0 !== this.maxY ? Math.max(this.maxY, n) : n, this._lastAbsX = t, 
        this._lastAbsY = n;
    },
    arcAbs: function(e, t, n, i, r, s, o) {
        this.calculateMinMax([ s, o ]);
    },
    arcRel: function(e, t, n, i, r, s, o) {
        this.calculateMinMax([ this._lastAbsX + s, this._lastAbsY + o ]);
    },
    curvetoCubicAbs: function(e, t, n, i, r, s) {
        this.calculateMinMax([ e, t, n, i, r, s ]);
    },
    curvetoCubicRel: function(e, t, n, i, r, s) {
        this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY + t, this._lastAbsX + n, this._lastAbsY + i, this._lastAbsX + r, this._lastAbsY + s ]);
    },
    linetoHorizontalAbs: function(e) {
        this.calculateMinMax([ e, this._lastAbsY ]);
    },
    linetoHorizontalRel: function(e) {
        this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY ]);
    },
    linetoAbs: function(e, t) {
        this.calculateMinMax([ e, t ]);
    },
    linetoRel: function(e, t) {
        this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY + t ]);
    },
    movetoAbs: function(e, t) {
        this.calculateMinMax([ e, t ]);
    },
    movetoRel: function(e, t) {
        this._lastAbsX && this._lastAbsY ? this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY + t ]) : this.calculateMinMax([ e, t ]);
    },
    curvetoQuadraticAbs: function(e, t, n, i) {
        this.calculateMinMax([ e, t, n, i ]);
    },
    curvetoQuadraticRel: function(e, t, n, i) {
        this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY + t, this._lastAbsX + n, this._lastAbsY + i ]);
    },
    curvetoCubicSmoothAbs: function(e, t, n, i) {
        this.calculateMinMax([ e, t, n, i ]);
    },
    curvetoCubicSmoothRel: function(e, t, n, i) {
        this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY + t, this._lastAbsX + n, this._lastAbsY + i ]);
    },
    curvetoQuadraticSmoothAbs: function(e, t) {
        this.calculateMinMax([ e, t ]);
    },
    curvetoQuadraticSmoothRel: function(e, t) {
        this.calculateMinMax([ this._lastAbsX + e, this._lastAbsY + t ]);
    },
    linetoVerticalAbs: function(e) {
        this.calculateMinMax([ this._lastAbsX, e ]);
    },
    linetoVerticalRel: function(e) {
        this.calculateMinMax([ this._lastAbsX, this._lastAbsY + e ]);
    },
    closePath: function() {}
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.SVG || (ORYX.Core.SVG = {}), ORYX.Core.SVG.PointsPathHandler = Clazz.extend({
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.points = [], this._lastAbsX = void 0, 
        this._lastAbsY = void 0;
    },
    addPoints: function(e) {
        if (e instanceof Array) for (var t, n, i = 0; i < e.length; i++) t = parseFloat(e[i]), 
        i++, n = parseFloat(e[i]), this.points.push(t), this.points.push(n), this._lastAbsX = t, 
        this._lastAbsY = n;
    },
    arcAbs: function(e, t, n, i, r, s, o) {
        this.addPoints([ s, o ]);
    },
    arcRel: function(e, t, n, i, r, s, o) {
        this.addPoints([ this._lastAbsX + s, this._lastAbsY + o ]);
    },
    curvetoCubicAbs: function(e, t, n, i, r, s) {
        this.addPoints([ r, s ]);
    },
    curvetoCubicRel: function(e, t, n, i, r, s) {
        this.addPoints([ this._lastAbsX + r, this._lastAbsY + s ]);
    },
    linetoHorizontalAbs: function(e) {
        this.addPoints([ e, this._lastAbsY ]);
    },
    linetoHorizontalRel: function(e) {
        this.addPoints([ this._lastAbsX + e, this._lastAbsY ]);
    },
    linetoAbs: function(e, t) {
        this.addPoints([ e, t ]);
    },
    linetoRel: function(e, t) {
        this.addPoints([ this._lastAbsX + e, this._lastAbsY + t ]);
    },
    movetoAbs: function(e, t) {
        this.addPoints([ e, t ]);
    },
    movetoRel: function(e, t) {
        this._lastAbsX && this._lastAbsY ? this.addPoints([ this._lastAbsX + e, this._lastAbsY + t ]) : this.addPoints([ e, t ]);
    },
    curvetoQuadraticAbs: function(e, t, n, i) {
        this.addPoints([ n, i ]);
    },
    curvetoQuadraticRel: function(e, t, n, i) {
        this.addPoints([ this._lastAbsX + n, this._lastAbsY + i ]);
    },
    curvetoCubicSmoothAbs: function(e, t, n, i) {
        this.addPoints([ n, i ]);
    },
    curvetoCubicSmoothRel: function(e, t, n, i) {
        this.addPoints([ this._lastAbsX + n, this._lastAbsY + i ]);
    },
    curvetoQuadraticSmoothAbs: function(e, t) {
        this.addPoints([ e, t ]);
    },
    curvetoQuadraticSmoothRel: function(e, t) {
        this.addPoints([ this._lastAbsX + e, this._lastAbsY + t ]);
    },
    linetoVerticalAbs: function(e) {
        this.addPoints([ this._lastAbsX, e ]);
    },
    linetoVerticalRel: function(e) {
        this.addPoints([ this._lastAbsX, this._lastAbsY + e ]);
    },
    closePath: function() {}
}), NAMESPACE_ORYX = "http://www.b3mn.org/oryx", NAMESPACE_SVG = "http://www.w3.org/2000/svg/", 
!ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.SVG || (ORYX.Core.SVG = {}), ORYX.Core.SVG.SVGMarker = Clazz.extend({
    construct: function(e) {
        arguments.callee.$.construct.apply(this, arguments), this.id = void 0, this.element = e, 
        this.refX = void 0, this.refY = void 0, this.markerWidth = void 0, this.markerHeight = void 0, 
        this.oldRefX = void 0, this.oldRefY = void 0, this.oldMarkerWidth = void 0, this.oldMarkerHeight = void 0, 
        this.optional = !1, this.enabled = !0, this.minimumLength = void 0, this.resize = !1, 
        this.svgShapes = [], this._init();
    },
    _init: function() {
        if ("[object SVGMarkerElement]" != this.element) throw "SVGMarker: Argument is not an instance of SVGMarkerElement.";
        this.id = this.element.getAttributeNS(null, "id");
        var e = this.element.getAttributeNS(null, "refX");
        this.refX = e ? parseFloat(e) : 0;
        var t = this.element.getAttributeNS(null, "refY");
        this.refY = t ? parseFloat(t) : 0;
        var n = this.element.getAttributeNS(null, "markerWidth");
        this.markerWidth = n ? parseFloat(n) : 3;
        var i = this.element.getAttributeNS(null, "markerHeight");
        this.markerHeight = i ? parseFloat(i) : 3, this.oldRefX = this.refX, this.oldRefY = this.refY, 
        this.oldMarkerWidth = this.markerWidth, this.oldMarkerHeight = this.markerHeight;
        var r = this.element.getAttributeNS(NAMESPACE_ORYX, "optional");
        r ? (r = r.strip(), this.optional = "yes" === r.toLowerCase()) : this.optional = !1;
        var s = this.element.getAttributeNS(NAMESPACE_ORYX, "enabled");
        s ? (s = s.strip(), this.enabled = !("no" === s.toLowerCase())) : this.enabled = !0;
        var o = this.element.getAttributeNS(NAMESPACE_ORYX, "minimumLength");
        o && (this.minimumLength = parseFloat(o));
        var a = this.element.getAttributeNS(NAMESPACE_ORYX, "resize");
        a ? (a = a.strip(), this.resize = "yes" === a.toLowerCase()) : this.resize = !1;
    },
    _getSVGShapes: function(e) {
        if (e.hasChildNodes) {
            var t = [], n = this;
            return $A(e.childNodes).each(function(e) {
                try {
                    var i = new ORYX.Core.SVG.SVGShape(e);
                    t.push(i);
                } catch (r) {
                    t = t.concat(n._getSVGShapes(e));
                }
            }), t;
        }
    },
    update: function() {
        this.oldRefX = this.refX, this.oldRefY = this.refY, this.oldMarkerWidth = this.markerWidth, 
        this.oldMarkerHeight = this.markerHeight;
    },
    toString: function() {
        return this.element ? "SVGMarker " + this.element.id : "SVGMarker " + this.element;
    }
}), NAMESPACE_ORYX = "http://www.b3mn.org/oryx", NAMESPACE_SVG = "http://www.w3.org/2000/svg/", 
!ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.SVG || (ORYX.Core.SVG = {}), ORYX.Core.SVG.SVGShape = Clazz.extend({
    construct: function(e) {
        arguments.callee.$.construct.apply(this, arguments), this.type, this.element = e, 
        this.x = void 0, this.y = void 0, this.width = void 0, this.height = void 0, this.oldX = void 0, 
        this.oldY = void 0, this.oldWidth = void 0, this.oldHeight = void 0, this.radiusX = void 0, 
        this.radiusY = void 0, this.isHorizontallyResizable = !1, this.isVerticallyResizable = !1, 
        this.anchorLeft = !1, this.anchorRight = !1, this.anchorTop = !1, this.anchorBottom = !1, 
        this.allowDockers = !0, this.resizeMarkerMid = !1, this.editPathParser, this.editPathHandler, 
        this.init();
    },
    init: function() {
        if (ORYX.Editor.checkClassType(this.element, SVGRectElement) || ORYX.Editor.checkClassType(this.element, SVGImageElement)) {
            this.type = "Rect";
            var e = this.element.getAttributeNS(null, "x");
            if (!e) throw "Missing attribute in element " + this.element;
            this.oldX = parseFloat(e);
            var t = this.element.getAttributeNS(null, "y");
            if (!t) throw "Missing attribute in element " + this.element;
            this.oldY = parseFloat(t);
            var n = this.element.getAttributeNS(null, "width");
            if (!n) throw "Missing attribute in element " + this.element;
            this.oldWidth = parseFloat(n);
            var i = this.element.getAttributeNS(null, "height");
            if (!i) throw "Missing attribute in element " + this.element;
            this.oldHeight = parseFloat(i);
        } else if (ORYX.Editor.checkClassType(this.element, SVGCircleElement)) {
            this.type = "Circle";
            var r = void 0, s = void 0, o = this.element.getAttributeNS(null, "cx");
            if (!o) throw "Missing attribute in element " + this.element;
            r = parseFloat(o);
            var a = this.element.getAttributeNS(null, "cy");
            if (!a) throw "Missing attribute in element " + this.element;
            s = parseFloat(a);
            var c = this.element.getAttributeNS(null, "r");
            if (!c) throw "Missing attribute in element " + this.element;
            this.radiusX = parseFloat(c), this.oldX = r - this.radiusX, this.oldY = s - this.radiusX, 
            this.oldWidth = 2 * this.radiusX, this.oldHeight = 2 * this.radiusX;
        } else if (ORYX.Editor.checkClassType(this.element, SVGEllipseElement)) {
            this.type = "Ellipse";
            var r = void 0, s = void 0, o = this.element.getAttributeNS(null, "cx");
            if (!o) throw "Missing attribute in element " + this.element;
            r = parseFloat(o);
            var a = this.element.getAttributeNS(null, "cy");
            if (!a) throw "Missing attribute in element " + this.element;
            s = parseFloat(a);
            var l = this.element.getAttributeNS(null, "rx");
            if (!l) throw "Missing attribute in element " + this.element;
            this.radiusX = parseFloat(l);
            var h = this.element.getAttributeNS(null, "ry");
            if (!h) throw "Missing attribute in element " + this.element;
            this.radiusY = parseFloat(h), this.oldX = r - this.radiusX, this.oldY = s - this.radiusY, 
            this.oldWidth = 2 * this.radiusX, this.oldHeight = 2 * this.radiusY;
        } else if (ORYX.Editor.checkClassType(this.element, SVGLineElement)) {
            this.type = "Line";
            var u = void 0, d = void 0, p = void 0, f = void 0, g = this.element.getAttributeNS(null, "x1");
            if (!g) throw "Missing attribute in element " + this.element;
            u = parseFloat(g);
            var m = this.element.getAttributeNS(null, "y1");
            if (!m) throw "Missing attribute in element " + this.element;
            d = parseFloat(m);
            var O = this.element.getAttributeNS(null, "x2");
            if (!O) throw "Missing attribute in element " + this.element;
            p = parseFloat(O);
            var R = this.element.getAttributeNS(null, "y2");
            if (!R) throw "Missing attribute in element " + this.element;
            f = parseFloat(R), this.oldX = Math.min(u, p), this.oldY = Math.min(d, f), this.oldWidth = Math.abs(u - p), 
            this.oldHeight = Math.abs(d - f);
        } else if (ORYX.Editor.checkClassType(this.element, SVGPolylineElement) || ORYX.Editor.checkClassType(this.element, SVGPolygonElement)) {
            this.type = "Polyline";
            var E = this.element.getAttributeNS(null, "points");
            if (!E) throw "Missing attribute in element " + this.element;
            E = E.replace(/,/g, " ");
            var S = E.split(" ");
            if (S = S.without(""), !(S && S.length && S.length > 1)) throw "Missing attribute in element " + this.element;
            for (var N = parseFloat(S[0]), v = parseFloat(S[1]), _ = parseFloat(S[0]), b = parseFloat(S[1]), C = 0; C < S.length; C++) N = Math.min(N, parseFloat(S[C])), 
            _ = Math.max(_, parseFloat(S[C])), C++, v = Math.min(v, parseFloat(S[C])), b = Math.max(b, parseFloat(S[C]));
            this.oldX = N, this.oldY = v, this.oldWidth = _ - N, this.oldHeight = b - v;
        } else {
            if (!ORYX.Editor.checkClassType(this.element, SVGPathElement)) throw "Element is not a shape.";
            this.type = "Path", this.editPathParser = new PathParser(), this.editPathHandler = new ORYX.Core.SVG.EditPathHandler(), 
            this.editPathParser.setHandler(this.editPathHandler);
            var I = new PathParser(), y = new ORYX.Core.SVG.MinMaxPathHandler();
            I.setHandler(y), I.parsePath(this.element), this.oldX = y.minX, this.oldY = y.minY, 
            this.oldWidth = y.maxX - y.minX, this.oldHeight = y.maxY - y.minY, delete I, delete y;
        }
        var X = this.element.getAttributeNS(NAMESPACE_ORYX, "resize");
        X ? (X = X.toLowerCase(), this.isHorizontallyResizable = X.match(/horizontal/) ? !0 : !1, 
        this.isVerticallyResizable = X.match(/vertical/) ? !0 : !1) : (this.isHorizontallyResizable = !1, 
        this.isVerticallyResizable = !1);
        var Y = this.element.getAttributeNS(NAMESPACE_ORYX, "anchors");
        if (Y) {
            Y = Y.replace("/,/g", " ");
            for (var A = Y.split(" ").without(""), C = 0; C < A.length; C++) switch (A[C].toLowerCase()) {
              case "left":
                this.anchorLeft = !0;
                break;

              case "right":
                this.anchorRight = !0;
                break;

              case "top":
                this.anchorTop = !0;
                break;

              case "bottom":
                this.anchorBottom = !0;
            }
        }
        if (ORYX.Editor.checkClassType(this.element, SVGPathElement)) {
            var x = this.element.getAttributeNS(NAMESPACE_ORYX, "allowDockers");
            x && (this.allowDockers = "no" === x.toLowerCase() ? !1 : !0);
            var T = this.element.getAttributeNS(NAMESPACE_ORYX, "resizeMarker-mid");
            T && (this.resizeMarkerMid = "yes" === T.toLowerCase() ? !0 : !1);
        }
        this.x = this.oldX, this.y = this.oldY, this.width = this.oldWidth, this.height = this.oldHeight;
    },
    update: function() {
        if (this.x !== this.oldX || this.y !== this.oldY || this.width !== this.oldWidth || this.height !== this.oldHeight) {
            switch (this.type) {
              case "Rect":
                this.x !== this.oldX && this.element.setAttributeNS(null, "x", this.x), this.y !== this.oldY && this.element.setAttributeNS(null, "y", this.y), 
                this.width !== this.oldWidth && this.element.setAttributeNS(null, "width", this.width), 
                this.height !== this.oldHeight && this.element.setAttributeNS(null, "height", this.height);
                break;

              case "Circle":
                this.radiusX = (this.width < this.height ? this.width : this.height) / 2, this.element.setAttributeNS(null, "cx", this.x + this.width / 2), 
                this.element.setAttributeNS(null, "cy", this.y + this.height / 2), this.element.setAttributeNS(null, "r", this.radiusX);
                break;

              case "Ellipse":
                this.radiusX = this.width / 2, this.radiusY = this.height / 2, this.element.setAttributeNS(null, "cx", this.x + this.radiusX), 
                this.element.setAttributeNS(null, "cy", this.y + this.radiusY), this.element.setAttributeNS(null, "rx", this.radiusX), 
                this.element.setAttributeNS(null, "ry", this.radiusY);
                break;

              case "Line":
                this.x !== this.oldX && this.element.setAttributeNS(null, "x1", this.x), this.y !== this.oldY && this.element.setAttributeNS(null, "y1", this.y), 
                (this.x !== this.oldX || this.width !== this.oldWidth) && this.element.setAttributeNS(null, "x2", this.x + this.width), 
                (this.y !== this.oldY || this.height !== this.oldHeight) && this.element.setAttributeNS(null, "y2", this.y + this.height);
                break;

              case "Polyline":
                var e = this.element.getAttributeNS(null, "points");
                if (e && (e = e.replace(/,/g, " ").split(" ").without(""), e && e.length && e.length > 1)) {
                    for (var t = 0 === this.oldWidth ? 0 : this.width / this.oldWidth, n = 0 === this.oldHeight ? 0 : this.height / this.oldHeight, i = "", r = 0; r < e.length; r++) {
                        var s = (parseFloat(e[r]) - this.oldX) * t + this.x;
                        r++;
                        var o = (parseFloat(e[r]) - this.oldY) * n + this.y;
                        i += s + " " + o + " ";
                    }
                    this.element.setAttributeNS(null, "points", i);
                }
                break;

              case "Path":
                var t = 0 === this.oldWidth ? 0 : this.width / this.oldWidth, n = 0 === this.oldHeight ? 0 : this.height / this.oldHeight;
                this.editPathHandler.init(this.x, this.y, this.oldX, this.oldY, t, n), this.editPathParser.parsePath(this.element), 
                this.element.setAttributeNS(null, "d", this.editPathHandler.d);
            }
            this.oldX = this.x, this.oldY = this.y, this.oldWidth = this.width, this.oldHeight = this.height;
        }
    },
    isPointIncluded: function(e, t) {
        if (!e || !t || !this.isVisible()) return !1;
        switch (this.type) {
          case "Rect":
            return e >= this.x && e <= this.x + this.width && t >= this.y && t <= this.y + this.height;

          case "Circle":
            return ORYX.Core.Math.isPointInEllipse(e, t, this.x + this.width / 2, this.y + this.height / 2, this.radiusX, this.radiusX);

          case "Ellipse":
            return ORYX.Core.Math.isPointInEllipse(e, t, this.x + this.radiusX, this.y + this.radiusY, this.radiusX, this.radiusY);

          case "Line":
            return ORYX.Core.Math.isPointInLine(e, t, this.x, this.y, this.x + this.width, this.y + this.height);

          case "Polyline":
            var n = this.element.getAttributeNS(null, "points");
            return n ? (n = n.replace(/,/g, " ").split(" ").without(""), n = n.collect(function(e) {
                return parseFloat(e);
            }), ORYX.Core.Math.isPointInPolygone(e, t, n)) : !1;

          case "Path":
            var i = new PathParser(), r = new ORYX.Core.SVG.PointsPathHandler();
            return i.setHandler(r), i.parsePath(this.element), ORYX.Core.Math.isPointInPolygone(e, t, r.points);

          default:
            return !1;
        }
    },
    isVisible: function(e) {
        e || (e = this.element);
        var t = !1;
        try {
            t = !!e.ownerSVGElement;
        } catch (n) {}
        if (t) {
            if (ORYX.Editor.checkClassType(e, SVGGElement) && e.className && "me" == e.className.baseVal) return !0;
            var i = e.getAttributeNS(null, "fill"), r = e.getAttributeNS(null, "stroke");
            if (i && "none" == i && r && "none" == r) return !1;
            var s = e.getAttributeNS(null, "display");
            return s ? "none" == s ? !1 : !0 : this.isVisible(e.parentNode);
        }
        return !0;
    },
    toString: function() {
        return this.element ? "SVGShape " + this.element.id : "SVGShape " + this.element;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.SVG || (ORYX.Core.SVG = {}), ORYX.Core.SVG.Label = Clazz.extend({
    _characterSets: [ "%W", "@", "m", "wDGMOQÖ#+=<>~^", "ABCHKNRSUVXZÜÄ&", "bdghnopquxöüETY1234567890ß_§${}*´`µ€", "aeksvyzäFLP?°²³", "c-", 'rtJ"/()[]:;!|\\', "fjI., ", "'", "il" ],
    _characterSetValues: [ 15, 14, 13, 11, 10, 9, 8, 7, 6, 5, 4, 3 ],
    construct: function(e) {
        if (arguments.callee.$.construct.apply(this, arguments), !e.textElement) throw "Label: No parameter textElement.";
        if (!ORYX.Editor.checkClassType(e.textElement, SVGTextElement)) throw "Label: Parameter textElement is not an SVGTextElement.";
        this.invisibleRenderPoint = -5e3, this.node = e.textElement, this.node.setAttributeNS(null, "stroke-width", "0pt"), 
        this.node.setAttributeNS(null, "letter-spacing", "-0.01px"), this.shapeId = e.shapeId, 
        this.id, this.fitToElemId, this.edgePosition, this.x, this.y, this.oldX, this.oldY, 
        this.isVisible = !0, this._text, this._verticalAlign, this._horizontalAlign, this._rotate, 
        this._rotationPoint, this.anchorLeft, this.anchorRight, this.anchorTop, this.anchorBottom, 
        this._isChanged = !0;
        var t = this.node.getAttributeNS(null, "id");
        t && (this.id = t), this.fitToElemId = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "fittoelem"), 
        this.fitToElemId && (this.fitToElemId = this.shapeId + this.fitToElemId);
        var n = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "align");
        n && (n = n.replace(/,/g, " "), n = n.split(" "), n = n.without(""), n.each(function(e) {
            switch (e) {
              case "top":
              case "middle":
              case "bottom":
                this._verticalAlign || (this._verticalAlign = e);
                break;

              case "left":
              case "center":
              case "right":
                this._horizontalAlign || (this._horizontalAlign = e);
            }
        }.bind(this))), this.edgePosition = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "edgePosition"), 
        this.edgePosition && (this.edgePosition = this.edgePosition.toLowerCase()), this.offsetTop = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "offsetTop") || ORYX.CONFIG.OFFSET_EDGE_LABEL_TOP, 
        this.offsetTop && (this.offsetTop = parseInt(this.offsetTop)), this.offsetBottom = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "offsetBottom") || ORYX.CONFIG.OFFSET_EDGE_LABEL_BOTTOM, 
        this.offsetBottom && (this.offsetBottom = parseInt(this.offsetBottom));
        var i = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "rotate");
        if (i) try {
            this._rotate = parseFloat(i);
        } catch (r) {
            this._rotate = 0;
        } else this._rotate = 0;
        var s = this.node.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "anchors");
        if (s) {
            s = s.replace("/,/g", " ");
            for (var o = s.split(" ").without(""), a = 0; a < o.length; a++) switch (o[a].toLowerCase()) {
              case "left":
                this.anchorLeft = !0;
                break;

              case "right":
                this.anchorRight = !0;
                break;

              case "top":
                this.anchorTop = !0;
                break;

              case "bottom":
                this.anchorBottom = !0;
            }
        }
        this._verticalAlign || (this._verticalAlign = "bottom"), this._horizontalAlign || (this._horizontalAlign = "left");
        var c = this.node.getAttributeNS(null, "x");
        c && (this.x = parseFloat(c), this.oldX = this.x);
        var l = this.node.getAttributeNS(null, "y");
        l && (this.y = parseFloat(l), this.oldY = this.y), this.text(this.node.textContent);
    },
    changed: function() {
        this._isChanged = !0;
    },
    update: function() {
        if (this._isChanged || this.x !== this.oldX || this.y !== this.oldY) if (this.isVisible) {
            switch (this._isChanged = !1, this.node.setAttributeNS(null, "x", this.x), this.node.setAttributeNS(null, "y", this.y), 
            this._fontSize && this.node.setAttributeNS(null, "font-size", this._fontSize), this._horizontalAlign) {
              case "left":
                this.node.setAttributeNS(null, "text-anchor", "start");
                break;

              case "center":
                this.node.setAttributeNS(null, "text-anchor", "middle");
                break;

              case "right":
                this.node.setAttributeNS(null, "text-anchor", "end");
            }
            this.oldX = this.x, this.oldY = this.y, void 0 !== this._rotate && (this._rotationPoint ? this.node.setAttributeNS(null, "transform", "rotate(" + this._rotate + " " + this._rotationPoint.x + " " + this._rotationPoint.y + ")") : this.node.setAttributeNS(null, "transform", "rotate(" + this._rotate + " " + this.x + " " + this.y + ")"));
            for (var e = this._text.split("\n"); "" == e.last(); ) e.pop();
            this.node.textContent = "", this.node.ownerDocument && (e.each(function(e) {
                var t = this.node.ownerDocument.createElementNS(ORYX.CONFIG.NAMESPACE_SVG, "tspan");
                t.textContent = e, t.setAttributeNS(null, "x", this.invisibleRenderPoint), t.setAttributeNS(null, "y", this.invisibleRenderPoint), 
                "" === t.textContent && (t.textContent = " "), this.node.appendChild(t);
            }.bind(this)), this.isVisible && this.node.setAttributeNS(null, "visibility", "hidden"), 
            this.fitToElemId ? window.setTimeout(this._checkFittingToReferencedElem.bind(this), 0) : window.setTimeout(this._positionText.bind(this), 0));
        } else this.node.textContent = "";
    },
    _checkFittingToReferencedElem: function() {
        try {
            var e = $A(this.node.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "tspan")), t = [], n = this.node.ownerDocument.getElementById(this.fitToElemId);
            if (n) {
                for (var i = n.getBBox(), r = this.getFontSize(), s = 0; s < e.length; s++) {
                    var o = e[s], a = this._getRenderedTextLength(o, void 0, void 0, r), c = 0 != this._rotate && 0 != this._rotate % 180 && 0 == this._rotate % 90 ? i.height : i.width;
                    if (a > c) {
                        for (var l = 0, h = 0, u = this.getTrimmedTextLength(o.textContent), d = 0; u > d; d++) {
                            var p = this._getRenderedTextLength(o, l, d - l, r);
                            if (p > c - 3) {
                                var f = this.node.ownerDocument.createElementNS(ORYX.CONFIG.NAMESPACE_SVG, "tspan");
                                l >= h ? (h = 0 == d ? d : d - 1, f.textContent = o.textContent.slice(l, h)) : f.textContent = o.textContent.slice(l, ++h), 
                                f.setAttributeNS(null, "x", this.invisibleRenderPoint), f.setAttributeNS(null, "y", this.invisibleRenderPoint), 
                                t.push(f), l = h;
                            } else {
                                var g = o.textContent.charAt(d);
                                (" " == g || "-" == g || "." == g || "," == g || ";" == g || ":" == g) && (h = d);
                            }
                        }
                        o.textContent = o.textContent.slice(l);
                    }
                    t.push(o);
                }
                for (;this.node.hasChildNodes(); ) this.node.removeChild(this.node.childNodes[0]);
                for (;t.length > 0; ) this.node.appendChild(t.shift());
            }
        } catch (m) {}
        window.setTimeout(this._positionText.bind(this), 0);
    },
    _positionText: function() {
        try {
            var e = this.node.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "tspan"), t = this.getFontSize(this.node), n = [];
            $A(e).each(function(i, r) {
                if ("" === i.textContent.trim()) n.push(i); else {
                    var s = 0;
                    switch (this._verticalAlign) {
                      case "bottom":
                        s = -(e.length - r - 1) * t;
                        break;

                      case "middle":
                        s = -(e.length / 2 - r - 1) * t, s -= ORYX.CONFIG.LABEL_LINE_DISTANCE / 2;
                        break;

                      case "top":
                        s = r * t, s += t;
                    }
                    i.setAttributeNS(null, "dy", s), i.setAttributeNS(null, "x", this.x), i.setAttributeNS(null, "y", this.y);
                }
            }.bind(this)), n.each(function(e) {
                this.node.removeChild(e);
            }.bind(this));
        } catch (i) {
            this._isChanged = !0;
        }
        this.isVisible && this.node.setAttributeNS(null, "visibility", "inherit");
    },
    _getRenderedTextLength: function(e, t, n, i) {
        return /Firefox[\/\s](\d+\.\d+)/.test(navigator.userAgent) && new Number(RegExp.$1) >= 3 ? void 0 === t ? e.getComputedTextLength() : e.getSubStringLength(t, n) : void 0 === t ? this._estimateTextWidth(e.textContent, i) : this._estimateTextWidth(e.textContent.substr(t, n).trim(), i);
    },
    _estimateTextWidth: function(e, t) {
        for (var n = 0, i = 0; i < e.length; i++) n += this._estimateCharacterWidth(e.charAt(i));
        return n * (t / 14);
    },
    _estimateCharacterWidth: function(e) {
        for (var t = 0; t < this._characterSets.length; t++) if (this._characterSets[t].indexOf(e) >= 0) return this._characterSetValues[t];
        return 9;
    },
    getReferencedElementWidth: function() {
        var e = this.node.ownerDocument.getElementById(this.fitToElemId);
        if (e) {
            var t = e.getBBox();
            return t ? t.width : void 0;
        }
        return void 0;
    },
    text: function() {
        switch (arguments.length) {
          case 0:
            return this._text;

          case 1:
            var e = this._text;
            this._text = arguments[0] ? arguments[0].toString() : "", e !== this._text && (this._isChanged = !0);
        }
    },
    fontSize: function() {
        switch (arguments.length) {
          case 0:
            return this._fontSize;

          case 1:
            var e = this._fontSize;
            this._fontSize = arguments[0] ? arguments[0].toString() : 12, e !== this._fontSize && (this._isChanged = !0);
        }
    },
    verticalAlign: function() {
        switch (arguments.length) {
          case 0:
            return this._verticalAlign;

          case 1:
            if ([ "top", "middle", "bottom" ].member(arguments[0])) {
                var e = this._verticalAlign;
                this._verticalAlign = arguments[0], this._verticalAlign !== e && (this._isChanged = !0);
            }
        }
    },
    horizontalAlign: function() {
        switch (arguments.length) {
          case 0:
            return this._horizontalAlign;

          case 1:
            if ([ "left", "center", "right" ].member(arguments[0])) {
                var e = this._horizontalAlign;
                this._horizontalAlign = arguments[0], this._horizontalAlign !== e && (this._isChanged = !0);
            }
        }
    },
    rotate: function() {
        switch (arguments.length) {
          case 0:
            return this._rotate;

          case 1:
            this._rotate != arguments[0] && (this._rotate = arguments[0], this._rotationPoint = void 0, 
            this._isChanged = !0);

          case 2:
            this._rotate == arguments[0] && this._rotationPoint && this._rotationPoint.x == arguments[1].x && this._rotationPoint.y == arguments[1].y || (this._rotate = arguments[0], 
            this._rotationPoint = arguments[1], this._isChanged = !0);
        }
    },
    hide: function() {
        this.isVisible && (this.isVisible = !1, this._isChanged = !0);
    },
    show: function() {
        this.isVisible || (this.isVisible = !0, this._isChanged = !0);
    },
    getInheritedFontSize: function(e) {
        if (e && e.getAttributeNS) {
            var t = e.getAttributeNS(null, "font-size");
            return t ? parseFloat(t) : ORYX.Editor.checkClassType(e, SVGSVGElement) ? void 0 : this.getInheritedFontSize(e.parentNode);
        }
    },
    getFontSize: function() {
        var e = this.node.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "tspan"), t = this.getInheritedFontSize(this.node);
        return t || (t = e[0] && /Firefox[\/\s](\d+\.\d+)/.test(navigator.userAgent) && new Number(RegExp.$1) >= 3 ? e[0].getExtentOfChar(0).height : ORYX.CONFIG.LABEL_DEFAULT_LINE_HEIGHT, 
        0 >= t && (t = ORYX.CONFIG.LABEL_DEFAULT_LINE_HEIGHT)), t && this.node.setAttribute("oryx:fontSize", t), 
        t;
    },
    getTrimmedTextLength: function(e) {
        e = e.strip().gsub("  ", " ");
        var t;
        do t = e.length, e = e.gsub("  ", " "); while (t > e.length);
        return e.length;
    },
    getOffsetBottom: function() {
        return this.offsetBottom;
    },
    getOffsetTop: function() {
        return this.offsetTop;
    },
    toString: function() {
        return "Label " + this.id;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Math || (ORYX.Core.Math = {}), ORYX.Core.Math.midPoint = function(e, t) {
    return {
        x: (e.x + t.x) / 2,
        y: (e.y + t.y) / 2
    };
}, ORYX.Core.Math.isPointInLine = function(e, t, n, i, r, s, o) {
    if (o = o ? Math.abs(o) : 1, Math.abs(n - r) <= o && Math.abs(e - n) <= o && t - Math.max(i, s) <= o && Math.min(i, s) - t <= o) return !0;
    if (Math.abs(i - s) <= o && Math.abs(t - i) <= o && e - Math.max(n, r) <= o && Math.min(n, r) - e <= o) return !0;
    if (e > Math.max(n, r) || e < Math.min(n, r)) return !1;
    if (t > Math.max(i, s) || t < Math.min(i, s)) return !1;
    var a = (i - s) / (n - r);
    return Math.abs(t - (a * e + i - a * n)) < o;
}, ORYX.Core.Math.isPointInEllipse = function(e, t, n, i, r, s) {
    if (void 0 === n || void 0 === i || void 0 === r || void 0 === s) throw "ORYX.Core.Math.isPointInEllipse needs a ellipse with these properties: x, y, radiusX, radiusY";
    var o = (e - n) / r, a = (t - i) / s;
    return 1 > o * o + a * a;
}, ORYX.Core.Math.isPointInPolygone = function(e, t, n) {
    if (arguments.length < 3) throw "ORYX.Core.Math.isPointInPolygone needs two arguments";
    var i = n.length - 1;
    (n[0] !== n[i - 1] || n[1] !== n[i]) && (n.push(n[0]), n.push(n[1]));
    for (var r, s, o, a, c, l = 0, h = 0; h < n.length - 3; ) if (r = n[h], s = n[++h], 
    o = n[++h], a = n[h + 1], c = (t - s) * (o - r) - (e - r) * (a - s), s >= t != a >= t && (l += a - s >= 0 ? c >= 0 : 0 >= c), 
    !c && Math.min(r, o) <= e && e <= Math.max(r, o) && Math.min(s, a) <= t && t <= Math.max(s, a)) return !0;
    return l % 2 ? !0 : !1;
}, ORYX.Core.Math.distancePointLinie = function(e, t, n, i) {
    var r = ORYX.Core.Math.getPointOfIntersectionPointLine(e, t, n, i);
    return r ? ORYX.Core.Math.getDistancePointToPoint(n, r) : null;
}, ORYX.Core.Math.getDistancePointToPoint = function(e, t) {
    return Math.sqrt(Math.pow(e.x - t.x, 2) + Math.pow(e.y - t.y, 2));
}, ORYX.Core.Math.getPointOfIntersectionPointLine = function(e, t, n, i) {
    var r = Math.pow(t.x - e.x, 2) + Math.pow(t.y - e.y, 2);
    if (0 == r) return void 0;
    var s = ((n.x - e.x) * (t.x - e.x) + (n.y - e.y) * (t.y - e.y)) / r;
    return !i || s >= 0 && 1 >= s ? (pointOfIntersection = new Object(), pointOfIntersection.x = e.x + s * (t.x - e.x), 
    pointOfIntersection.y = e.y + s * (t.y - e.y), pointOfIntersection) : void 0;
}, !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet.Stencil = {
    construct: function(e, t, n, i, r, s) {
        if (arguments.callee.$.construct.apply(this, arguments), !e) throw "Stencilset seems corrupt.";
        if (!t) throw "Stencil does not provide namespace.";
        if (!n) throw "Stencil does not provide SVG source.";
        if (!i) throw "Fatal internal error loading stencilset.";
        if (this._source = n, this._jsonStencil = e, this._stencilSet = i, this._namespace = t, 
        this._propertyPackages = r, s && !this._jsonStencil.position && (this._jsonStencil.position = s), 
        this._view, this._properties = new Hash(), !this._jsonStencil.type || "edge" !== this._jsonStencil.type && "node" !== this._jsonStencil.type) throw "ORYX.Core.StencilSet.Stencil(construct): Type is not defined.";
        if (!this._jsonStencil.id || "" === this._jsonStencil.id) throw "ORYX.Core.StencilSet.Stencil(construct): Id is not defined.";
        if (!this._jsonStencil.title || "" === this._jsonStencil.title) throw "ORYX.Core.StencilSet.Stencil(construct): Title is not defined.";
        if (this._jsonStencil.description || (this._jsonStencil.description = ""), this._jsonStencil.groups || (this._jsonStencil.groups = []), 
        this._jsonStencil.roles || (this._jsonStencil.roles = []), this._jsonStencil.roles.push(this._jsonStencil.id), 
        this._jsonStencil.roles.each(function(e, n) {
            this._jsonStencil.roles[n] = t + e;
        }.bind(this)), this._jsonStencil.roles = this._jsonStencil.roles.uniq(), this._jsonStencil.id = t + this._jsonStencil.id, 
        this.postProcessProperties(), this._jsonStencil.serialize || (this._jsonStencil.serialize = {}), 
        this._jsonStencil.deserialize || (this._jsonStencil.deserialize = {}), this._jsonStencil.layout || (this._jsonStencil.layout = []), 
        void 0 === e.view) ; else if ("/" == e.view.charAt(0)) var o = e.view; else var o = n + "view/" + e.view;
        if (this._jsonStencil.view) if (this._jsonStencil.view.trim().match(/</)) {
            var a = new DOMParser(), c = a.parseFromString(this._jsonStencil.view, "text/xml");
            if (!ORYX.Editor.checkClassType(c.documentElement, SVGSVGElement)) throw "ORYX.Core.StencilSet.Stencil(_loadSVGOnSuccess): The response is not a SVG document.";
            this._view = c.documentElement;
            var l = this._view.getElementsByTagNameNS("http://www.w3.org/2000/svg", "image");
            $A(l).each(function(e) {
                var t = e.getAttributeNodeNS("http://www.w3.org/1999/xlink", "href");
                t && -1 == t.value.indexOf("://") && -1 == t.value.indexOf("base64") && (t.textContent = this._source + "view/" + t.value);
            }.bind(this));
        } else new Ajax.Request(o, {
            asynchronous: !1,
            method: "get",
            onSuccess: this._loadSVGOnSuccess.bind(this),
            onFailure: this._loadSVGOnFailure.bind(this)
        });
    },
    postProcessProperties: function() {
        this._jsonStencil.icon ? "/" === this._jsonStencil.icon.charAt(0) || -1 === this._jsonStencil.icon.indexOf("://") && -1 === this._jsonStencil.icon.indexOf("base64") && (this._jsonStencil.icon = this._source + "icons/" + this._jsonStencil.icon) : this._jsonStencil.icon = "", 
        this._jsonStencil.propertyPackages && this._jsonStencil.propertyPackages instanceof Array && this._jsonStencil.propertyPackages.each(function(e) {
            var t = this._propertyPackages[e];
            t && t.each(function(e) {
                var t = new ORYX.Core.StencilSet.Property(e, this._namespace, this);
                this._properties[t.prefix() + "-" + t.id()] = t;
            }.bind(this));
        }.bind(this)), this._jsonStencil.properties && this._jsonStencil.properties instanceof Array && this._jsonStencil.properties.each(function(e) {
            var t = new ORYX.Core.StencilSet.Property(e, this._namespace, this);
            this._properties[t.prefix() + "-" + t.id()] = t;
        }.bind(this));
    },
    equals: function(e) {
        return this.id() === e.id();
    },
    stencilSet: function() {
        return this._stencilSet;
    },
    type: function() {
        return this._jsonStencil.type;
    },
    namespace: function() {
        return this._namespace;
    },
    id: function() {
        return this._jsonStencil.id;
    },
    idWithoutNs: function() {
        return this.id().replace(this.namespace(), "");
    },
    title: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonStencil, "title");
    },
    description: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonStencil, "description");
    },
    groups: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonStencil, "groups");
    },
    position: function() {
        return isNaN(this._jsonStencil.position) ? 0 : this._jsonStencil.position;
    },
    view: function() {
        return this._view.cloneNode(!0) || this._view;
    },
    hidden: function() {
        return this._jsonStencil.hide;
    },
    icon: function() {
        return this._jsonStencil.icon;
    },
    fixedAspectRatio: function() {
        return this._jsonStencil.fixedAspectRatio === !0;
    },
    hasMultipleRepositoryEntries: function() {
        return this.getRepositoryEntries().length > 0;
    },
    getRepositoryEntries: function() {
        return this._jsonStencil.repositoryEntries ? $A(this._jsonStencil.repositoryEntries) : $A([]);
    },
    properties: function() {
        return this._properties.values();
    },
    property: function(e) {
        return this._properties[e];
    },
    roles: function() {
        return this._jsonStencil.roles;
    },
    defaultAlign: function() {
        return this._jsonStencil.defaultAlign ? this._jsonStencil.defaultAlign : "east";
    },
    serialize: function() {
        return this._jsonStencil.serialize;
    },
    deserialize: function() {
        return this._jsonStencil.deserialize;
    },
    layout: function() {
        return this._jsonStencil.layout;
    },
    addProperty: function(e, t) {
        if (e && t) {
            var n = new ORYX.Core.StencilSet.Property(e, t, this);
            this._properties[n.prefix() + "-" + n.id()] = n;
        }
    },
    removeProperty: function(e) {
        if (e) {
            var t = this._properties.values().find(function(t) {
                return e == t.id();
            });
            t && delete this._properties[t.prefix() + "-" + t.id()];
        }
    },
    _loadSVGOnSuccess: function(e) {
        var t = null;
        if (t = e.responseXML, !ORYX.Editor.checkClassType(t.documentElement, SVGSVGElement)) throw "ORYX.Core.StencilSet.Stencil(_loadSVGOnSuccess): The response is not a SVG document.";
        this._view = t.documentElement;
        var n = this._view.getElementsByTagNameNS("http://www.w3.org/2000/svg", "image");
        $A(n).each(function(e) {
            var t = e.getAttributeNodeNS("http://www.w3.org/1999/xlink", "href");
            t && -1 == t.value.indexOf("://") && -1 == t.value.indexOf("base64") && (t.textContent = this._source + "view/" + t.value);
        }.bind(this));
    },
    _loadSVGOnFailure: function() {
        throw "ORYX.Core.StencilSet.Stencil(_loadSVGOnFailure): Loading SVG document failed.";
    },
    toString: function() {
        return "Stencil " + this.title() + " (" + this.id() + ")";
    }
}, ORYX.Core.StencilSet.Stencil = Clazz.extend(ORYX.Core.StencilSet.Stencil), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet.Property = Clazz.extend({
    construct: function(e, t, n) {
        if (arguments.callee.$.construct.apply(this, arguments), this._jsonProp = e || ORYX.Log.error("Parameter jsonProp is not defined."), 
        this._namespace = t || ORYX.Log.error("Parameter namespace is not defined."), this._stencil = n || ORYX.Log.error("Parameter stencil is not defined."), 
        this._items = new Hash(), this._complexItems = new Hash(), e.id = e.id || ORYX.Log.error("ORYX.Core.StencilSet.Property(construct): Id is not defined."), 
        e.id = e.id.toLowerCase(), e.type ? e.type = e.type.toLowerCase() : (ORYX.Log.info("Type is not defined for stencil '%0', id '%1'. Falling back to 'String'.", n, e.id), 
        e.type = "string"), e.prefix = e.prefix || "oryx", e.title = e.title || "", e.value = e.value || "", 
        e.description = e.description || "", e.readonly = e.readonly || !1, 0 != e.optional && (e.optional = !0), 
        this._jsonProp.refToView ? this._jsonProp.refToView instanceof Array || (this._jsonProp.refToView = [ this._jsonProp.refToView ]) : this._jsonProp.refToView = [], 
        (void 0 === e.min || null === e.min) && (e.min = Number.MIN_VALUE), (void 0 === e.max || null === e.max) && (e.max = Number.MAX_VALUE), 
        e.fillOpacity || (e.fillOpacity = !1), e.strokeOpacity || (e.strokeOpacity = !1), 
        (void 0 === e.length || null === e.length) && (e.length = Number.MAX_VALUE), e.wrapLines || (e.wrapLines = !1), 
        e.dateFormat || (e.dataFormat = "m/d/y"), e.fill || (e.fill = !1), e.stroke || (e.stroke = !1), 
        e.inverseBoolean || (e.inverseBoolean = !1), e.directlyEditable || 0 == e.directlyEditable || (e.directlyEditable = !0), 
        e.visible || (e.visible = !0), e.fortasktypes || (e.fortasktypes = ""), e.ifproptrue || (e.ifproptrue = ""), 
        e.fordistribution || (e.fordistribution = ""), e.popular || (e.popular = !1), e.simulation || (e.simulation = !1), 
        e.complexItems && e.complexItems instanceof Array && e.complexItems.each(function(n) {
            try {
                this._complexItems[n.id] = new ORYX.Core.StencilSet.ComplexPropertyItem(n, t, this);
            } catch (i) {
                ORYX.Log.error("error while initializing complex items for " + e.title), ORYX.Log.error(i);
            }
        }.bind(this)), e.type === ORYX.CONFIG.TYPE_CHOICE || e.type === ORYX.CONFIG.TYPE_DYNAMICCHOICE) {
            if (!(e.items && e.items instanceof Array)) throw "ORYX.Core.StencilSet.Property(construct): No property items defined.";
            e.items.each(function(e) {
                this._items[e.value] = new ORYX.Core.StencilSet.PropertyItem(e, t, this);
            }.bind(this));
        }
        if (e.type === ORYX.CONFIG.TYPE_COMPLEX && void 0 === e.complexItems) throw "ORYX.Core.StencilSet.Property(construct): No complex property items defined.";
        e.labelProvider && (this._labelProvider = e.labelProvider.transform);
    },
    equals: function(e) {
        return this._namespace === e.namespace() && this.id() === e.id() ? !0 : !1;
    },
    namespace: function() {
        return this._namespace;
    },
    stencil: function() {
        return this._stencil;
    },
    id: function() {
        return this._jsonProp.id;
    },
    prefix: function() {
        return this._jsonProp.prefix;
    },
    type: function() {
        return this._jsonProp.type;
    },
    inverseBoolean: function() {
        return this._jsonProp.inverseBoolean;
    },
    popular: function() {
        return this._jsonProp.popular;
    },
    simulation: function() {
        return this._jsonProp.simulation;
    },
    setPopular: function() {
        this._jsonProp.popular = !0;
    },
    setSimulation: function() {
        this._jsonProp.simulation = !0;
    },
    directlyEditable: function() {
        return this._jsonProp.directlyEditable;
    },
    visible: function() {
        return this._jsonProp.visible;
    },
    fortasktypes: function() {
        return this._jsonProp.fortasktypes;
    },
    ifproptrue: function() {
        return this._jsonProp.ifproptrue;
    },
    fordistribution: function() {
        return this._jsonProp.fordistribution;
    },
    title: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonProp, "title");
    },
    value: function() {
        return this._jsonProp.value;
    },
    readonly: function() {
        return this._jsonProp.readonly;
    },
    optional: function() {
        return this._jsonProp.optional;
    },
    description: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonProp, "description");
    },
    refToView: function() {
        return this._jsonProp.refToView;
    },
    min: function() {
        return this._jsonProp.min;
    },
    max: function() {
        return this._jsonProp.max;
    },
    fillOpacity: function() {
        return this._jsonProp.fillOpacity;
    },
    strokeOpacity: function() {
        return this._jsonProp.strokeOpacity;
    },
    length: function() {
        return this._jsonProp.length ? this._jsonProp.length : Number.MAX_VALUE;
    },
    wrapLines: function() {
        return this._jsonProp.wrapLines;
    },
    dateFormat: function() {
        return this._jsonProp.dateFormat;
    },
    fill: function() {
        return this._jsonProp.fill;
    },
    stroke: function() {
        return this._jsonProp.stroke;
    },
    items: function() {
        return this._items.values();
    },
    item: function(e) {
        return this._items[e];
    },
    toString: function() {
        return "Property " + this.title() + " (" + this.id() + ")";
    },
    complexItems: function() {
        return this._complexItems.values();
    },
    complexItem: function(e) {
        return this._complexItems[e];
    },
    complexAttributeToView: function() {
        return this._jsonProp.complexAttributeToView || "";
    },
    labelProvider: function() {
        return this._labelProvider;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet.PropertyItem = Clazz.extend({
    construct: function(e, t, n) {
        if (arguments.callee.$.construct.apply(this, arguments), !e) throw "ORYX.Core.StencilSet.PropertyItem(construct): Parameter jsonItem is not defined.";
        if (!t) throw "ORYX.Core.StencilSet.PropertyItem(construct): Parameter namespace is not defined.";
        if (!n) throw "ORYX.Core.StencilSet.PropertyItem(construct): Parameter property is not defined.";
        if (this._jsonItem = e, this._namespace = t, this._property = n, !e.value) throw "ORYX.Core.StencilSet.PropertyItem(construct): Value is not defined.";
        this._jsonItem.refToView ? this._jsonItem.refToView instanceof Array || (this._jsonItem.refToView = [ this._jsonItem.refToView ]) : this._jsonItem.refToView = [];
    },
    equals: function(e) {
        return this.property().equals(e.property()) && this.value() === e.value();
    },
    namespace: function() {
        return this._namespace;
    },
    property: function() {
        return this._property;
    },
    value: function() {
        return this._jsonItem.value;
    },
    title: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonItem, "title");
    },
    refToView: function() {
        return this._jsonItem.refToView;
    },
    icon: function() {
        return this._jsonItem.icon ? this.property().stencil()._source + "icons/" + this._jsonItem.icon : "";
    },
    toString: function() {
        return "PropertyItem " + this.property() + " (" + this.value() + ")";
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet.ComplexPropertyItem = Clazz.extend({
    construct: function(e, t, n) {
        if (arguments.callee.$.construct.apply(this, arguments), !e) throw "ORYX.Core.StencilSet.ComplexPropertyItem(construct): Parameter jsonItem is not defined.";
        if (!t) throw "ORYX.Core.StencilSet.ComplexPropertyItem(construct): Parameter namespace is not defined.";
        if (!n) throw "ORYX.Core.StencilSet.ComplexPropertyItem(construct): Parameter property is not defined.";
        if (this._jsonItem = e, this._namespace = t, this._property = n, this._items = new Hash(), 
        !e.name) throw "ORYX.Core.StencilSet.ComplexPropertyItem(construct): Name is not defined.";
        if (!e.type) throw "ORYX.Core.StencilSet.ComplexPropertyItem(construct): Type is not defined.";
        if (e.type = e.type.toLowerCase(), e.type === ORYX.CONFIG.TYPE_CHOICE || e.type === ORYX.CONFIG.TYPE_DYNAMICCHOICE) {
            if (!(e.items && e.items instanceof Array)) throw "ORYX.Core.StencilSet.Property(construct): No property items defined.";
            e.items.each(function(e) {
                this._items[e.value] = new ORYX.Core.StencilSet.PropertyItem(e, t, this);
            }.bind(this));
        }
    },
    equals: function(e) {
        return this.property().equals(e.property()) && this.name() === e.name();
    },
    namespace: function() {
        return this._namespace;
    },
    property: function() {
        return this._property;
    },
    name: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonItem, "name");
    },
    id: function() {
        return this._jsonItem.id;
    },
    type: function() {
        return this._jsonItem.type;
    },
    optional: function() {
        return this._jsonItem.optional;
    },
    width: function() {
        return this._jsonItem.width;
    },
    value: function() {
        return this._jsonItem.value;
    },
    items: function() {
        return this._items.values();
    },
    disable: function() {
        return this._jsonItem.disable;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet.Rules = {
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this._stencilSets = [], this._stencils = [], 
        this._cachedConnectSET = new Hash(), this._cachedConnectSE = new Hash(), this._cachedConnectTE = new Hash(), 
        this._cachedCardSE = new Hash(), this._cachedCardTE = new Hash(), this._cachedContainPC = new Hash(), 
        this._cachedMorphRS = new Hash(), this._connectionRules = new Hash(), this._cardinalityRules = new Hash(), 
        this._containmentRules = new Hash(), this._morphingRules = new Hash(), this._layoutRules = new Hash();
    },
    initializeRules: function(e) {
        var t = this._stencilSets.find(function(t) {
            return t.namespace() == e.namespace();
        });
        if (t) {
            var n = this._stencilSets.clone();
            return n = n.without(t), n.push(e), this._stencilSets = [], this._stencils = [], 
            this._cachedConnectSET = new Hash(), this._cachedConnectSE = new Hash(), this._cachedConnectTE = new Hash(), 
            this._cachedCardSE = new Hash(), this._cachedCardTE = new Hash(), this._cachedContainPC = new Hash(), 
            this._cachedMorphRS = new Hash(), this._connectionRules = new Hash(), this._cardinalityRules = new Hash(), 
            this._containmentRules = new Hash(), this._morphingRules = new Hash(), this._layoutRules = new Hash(), 
            n.each(function(e) {
                this.initializeRules(e);
            }.bind(this)), void 0;
        }
        this._stencilSets.push(e);
        var r = new Hash(e.jsonRules()), s = e.namespace(), o = e.stencils();
        e.extensions().values().each(function(e) {
            e.rules && (e.rules.connectionRules && (r.connectionRules = r.connectionRules.concat(e.rules.connectionRules)), 
            e.rules.cardinalityRules && (r.cardinalityRules = r.cardinalityRules.concat(e.rules.cardinalityRules)), 
            e.rules.containmentRules && (r.containmentRules = r.containmentRules.concat(e.rules.containmentRules)), 
            e.rules.morphingRules && (r.morphingRules = r.morphingRules.concat(e.rules.morphingRules))), 
            e.stencils && (o = o.concat(e.stencils));
        }), this._stencils = this._stencils.concat(e.stencils());
        var a = this._connectionRules;
        r.connectionRules && r.connectionRules.each(function(e) {
            this._isRoleOfOtherNamespace(e.role) ? a[e.role] || (a[e.role] = new Hash()) : a[s + e.role] || (a[s + e.role] = new Hash()), 
            e.connects.each(function(t) {
                var n = [];
                t.to && (t.to instanceof Array || (t.to = [ t.to ]), t.to.each(function(e) {
                    this._isRoleOfOtherNamespace(e) ? n.push(e) : n.push(s + e);
                }.bind(this)));
                var i, r;
                i = this._isRoleOfOtherNamespace(e.role) ? e.role : s + e.role, r = this._isRoleOfOtherNamespace(t.from) ? t.from : s + t.from, 
                a[i][r] = a[i][r] ? a[i][r].concat(n) : n;
            }.bind(this));
        }.bind(this));
        var c = this._cardinalityRules;
        r.cardinalityRules && r.cardinalityRules.each(function(e) {
            var t;
            if (t = this._isRoleOfOtherNamespace(e.role) ? e.role : s + e.role, !c[t]) {
                c[t] = {};
                for (i in e) c[t][i] = e[i];
            }
            var n = new Hash();
            e.outgoingEdges && e.outgoingEdges.each(function(e) {
                this._isRoleOfOtherNamespace(e.role) ? n[e.role] = e : n[s + e.role] = e;
            }.bind(this)), c[t].outgoingEdges = n;
            var r = new Hash();
            e.incomingEdges && e.incomingEdges.each(function(e) {
                this._isRoleOfOtherNamespace(e.role) ? r[e.role] = e : r[s + e.role] = e;
            }.bind(this)), c[t].incomingEdges = r;
        }.bind(this));
        var l = this._containmentRules;
        r.containmentRules && r.containmentRules.each(function(e) {
            var t;
            t = this._isRoleOfOtherNamespace(e.role) ? e.role : s + e.role, l[t] || (l[t] = []), 
            e.contains.each(function(e) {
                this._isRoleOfOtherNamespace(e) ? l[t].push(e) : l[t].push(s + e);
            }.bind(this));
        }.bind(this));
        var h = this._morphingRules;
        r.morphingRules && r.morphingRules.each(function(e) {
            var t;
            t = this._isRoleOfOtherNamespace(e.role) ? e.role : s + e.role, h[t] || (h[t] = []), 
            e.preserveBounds || (e.preserveBounds = !1), e.baseMorphs.each(function(e) {
                h[t].push(this._getStencilById(s + e));
            }.bind(this));
        }.bind(this));
        var u = this._layoutRules;
        if (r.layoutRules) {
            var d = function(e) {
                return {
                    edgeRole: e.edgeRole || void 0,
                    t: e.t || 1,
                    r: e.r || 1,
                    b: e.b || 1,
                    l: e.l || 1
                };
            };
            r.layoutRules.each(function(e) {
                var t;
                t = this._isRoleOfOtherNamespace(e.role) ? e.role : s + e.role, u[t] || (u[t] = {}), 
                e["in"] && (u[t]["in"] = d(e["in"])), e.ins && (u[t].ins = (e.ins || []).map(function(e) {
                    return d(e);
                })), e.out && (u[t].out = d(e.out)), e.outs && (u[t].outs = (e.outs || []).map(function(e) {
                    return d(e);
                }));
            }.bind(this));
        }
    },
    _getStencilById: function(e) {
        return this._stencils.find(function(t) {
            return t.id() == e;
        });
    },
    _cacheConnect: function(e) {
        if (result = this._canConnect(e), e.sourceStencil && e.targetStencil) {
            var t = this._cachedConnectSET[e.sourceStencil.id()];
            t || (t = new Hash(), this._cachedConnectSET[e.sourceStencil.id()] = t);
            var n = t[e.edgeStencil.id()];
            n || (n = new Hash(), t[e.edgeStencil.id()] = n), n[e.targetStencil.id()] = result;
        } else if (e.sourceStencil) {
            var t = this._cachedConnectSE[e.sourceStencil.id()];
            t || (t = new Hash(), this._cachedConnectSE[e.sourceStencil.id()] = t), t[e.edgeStencil.id()] = result;
        } else {
            var i = this._cachedConnectTE[e.targetStencil.id()];
            i || (i = new Hash(), this._cachedConnectTE[e.targetStencil.id()] = i), i[e.edgeStencil.id()] = result;
        }
        return result;
    },
    _cacheCard: function(e) {
        if (e.sourceStencil) {
            var t = this._cachedCardSE[e.sourceStencil.id()];
            t || (t = new Hash(), this._cachedCardSE[e.sourceStencil.id()] = t);
            var n = this._getMaximumNumberOfOutgoingEdge(e);
            void 0 == n && (n = -1), t[e.edgeStencil.id()] = n;
        }
        if (e.targetStencil) {
            var i = this._cachedCardTE[e.targetStencil.id()];
            i || (i = new Hash(), this._cachedCardTE[e.targetStencil.id()] = i);
            var n = this._getMaximumNumberOfIncomingEdge(e);
            void 0 == n && (n = -1), i[e.edgeStencil.id()] = n;
        }
    },
    _cacheContain: function(e) {
        var t = [ this._canContain(e), this._getMaximumOccurrence(e.containingStencil, e.containedStencil) ];
        void 0 == t[1] && (t[1] = -1);
        var n = this._cachedContainPC[e.containingStencil.id()];
        return n || (n = new Hash(), this._cachedContainPC[e.containingStencil.id()] = n), 
        n[e.containedStencil.id()] = t, t;
    },
    _cacheMorph: function(e) {
        var t = this._cachedMorphRS[e];
        return t || (t = [], this._morphingRules.keys().include(e) && (t = this._stencils.select(function(t) {
            return t.roles().include(e);
        })), this._cachedMorphRS[e] = t), t;
    },
    outgoingEdgeStencils: function(e) {
        if (!e.sourceShape && !e.sourceStencil) return [];
        e.sourceShape && (e.sourceStencil = e.sourceShape.getStencil());
        var t = [];
        return this._stencils.each(function(n) {
            if ("edge" === n.type()) {
                var i = Object.clone(e);
                i.edgeStencil = n, this.canConnect(i) && t.push(n);
            }
        }.bind(this)), t;
    },
    incomingEdgeStencils: function(e) {
        if (!e.targetShape && !e.targetStencil) return [];
        e.targetShape && (e.targetStencil = e.targetShape.getStencil());
        var t = [];
        return this._stencils.each(function(n) {
            if ("edge" === n.type()) {
                var i = Object.clone(e);
                i.edgeStencil = n, this.canConnect(i) && t.push(n);
            }
        }.bind(this)), t;
    },
    sourceStencils: function(e) {
        if (!e || !e.edgeShape && !e.edgeStencil) return [];
        e.targetShape && (e.targetStencil = e.targetShape.getStencil()), e.edgeShape && (e.edgeStencil = e.edgeShape.getStencil());
        var t = [];
        return this._stencils.each(function(n) {
            var i = Object.clone(e);
            i.sourceStencil = n, this.canConnect(i) && t.push(n);
        }.bind(this)), t;
    },
    targetStencils: function(e) {
        if (!e || !e.edgeShape && !e.edgeStencil) return [];
        e.sourceShape && (e.sourceStencil = e.sourceShape.getStencil()), e.edgeShape && (e.edgeStencil = e.edgeShape.getStencil());
        var t = [];
        return this._stencils.each(function(n) {
            var i = Object.clone(e);
            i.targetStencil = n, this.canConnect(i) && t.push(n);
        }.bind(this)), t;
    },
    canConnect: function(e) {
        if (!e || !e.sourceShape && !e.sourceStencil && !e.targetShape && !e.targetStencil || !e.edgeShape && !e.edgeStencil) return !1;
        e.sourceShape && (e.sourceStencil = e.sourceShape.getStencil()), e.targetShape && (e.targetStencil = e.targetShape.getStencil()), 
        e.edgeShape && (e.edgeStencil = e.edgeShape.getStencil());
        var t;
        if (e.sourceStencil && e.targetStencil) {
            var n = this._cachedConnectSET[e.sourceStencil.id()];
            if (n) {
                var i = n[e.edgeStencil.id()];
                if (i) {
                    var r = i[e.targetStencil.id()];
                    t = void 0 == r ? this._cacheConnect(e) : r;
                } else t = this._cacheConnect(e);
            } else t = this._cacheConnect(e);
        } else if (e.sourceStencil) {
            var n = this._cachedConnectSE[e.sourceStencil.id()];
            if (n) {
                var i = n[e.edgeStencil.id()];
                t = void 0 == i ? this._cacheConnect(e) : i;
            } else t = this._cacheConnect(e);
        } else {
            var r = this._cachedConnectTE[e.targetStencil.id()];
            if (r) {
                var i = r[e.edgeStencil.id()];
                t = void 0 == i ? this._cacheConnect(e) : i;
            } else t = this._cacheConnect(e);
        }
        if (t) {
            if (e.sourceShape) {
                var n = this._cachedCardSE[e.sourceStencil.id()];
                n || (this._cacheCard(e), n = this._cachedCardSE[e.sourceStencil.id()]);
                var s = n[e.edgeStencil.id()];
                void 0 == s && this._cacheCard(e), s = n[e.edgeStencil.id()], -1 != s && (t = e.sourceShape.getOutgoingShapes().all(function(t) {
                    return t.getStencil().id() !== e.edgeStencil.id() || (e.edgeShape ? t === e.edgeShape : 0) ? !0 : (s--, 
                    s > 0 ? !0 : !1);
                }));
            }
            if (e.targetShape) {
                var r = this._cachedCardTE[e.targetStencil.id()];
                r || (this._cacheCard(e), r = this._cachedCardTE[e.targetStencil.id()]);
                var s = r[e.edgeStencil.id()];
                void 0 == s && this._cacheCard(e), s = r[e.edgeStencil.id()], -1 != s && (t = e.targetShape.getIncomingShapes().all(function(t) {
                    return t.getStencil().id() !== e.edgeStencil.id() || (e.edgeShape ? t === e.edgeShape : 0) ? !0 : (s--, 
                    s > 0 ? !0 : !1);
                }));
            }
        }
        return t;
    },
    _canConnect: function(e) {
        if (!e || !e.sourceShape && !e.sourceStencil && !e.targetShape && !e.targetStencil || !e.edgeShape && !e.edgeStencil) return !1;
        e.sourceShape && (e.sourceStencil = e.sourceShape.getStencil()), e.targetShape && (e.targetStencil = e.targetShape.getStencil()), 
        e.edgeShape && (e.edgeStencil = e.edgeShape.getStencil());
        var t, n = this._getConnectionRulesOfEdgeStencil(e.edgeStencil);
        return t = 0 === n.keys().length ? !1 : e.sourceStencil ? e.sourceStencil.roles().any(function(t) {
            var i = n[t];
            return i ? e.targetStencil ? i.any(function(t) {
                return e.targetStencil.roles().member(t);
            }) : !0 : !1;
        }) : n.values().any(function(t) {
            return e.targetStencil.roles().any(function(e) {
                return t.member(e);
            });
        });
    },
    canContain: function(e) {
        if (!e || !e.containingStencil && !e.containingShape || !e.containedStencil && !e.containedShape) return !1;
        if (e.containedShape && (e.containedStencil = e.containedShape.getStencil()), e.containingShape && (e.containingStencil = e.containingShape.getStencil()), 
        "edge" == e.containedStencil.type()) return !1;
        var t, n = this._cachedContainPC[e.containingStencil.id()];
        if (n ? (t = n[e.containedStencil.id()], t || (t = this._cacheContain(e))) : t = this._cacheContain(e), 
        t[0]) {
            if (-1 == t[1]) return !0;
            if (e.containingShape) {
                var i = t[1];
                return e.containingShape.getChildShapes(!1).all(function(t) {
                    return t.getStencil().id() === e.containedStencil.id() ? (i--, i > 0 ? !0 : !1) : !0;
                });
            }
            return !0;
        }
        return !1;
    },
    _canContain: function(e) {
        if (!e || !e.containingStencil && !e.containingShape || !e.containedStencil && !e.containedShape) return !1;
        e.containedShape && (e.containedStencil = e.containedShape.getStencil()), e.containingShape && (e.containingStencil = e.containingShape.getStencil());
        var t;
        return t = e.containingStencil.roles().any(function(t) {
            var n = this._containmentRules[t];
            return n ? n.any(function(t) {
                return e.containedStencil.roles().member(t);
            }) : !1;
        }.bind(this));
    },
    morphStencils: function(e) {
        if (!e.stencil && !e.shape) return [];
        e.shape && (e.stencil = e.shape.getStencil());
        var t = [];
        return e.stencil.roles().each(function(e) {
            this._cacheMorph(e).each(function(e) {
                t.push(e);
            });
        }.bind(this)), t.uniq();
    },
    baseMorphs: function() {
        var e = [];
        return this._morphingRules.each(function(t) {
            t.value.each(function(t) {
                e.push(t);
            });
        }), e;
    },
    containsMorphingRules: function() {
        return this._stencilSets.any(function(e) {
            return !!e.jsonRules().morphingRules;
        });
    },
    connectMorph: function(e) {
        if (!e || !e.sourceShape && !e.sourceStencil && !e.targetShape && !e.targetStencil) return !1;
        e.sourceShape && (e.sourceStencil = e.sourceShape.getStencil()), e.targetShape && (e.targetStencil = e.targetShape.getStencil());
        var t = this.incomingEdgeStencils(e), n = this.outgoingEdgeStencils(e), i = t.select(function(e) {
            return n.member(e);
        }), r = this.baseMorphs().select(function(e) {
            return i.member(e);
        });
        return r.size() > 0 ? r[0] : i.size() > 0 ? i[0] : null;
    },
    showInShapeMenu: function(e) {
        return this._stencilSets.any(function(t) {
            return t.jsonRules().morphingRules.any(function(n) {
                return e.roles().include(t.namespace() + n.role) && n.showInShapeMenu !== !1;
            });
        });
    },
    preserveBounds: function(e) {
        return this._stencilSets.any(function(t) {
            return t.jsonRules().morphingRules.any(function(n) {
                return e.roles().include(t.namespace() + n.role) && n.preserveBounds;
            });
        });
    },
    getLayoutingRules: function(e, t) {
        if (e && e instanceof ORYX.Core.Shape) {
            var n = {
                "in": {},
                out: {}
            }, i = function(e, i) {
                e && e[i] && [ "t", "r", "b", "l" ].each(function(t) {
                    n[i][t] = Math.max(e[i][t], n[i][t] || 0);
                }), e && e[i + "s"] instanceof Array && [ "t", "r", "b", "l" ].each(function(r) {
                    var s, o = e[i + "s"].find(function(e) {
                        return !e.edgeRole;
                    });
                    t instanceof ORYX.Core.Edge && (s = e[i + "s"].find(function(e) {
                        return this._hasRole(t, e.edgeRole);
                    }.bind(this))), n[i][r] = Math.max(s ? s[r] : o[r], n[i][r] || 0);
                }.bind(this));
            }.bind(this);
            return e.getStencil().roles().each(function(e) {
                this._layoutRules[e] && (i(this._layoutRules[e], "in"), i(this._layoutRules[e], "out"));
            }.bind(this)), [ "in", "out" ].each(function(e) {
                [ "t", "r", "b", "l" ].each(function(t) {
                    n[e][t] = void 0 !== n[e][t] ? n[e][t] : 1;
                });
            }), n;
        }
    },
    _hasRole: function(e, t) {
        if (e instanceof ORYX.Core.Shape && t) {
            var n = e.getStencil().roles().any(function(e) {
                return e == t;
            });
            return n || e.getStencil().id() == e.getStencil().namespace() + t;
        }
    },
    _stencilsWithRole: function(e) {
        return this._stencils.findAll(function(t) {
            return t.roles().member(e) ? !0 : !1;
        });
    },
    _edgesWithRole: function(e) {
        return this._stencils.findAll(function(t) {
            return t.roles().member(e) && "edge" === t.type() ? !0 : !1;
        });
    },
    _nodesWithRole: function(e) {
        return this._stencils.findAll(function(t) {
            return t.roles().member(e) && "node" === t.type() ? !0 : !1;
        });
    },
    _getMaximumOccurrence: function(e, t) {
        var n;
        return t.roles().each(function(e) {
            var t = this._cardinalityRules[e];
            t && t.maximumOccurrence && (n = n ? Math.min(n, t.maximumOccurrence) : t.maximumOccurrence);
        }.bind(this)), n;
    },
    _getMaximumNumberOfOutgoingEdge: function(e) {
        if (!e || !e.sourceStencil || !e.edgeStencil) return !1;
        var t;
        return e.sourceStencil.roles().each(function(n) {
            var i = this._cardinalityRules[n];
            i && i.outgoingEdges && e.edgeStencil.roles().each(function(e) {
                var n = i.outgoingEdges[e];
                n && n.maximum && (t = t ? Math.min(t, n.maximum) : n.maximum);
            });
        }.bind(this)), t;
    },
    _getMaximumNumberOfIncomingEdge: function(e) {
        if (!e || !e.targetStencil || !e.edgeStencil) return !1;
        var t;
        return e.targetStencil.roles().each(function(n) {
            var i = this._cardinalityRules[n];
            i && i.incomingEdges && e.edgeStencil.roles().each(function(e) {
                var n = i.incomingEdges[e];
                n && n.maximum && (t = t ? Math.min(t, n.maximum) : n.maximum);
            });
        }.bind(this)), t;
    },
    _getConnectionRulesOfEdgeStencil: function(e) {
        var t = new Hash();
        return e.roles().each(function(e) {
            this._connectionRules[e] && this._connectionRules[e].each(function(e) {
                t[e.key] = t[e.key] ? t[e.key].concat(e.value) : e.value;
            });
        }.bind(this)), t;
    },
    _isRoleOfOtherNamespace: function(e) {
        return e.indexOf("#") > 0;
    },
    toString: function() {
        return "Rules";
    }
}, ORYX.Core.StencilSet.Rules = Clazz.extend(ORYX.Core.StencilSet.Rules), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet.StencilSet = Clazz.extend({
    construct: function(e) {
        if (arguments.callee.$.construct.apply(this, arguments), !e) throw "ORYX.Core.StencilSet.StencilSet(construct): Parameter 'source' is not defined.";
        e.endsWith("/") && (e = e.substr(0, e.length - 1)), this._extensions = new Hash(), 
        this._source = e, this._baseUrl = e, this._jsonObject = {}, this._stencils = new Hash(), 
        this._availableStencils = new Hash(), new Ajax.Request(e, {
            asynchronous: !1,
            method: "get",
            onSuccess: this._init.bind(this),
            onFailure: function() {
                throw "Loading stencil set " + e + " failed.";
            }.bind(e)
        });
    },
    findRootStencilName: function() {
        var e = this._stencils.values().find(function(e) {
            return e._jsonStencil.mayBeRoot;
        });
        return e || (ORYX.Log.warn("Did not find any stencil that may be root. Taking a guess."), 
        e = this._stencils.values()[0]), e.id();
    },
    equals: function(e) {
        return this.namespace() === e.namespace();
    },
    stencils: function(e, t, n) {
        if (e && t) {
            for (var i = this._availableStencils.values(), r = [ e ], s = [], o = []; r.size() > 0; ) {
                var a = r.pop();
                s.push(a);
                for (var c = i.findAll(function(e) {
                    var n = {
                        containingStencil: a,
                        containedStencil: e
                    };
                    return t.canContain(n);
                }), l = 0; l < c.size(); l++) s.member(c[l]) || r.push(c[l]);
                o = o.concat(c).uniq();
            }
            o = o.sortBy(function(e) {
                return i.indexOf(e);
            }), n && (o = o.sortBy(function(e) {
                return e.groups().first();
            }));
            var h = i.findAll(function(e) {
                return "edge" == e.type();
            });
            return o = o.concat(h);
        }
        return n ? this._availableStencils.values().sortBy(function(e) {
            return e.groups().first();
        }) : this._availableStencils.values();
    },
    nodes: function() {
        return this._availableStencils.values().findAll(function(e) {
            return "node" === e.type();
        });
    },
    edges: function() {
        return this._availableStencils.values().findAll(function(e) {
            return "edge" === e.type();
        });
    },
    stencil: function(e) {
        return this._stencils[e];
    },
    title: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonObject, "title");
    },
    description: function() {
        return ORYX.Core.StencilSet.getTranslation(this._jsonObject, "description");
    },
    namespace: function() {
        return this._jsonObject ? this._jsonObject.namespace : null;
    },
    jsonRules: function() {
        return this._jsonObject ? this._jsonObject.rules : null;
    },
    source: function() {
        return this._source;
    },
    extensions: function() {
        return this._extensions;
    },
    addExtension: function(e) {
        this.addExtensionDirectly(e);
    },
    addExtensionFromDefinition: function(e) {
        new Ajax.Request(e, {
            method: "GET",
            asynchronous: !1,
            onSuccess: function(e) {
                try {
                    var t = e.responseText, n = t.evalJSON();
                    this.addExtensionDirectly(n);
                } catch (i) {
                    ORYX.Log.debug("Unable to load extension definition: " + i), ORYX.EDITOR._pluginFacade.raiseEvent({
                        type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
                        ntype: "error",
                        msg: "Unable to load extension definition: " + i,
                        title: ""
                    });
                }
            }.bind(this),
            onFailure: function() {
                ORYX.EDITOR._pluginFacade.raiseEvent({
                    type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
                    ntype: "error",
                    msg: "Unable to create extension definition.",
                    title: ""
                });
            }.bind(this)
        });
    },
    addExtensionDirectly: function(e) {
        try {
            if (e["extends"].endsWith("#") || (e["extends"] += "#"), e["extends"] == this.namespace()) {
                this._extensions[e.namespace] = e;
                var t = this._stencils.keys().size();
                if (e.stencils && $A(e.stencils).each(function(e) {
                    t++;
                    var n = new ORYX.Core.StencilSet.Stencil(e, this.namespace(), this._baseUrl, this, void 0, t);
                    this._stencils[n.id()] = n, this._availableStencils[n.id()] = n;
                }.bind(this)), e.properties) {
                    var n = this._stencils.values();
                    n.each(function(t) {
                        var n = t.roles();
                        e.properties.each(function(i) {
                            i.roles.any(function(r) {
                                return r = e["extends"] + r, n.member(r) ? (i.properties.each(function(n) {
                                    t.addProperty(n, e.namespace);
                                }), !0) : !1;
                            });
                        });
                    }.bind(this));
                }
                e.removeproperties && e.removeproperties.each(function(t) {
                    var n = this.stencil(e["extends"] + t.stencil);
                    n && t.properties.each(function(e) {
                        n.removeProperty(e);
                    });
                }.bind(this)), e.removestencils && $A(e.removestencils).each(function(t) {
                    delete this._availableStencils[e["extends"] + t];
                }.bind(this));
            }
        } catch (i) {
            ORYX.Log.debug("StencilSet.addExtension: Something went wrong when initialising the stencil set extension. " + i);
        }
    },
    changeTitle: function(e) {
        this._jsonObject.title = e;
    },
    removeExtension: function(e) {
        var t = this._extensions[e];
        if (t) {
            if (t.stencils && $A(t.stencils).each(function(e) {
                var t = new ORYX.Core.StencilSet.Stencil(e, this.namespace(), this._baseUrl, this);
                delete this._stencils[t.id()], delete this._availableStencils[t.id()];
            }.bind(this)), t.properties) {
                var n = this._stencils.values();
                n.each(function(e) {
                    var n = e.roles();
                    t.properties.each(function(i) {
                        i.roles.any(function(r) {
                            return r = t["extends"] + r, n.member(r) ? (i.properties.each(function(t) {
                                e.removeProperty(t.id);
                            }), !0) : !1;
                        });
                    });
                }.bind(this));
            }
            t.removeproperties && t.removeproperties.each(function(e) {
                var n = this.stencil(t["extends"] + e.stencil);
                if (n) {
                    var i = $A(this._jsonObject.stencils).find(function(e) {
                        return e.id == n.id();
                    });
                    e.properties.each(function(e) {
                        var t = $A(i.properties).find(function(t) {
                            return t.id == e;
                        });
                        n.addProperty(t, this.namespace());
                    }.bind(this));
                }
            }.bind(this)), t.removestencils && $A(t.removestencils).each(function(e) {
                var n = t["extends"] + e;
                this._availableStencils[n] = this._stencils[n];
            }.bind(this));
        }
        delete this._extensions[e];
    },
    __handleStencilset: function(response) {
        try {
            eval("this._jsonObject =" + response.responseText);
        } catch (e) {
            throw "Stenciset corrupt: " + e;
        }
        if (!this._jsonObject) throw "Error evaluating stencilset. It may be corrupt.";
        with (this._jsonObject) {
            if (!namespace || "" === namespace) throw "Namespace definition missing in stencilset.";
            if (!(stencils instanceof Array)) throw "Stencilset corrupt.";
            namespace.endsWith("#") || (namespace += "#"), title || (title = ""), description || (description = "");
        }
    },
    _getJSONURL: function(e) {
        this._baseUrl = e.responseText.substring(0, e.responseText.lastIndexOf("/") + 1), 
        this._source = e.responseText, new Ajax.Request(e.responseText, {
            asynchronous: !1,
            method: "get",
            onSuccess: this._init.bind(this),
            onFailure: this._cancelInit.bind(this)
        });
    },
    _init: function(e) {
        this.__handleStencilset(e);
        var t = new Hash();
        this._jsonObject.propertyPackages && $A(this._jsonObject.propertyPackages).each(function(e) {
            t[e.name] = e.properties;
        }.bind(this));
        var n = 0;
        $A(this._jsonObject.stencils).each(function(e) {
            n++;
            try {
                var i = new ORYX.Core.StencilSet.Stencil(e, this.namespace(), this._baseUrl, this, t, n);
                this._stencils[i.id()] = i, this._availableStencils[i.id()] = i;
            } catch (r) {
                ORYX.Log.error("Problems instantiating a stencil:"), void 0 !== console && (console.log(r), 
                void 0 !== r.stack && console.log(r.stack));
            }
        }.bind(this));
    },
    toString: function() {
        return "StencilSet " + this.title() + " (" + this.namespace() + ")";
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.StencilSet || (ORYX.Core.StencilSet = {}), 
ORYX.Core.StencilSet._stencilSetsByNamespace = new Hash(), ORYX.Core.StencilSet._stencilSetsByUrl = new Hash(), 
ORYX.Core.StencilSet._StencilSetNSByEditorInstance = new Hash(), ORYX.Core.StencilSet._rulesByEditorInstance = new Hash(), 
ORYX.Core.StencilSet.stencilSets = function(e) {
    var t = ORYX.Core.StencilSet._StencilSetNSByEditorInstance[e], n = new Hash();
    return t && t.each(function(e) {
        var t = ORYX.Core.StencilSet.stencilSet(e);
        n[t.namespace()] = t;
    }), n;
}, ORYX.Core.StencilSet.stencilSet = function(e) {
    ORYX.Log.trace("Getting stencil set %0", e);
    var t = e.split("#", 1);
    return 1 === t.length ? (ORYX.Log.trace("Getting stencil set %0", t[0]), ORYX.Core.StencilSet._stencilSetsByNamespace[t[0] + "#"]) : void 0;
}, ORYX.Core.StencilSet.stencil = function(e) {
    ORYX.Log.trace("Getting stencil for %0", e);
    var t = ORYX.Core.StencilSet.stencilSet(e);
    return t ? t.stencil(e) : (ORYX.Log.trace("Cannot fild stencil for %0", e), void 0);
}, ORYX.Core.StencilSet.rules = function(e) {
    return ORYX.Core.StencilSet._rulesByEditorInstance[e] || (ORYX.Core.StencilSet._rulesByEditorInstance[e] = new ORYX.Core.StencilSet.Rules()), 
    ORYX.Core.StencilSet._rulesByEditorInstance[e];
}, ORYX.Core.StencilSet.loadStencilSet = function(e, t) {
    var n = ORYX.Core.StencilSet._stencilSetsByUrl[e];
    n || (n = new ORYX.Core.StencilSet.StencilSet(e), ORYX.Core.StencilSet._stencilSetsByNamespace[n.namespace()] = n, 
    ORYX.Core.StencilSet._stencilSetsByUrl[e] = n);
    var i = n.namespace();
    if (ORYX.Core.StencilSet._StencilSetNSByEditorInstance[t] ? ORYX.Core.StencilSet._StencilSetNSByEditorInstance[t].push(i) : ORYX.Core.StencilSet._StencilSetNSByEditorInstance[t] = [ i ], 
    ORYX.Core.StencilSet._rulesByEditorInstance[t]) ORYX.Core.StencilSet._rulesByEditorInstance[t].initializeRules(n); else {
        var r = new ORYX.Core.StencilSet.Rules();
        r.initializeRules(n), ORYX.Core.StencilSet._rulesByEditorInstance[t] = r;
    }
}, ORYX.Core.StencilSet.getTranslation = function(e, t) {
    var n = ORYX.I18N.Language.toLowerCase(), i = e[t + "_" + n];
    return i ? i : (i = e[t + "_" + n.substr(0, 2)], i ? i : e[t]);
}, !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Command = Clazz.extend({
    construct: function() {},
    execute: function() {
        throw "Command.execute() has to be implemented!";
    },
    rollback: function() {
        throw "Command.rollback() has to be implemented!";
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Bounds = {
    construct: function() {
        this._changedCallbacks = [], this.a = {}, this.b = {}, this.set.apply(this, arguments), 
        this.suspendChange = !1, this.changedWhileSuspend = !1;
    },
    _changed: function(e) {
        this.suspendChange ? this.changedWhileSuspend = !0 : (this._changedCallbacks.each(function(t) {
            t(this, e);
        }.bind(this)), this.changedWhileSuspend = !1);
    },
    registerCallback: function(e) {
        this._changedCallbacks.member(e) || this._changedCallbacks.push(e);
    },
    unregisterCallback: function(e) {
        this._changedCallbacks = this._changedCallbacks.without(e);
    },
    set: function() {
        var e = !1;
        switch (arguments.length) {
          case 1:
            this.a.x !== arguments[0].a.x && (e = !0, this.a.x = arguments[0].a.x), this.a.y !== arguments[0].a.y && (e = !0, 
            this.a.y = arguments[0].a.y), this.b.x !== arguments[0].b.x && (e = !0, this.b.x = arguments[0].b.x), 
            this.b.y !== arguments[0].b.y && (e = !0, this.b.y = arguments[0].b.y);
            break;

          case 2:
            var t = Math.min(arguments[0].x, arguments[1].x), n = Math.min(arguments[0].y, arguments[1].y), i = Math.max(arguments[0].x, arguments[1].x), r = Math.max(arguments[0].y, arguments[1].y);
            this.a.x !== t && (e = !0, this.a.x = t), this.a.y !== n && (e = !0, this.a.y = n), 
            this.b.x !== i && (e = !0, this.b.x = i), this.b.y !== r && (e = !0, this.b.y = r);
            break;

          case 4:
            var t = Math.min(arguments[0], arguments[2]), n = Math.min(arguments[1], arguments[3]), i = Math.max(arguments[0], arguments[2]), r = Math.max(arguments[1], arguments[3]);
            this.a.x !== t && (e = !0, this.a.x = t), this.a.y !== n && (e = !0, this.a.y = n), 
            this.b.x !== i && (e = !0, this.b.x = i), this.b.y !== r && (e = !0, this.b.y = r);
        }
        e && this._changed(!0);
    },
    moveTo: function() {
        var e = this.upperLeft();
        switch (arguments.length) {
          case 1:
            this.moveBy({
                x: arguments[0].x - e.x,
                y: arguments[0].y - e.y
            });
            break;

          case 2:
            this.moveBy({
                x: arguments[0] - e.x,
                y: arguments[1] - e.y
            });
        }
    },
    moveBy: function() {
        var e = !1;
        switch (arguments.length) {
          case 1:
            var t = arguments[0];
            (0 !== t.x || 0 !== t.y) && (e = !0, this.a.x += t.x, this.b.x += t.x, this.a.y += t.y, 
            this.b.y += t.y);
            break;

          case 2:
            var n = arguments[0], i = arguments[1];
            (0 !== n || 0 !== i) && (e = !0, this.a.x += n, this.b.x += n, this.a.y += i, this.b.y += i);
        }
        e && this._changed();
    },
    include: function(e) {
        if (void 0 === this.a.x && void 0 === this.a.y && void 0 === this.b.x && void 0 === this.b.y) return e;
        var t = Math.min(this.a.x, e.a.x), n = Math.min(this.a.y, e.a.y), i = Math.max(this.b.x, e.b.x), r = Math.max(this.b.y, e.b.y);
        this.set(t, n, i, r);
    },
    extend: function(e) {
        (0 !== e.x || 0 !== e.y) && (this.b.x += e.x, this.b.y += e.y, this._changed(!0));
    },
    widen: function(e) {
        0 !== e && (this.suspendChange = !0, this.moveBy({
            x: -e,
            y: -e
        }), this.extend({
            x: 2 * e,
            y: 2 * e
        }), this.suspendChange = !1, this.changedWhileSuspend && this._changed(!0));
    },
    upperLeft: function() {
        return {
            x: this.a.x,
            y: this.a.y
        };
    },
    lowerRight: function() {
        return {
            x: this.b.x,
            y: this.b.y
        };
    },
    width: function() {
        return this.b.x - this.a.x;
    },
    height: function() {
        return this.b.y - this.a.y;
    },
    center: function() {
        return {
            x: (this.a.x + this.b.x) / 2,
            y: (this.a.y + this.b.y) / 2
        };
    },
    midPoint: function() {
        return {
            x: (this.b.x - this.a.x) / 2,
            y: (this.b.y - this.a.y) / 2
        };
    },
    centerMoveTo: function() {
        var e = this.center();
        switch (arguments.length) {
          case 1:
            this.moveBy(arguments[0].x - e.x, arguments[0].y - e.y);
            break;

          case 2:
            this.moveBy(arguments[0] - e.x, arguments[1] - e.y);
        }
    },
    isIncluded: function(e, t) {
        var n, i, t;
        switch (arguments.length) {
          case 1:
            n = arguments[0].x, i = arguments[0].y, t = 0;
            break;

          case 2:
            arguments[0].x && arguments[0].y ? (n = arguments[0].x, i = arguments[0].y, t = Math.abs(arguments[1])) : (n = arguments[0], 
            i = arguments[1], t = 0);
            break;

          case 3:
            n = arguments[0], i = arguments[1], t = Math.abs(arguments[2]);
            break;

          default:
            throw "isIncluded needs one, two or three arguments";
        }
        var r = this.upperLeft(), s = this.lowerRight();
        return n >= r.x - t && n <= s.x + t && i >= r.y - t && i <= s.y + t ? !0 : !1;
    },
    clone: function() {
        return new ORYX.Core.Bounds(this);
    },
    toString: function() {
        return "( " + this.a.x + " | " + this.a.y + " )/( " + this.b.x + " | " + this.b.y + " )";
    },
    serializeForERDF: function() {
        return this.a.x + "," + this.a.y + "," + this.b.x + "," + this.b.y;
    }
}, ORYX.Core.Bounds = Clazz.extend(ORYX.Core.Bounds), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.UIObject = {
    construct: function(e) {
        this.isChanged = !0, this.isResized = !0, this.isVisible = !0, this.isSelectable = !1, 
        this.isResizable = !1, this.isMovable = !1, this.id = ORYX.Editor.provideId(), this.parent = void 0, 
        this.node = void 0, this.children = [], this.bounds = new ORYX.Core.Bounds(), this._changedCallback = this._changed.bind(this), 
        this.bounds.registerCallback(this._changedCallback), e && e.eventHandlerCallback && (this.eventHandlerCallback = e.eventHandlerCallback);
    },
    _changed: function(e, t) {
        this.isChanged = !0, this.bounds == e && (this.isResized = t || this.isResized);
    },
    update: function() {
        this.isChanged && (this.refresh(), this.isChanged = !1, this.children.each(function(e) {
            e.update();
        }));
    },
    refresh: function() {},
    getChildren: function() {
        return this.children.clone();
    },
    getParents: function() {
        for (var e = [], t = this.parent; t; ) e.push(t), t = t.parent;
        return e;
    },
    isParent: function(e) {
        for (var t = this; t; ) {
            if (t === e) return !0;
            t = t.parent;
        }
        return !1;
    },
    getId: function() {
        return this.id;
    },
    getChildById: function(e, t) {
        return this.children.find(function(n) {
            if (n.getId() === e) return n;
            if (t) {
                var i = n.getChildById(e, t);
                if (i) return i;
            }
        });
    },
    add: function(e) {
        this.children.member(e) ? ORYX.Log.info("add: ORYX.Core.UIObject is already a child of this object.") : (e.parent && e.remove(e), 
        this.children.push(e), e.parent = this, e.node = this.node.appendChild(e.node), 
        e.bounds.registerCallback(this._changedCallback), this.eventHandlerCallback && this.eventHandlerCallback({
            type: ORYX.CONFIG.EVENT_SHAPEADDED,
            shape: e
        }));
    },
    remove: function(e) {
        this.children.member(e) ? (this.children = this._uiObjects.without(e), e.parent = void 0, 
        e.node = this.node.removeChild(e.node), e.bounds.unregisterCallback(this._changedCallback)) : ORYX.Log.info("remove: ORYX.Core.UIObject is not a child of this object.");
    },
    absoluteBounds: function() {
        if (this.parent) {
            var e = this.absoluteXY();
            return new ORYX.Core.Bounds(e.x, e.y, e.x + this.bounds.width(), e.y + this.bounds.height());
        }
        return this.bounds.clone();
    },
    absoluteXY: function() {
        if (this.parent) {
            var e = this.parent.absoluteXY();
            return {
                x: e.x + this.bounds.upperLeft().x,
                y: e.y + this.bounds.upperLeft().y
            };
        }
        return {
            x: this.bounds.upperLeft().x,
            y: this.bounds.upperLeft().y
        };
    },
    absoluteCenterXY: function() {
        if (this.parent) {
            var e = this.parent.absoluteXY();
            return {
                x: e.x + this.bounds.center().x,
                y: e.y + this.bounds.center().y
            };
        }
        return {
            x: this.bounds.center().x,
            y: this.bounds.center().y
        };
    },
    hide: function() {
        this.node.setAttributeNS(null, "display", "none"), this.isVisible = !1, this.children.each(function(e) {
            e.hide();
        });
    },
    show: function() {
        this.node.setAttributeNS(null, "display", "inherit"), this.isVisible = !0, this.children.each(function(e) {
            e.show();
        });
    },
    addEventHandlers: function(e) {
        e.addEventListener(ORYX.CONFIG.EVENT_MOUSEDOWN, this._delegateEvent.bind(this), !1), 
        e.addEventListener(ORYX.CONFIG.EVENT_MOUSEMOVE, this._delegateEvent.bind(this), !1), 
        e.addEventListener(ORYX.CONFIG.EVENT_MOUSEUP, this._delegateEvent.bind(this), !1), 
        e.addEventListener(ORYX.CONFIG.EVENT_MOUSEOVER, this._delegateEvent.bind(this), !1), 
        e.addEventListener(ORYX.CONFIG.EVENT_MOUSEOUT, this._delegateEvent.bind(this), !1), 
        e.addEventListener("click", this._delegateEvent.bind(this), !1), e.addEventListener(ORYX.CONFIG.EVENT_DBLCLICK, this._delegateEvent.bind(this), !1);
    },
    _delegateEvent: function(e) {
        this.eventHandlerCallback && this.eventHandlerCallback(e, this);
    },
    toString: function() {
        return "UIObject " + this.id;
    }
}, ORYX.Core.UIObject = Clazz.extend(ORYX.Core.UIObject), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.AbstractShape = ORYX.Core.UIObject.extend({
    construct: function(e, t) {
        arguments.callee.$.construct.apply(this, arguments), this.resourceId = ORYX.Editor.provideId(), 
        this._stencil = t, this._stencil._jsonStencil.superId && (stencilId = this._stencil.id(), 
        superStencilId = stencilId.substring(0, stencilId.indexOf("#") + 1) + t._jsonStencil.superId, 
        stencilSet = this._stencil.stencilSet(), this._stencil = stencilSet.stencil(superStencilId)), 
        this.properties = new Hash(), this.propertiesChanged = new Hash(), this.hiddenProperties = new Hash(), 
        this._stencil.properties().each(function(e) {
            var t = e.prefix() + "-" + e.id();
            this.properties[t] = e.value(), this.propertiesChanged[t] = !0;
        }.bind(this)), t._jsonStencil.superId && t.properties().each(function(e) {
            var t = e.prefix() + "-" + e.id(), n = e.value(), i = this.properties[t];
            this.properties[t] = n, this.propertiesChanged[t] = !0, this._delegateEvent({
                type: ORYX.CONFIG.EVENT_PROPERTY_CHANGED,
                name: t,
                value: n,
                oldValue: i
            });
        }.bind(this));
    },
    layout: function() {},
    getStencil: function() {
        return this._stencil;
    },
    getChildShapeByResourceId: function(e) {
        return e = ERDF.__stripHashes(e), this.getChildShapes(!0).find(function(t) {
            return t.resourceId == e;
        });
    },
    getChildShapes: function(e, t) {
        var n = [];
        return this.children.each(function(i) {
            i instanceof ORYX.Core.Shape && i.isVisible && (t && t(i), n.push(i), e && (n = n.concat(i.getChildShapes(e, t))));
        }), n;
    },
    hasChildShape: function(e) {
        return this.getChildShapes().any(function(t) {
            return t === e || t.hasChildShape(e);
        });
    },
    getChildNodes: function(e, t) {
        var n = [];
        return this.children.each(function(i) {
            i instanceof ORYX.Core.Node && i.isVisible && (t && t(i), n.push(i)), i instanceof ORYX.Core.Shape && e && (n = n.concat(i.getChildNodes(e, t)));
        }), n;
    },
    getChildEdges: function(e, t) {
        var n = [];
        return this.children.each(function(i) {
            i instanceof ORYX.Core.Edge && i.isVisible && (t && t(i), n.push(i)), i instanceof ORYX.Core.Shape && e && (n = n.concat(i.getChildEdges(e, t)));
        }), n;
    },
    getAbstractShapesAtPosition: function() {
        var e, t;
        switch (arguments.length) {
          case 1:
            e = arguments[0].x, t = arguments[0].y;
            break;

          case 2:
            e = arguments[0], t = arguments[1];
            break;

          default:
            throw "getAbstractShapesAtPosition needs 1 or 2 arguments!";
        }
        if (this.isPointIncluded(e, t)) {
            var n = [];
            n.push(this);
            var i = this.getChildNodes(), r = this.getChildEdges();
            return [ i, r ].each(function(i) {
                var r = new Hash();
                i.each(function(n) {
                    if (n.isVisible) {
                        var i = n.getAbstractShapesAtPosition(e, t);
                        if (i.length > 0) {
                            var s = $A(n.node.parentNode.childNodes), o = s.indexOf(n.node);
                            r[o] = i;
                        }
                    }
                }), r.keys().sort().each(function(e) {
                    n = n.concat(r[e]);
                });
            }), n;
        }
        return [];
    },
    setProperty: function(e, t, n) {
        var i = this.properties[e];
        (i !== t || n === !0) && (this.properties[e] = t, this.propertiesChanged[e] = !0, 
        this._changed(), this._isInSetProperty || (this._isInSetProperty = !0, this._delegateEvent({
            type: ORYX.CONFIG.EVENT_PROPERTY_CHANGED,
            elements: [ this ],
            name: e,
            value: t,
            oldValue: i
        }), delete this._isInSetProperty));
    },
    setHiddenProperty: function(e, t) {
        if (void 0 === t) return delete this.hiddenProperties[e], void 0;
        var n = this.hiddenProperties[e];
        n !== t && (this.hiddenProperties[e] = t);
    },
    isPointIncluded: function(e, t, n) {
        var i = n ? n : this.absoluteBounds();
        return i.isIncluded(e, t);
    },
    serialize: function() {
        var e = [];
        return e.push({
            name: "type",
            prefix: "oryx",
            value: this.getStencil().id(),
            type: "literal"
        }), this.hiddenProperties.each(function(t) {
            e.push({
                name: t.key.replace("oryx-", ""),
                prefix: "oryx",
                value: t.value,
                type: "literal"
            });
        }.bind(this)), this.getStencil().properties().each(function(t) {
            var n = t.prefix(), i = t.id();
            e.push({
                name: i,
                prefix: n,
                value: this.properties[n + "-" + i],
                type: "literal"
            });
        }.bind(this)), e;
    },
    deserialize: function(e) {
        e = e.sort(function(e, t) {
            return Number(this.properties.keys().member(e.prefix + "-" + e.name)) > Number(this.properties.keys().member(t.prefix + "-" + t.name)) ? -1 : 0;
        }.bind(this)), e.each(function(e) {
            var t = e.name, n = e.prefix, i = e.value;
            switch ("object" === Ext.type(i) && (i = Ext.encode(i)), n + "-" + t) {
              case "raziel-parent":
                if (!this.parent) break;
                var r = this.getCanvas().getChildShapeByResourceId(i);
                r && r.add(this);
                break;

              default:
                this.properties.keys().member(n + "-" + t) ? this.setProperty(n + "-" + t, i) : "bounds" !== t && "parent" !== t && "target" !== t && "dockers" !== t && "docker" !== t && "outgoing" !== t && "incoming" !== t && this.setHiddenProperty(n + "-" + t, i);
            }
        }.bind(this));
    },
    toString: function() {
        return "ORYX.Core.AbstractShape " + this.id;
    },
    toJSON: function() {
        var e = {
            resourceId: this.resourceId,
            properties: Ext.apply({}, this.properties, this.hiddenProperties).inject({}, function(e, t) {
                var n = t[0], i = t[1];
                if (this.getStencil().property(n) && this.getStencil().property(n).type() === ORYX.CONFIG.TYPE_COMPLEX && "string" === Ext.type(i)) try {
                    i = Ext.decode(i);
                } catch (r) {}
                return n = n.replace(/^[\w_]+-/, ""), e[n] = i, e;
            }.bind(this)),
            stencil: {
                id: this.getStencil().idWithoutNs()
            },
            childShapes: this.getChildShapes().map(function(e) {
                return e.toJSON();
            })
        };
        return this.getOutgoingShapes && (e.outgoing = this.getOutgoingShapes().map(function(e) {
            return {
                resourceId: e.resourceId
            };
        })), this.bounds && (e.bounds = {
            lowerRight: this.bounds.lowerRight(),
            upperLeft: this.bounds.upperLeft()
        }), this.dockers && (e.dockers = this.dockers.map(function(e) {
            var t = e.getDockedShape() && e.referencePoint ? e.referencePoint : e.bounds.center();
            return t.getDocker = function() {
                return e;
            }, t;
        })), Ext.apply(e, ORYX.Core.AbstractShape.JSONHelper), e.getShape = function() {
            return this;
        }.bind(this), e;
    }
}), ORYX.Core.AbstractShape.JSONHelper = {
    eachChild: function(e, t, n) {
        if (this.childShapes) {
            var i = [];
            this.childShapes.each(function(r) {
                var s = e(r, this);
                s && i.push(s), t && r.eachChild(e, t, n);
            }.bind(this)), n && (this.childShapes = i);
        }
    },
    getChildShapes: function(e) {
        var t = this.childShapes;
        return e && this.eachChild(function(n) {
            t = t.concat(n.getChildShapes(e));
        }, !0), t;
    },
    serialize: function() {
        return Ext.encode(this);
    }
}, !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Canvas = ORYX.Core.AbstractShape.extend({
    zoomLevel: 1,
    construct: function(e) {
        return arguments.callee.$.construct.apply(this, arguments), e && e.width && e.height ? (this.resourceId = e.id, 
        this.nodes = [], this.edges = [], this.rootNode = ORYX.Editor.graft("http://www.w3.org/2000/svg", e.parentNode, [ "svg", {
            id: this.id,
            width: e.width,
            height: e.height
        }, [ "defs", {} ] ]), this.rootNode.setAttribute("xmlns:xlink", "http://www.w3.org/1999/xlink"), 
        this.rootNode.setAttribute("xmlns:svg", "http://www.w3.org/2000/svg"), this._htmlContainer = ORYX.Editor.graft("http://www.w3.org/1999/xhtml", e.parentNode, [ "div", {
            id: "oryx_canvas_htmlContainer",
            style: "position:absolute; top:5px"
        } ]), this.node = ORYX.Editor.graft("http://www.w3.org/2000/svg", this.rootNode, [ "g", {}, [ "g", {
            "class": "stencils"
        }, [ "g", {
            "class": "me"
        } ], [ "g", {
            "class": "children"
        } ], [ "g", {
            "class": "edge"
        } ] ], [ "g", {
            "class": "svgcontainer"
        } ] ]), this.node.setAttributeNS(null, "stroke", "none"), this.node.setAttributeNS(null, "font-family", "Verdana, sans-serif"), 
        this.node.setAttributeNS(null, "font-size-adjust", "none"), this.node.setAttributeNS(null, "font-style", "normal"), 
        this.node.setAttributeNS(null, "font-variant", "normal"), this.node.setAttributeNS(null, "font-weight", "normal"), 
        this.node.setAttributeNS(null, "line-heigth", "normal"), this.node.setAttributeNS(null, "font-size", ORYX.CONFIG.LABEL_DEFAULT_LINE_HEIGHT), 
        this.bounds.set(0, 0, e.width, e.height), this.addEventHandlers(this.rootNode.parentNode), 
        this.rootNode.oncontextmenu = function() {
            return !1;
        }, void 0) : (ORYX.Log.fatal("Canvas is missing mandatory parameters options.width and options.height."), 
        void 0);
    },
    focus: function() {
        this.headerA || (this.headerA = Ext.get("oryx_editor_header").child("a").dom), this.headerA.focus(), 
        this.headerA.blur();
    },
    update: function() {
        this.nodes.each(function(e) {
            this._traverseForUpdate(e);
        }.bind(this));
        var e = this.getStencil().layout();
        e && e.each(function(e) {
            e.shape = this, e.forceExecution = !0, e.target = this.rootNode, this._delegateEvent(e);
        }.bind(this)), this.nodes.invoke("_update"), this.edges.invoke("_update", !0);
    },
    _traverseForUpdate: function(e) {
        var t = e.isChanged;
        return e.getChildNodes(!1, function(e) {
            this._traverseForUpdate(e) && (t = !0);
        }.bind(this)), t ? (e.layout(), !0) : !1;
    },
    layout: function() {},
    getChildNodes: function(e, t) {
        if (e || t) {
            var n = [];
            return this.nodes.each(function(i) {
                t && t(i), n.push(i), e && i instanceof ORYX.Core.Shape && (n = n.concat(i.getChildNodes(e, t)));
            }), n;
        }
        return this.nodes.clone();
    },
    add: function(e) {
        e instanceof ORYX.Core.UIObject ? this.children.member(e) ? ORYX.Log.warn("add: ORYX.Core.UIObject is already a child of this object.") : (e.parent && e.parent.remove(e), 
        this.children.push(e), e.parent = this, e instanceof ORYX.Core.Shape ? e instanceof ORYX.Core.Edge ? (e.addMarkers(this.rootNode.getElementsByTagNameNS(NAMESPACE_SVG, "defs")[0]), 
        e.node = this.node.childNodes[0].childNodes[2].appendChild(e.node), this.edges.push(e)) : (e.node = this.node.childNodes[0].childNodes[1].appendChild(e.node), 
        this.nodes.push(e)) : e.node = this.node.appendChild(e.node), e.bounds.registerCallback(this._changedCallback), 
        this.eventHandlerCallback && this.eventHandlerCallback({
            type: ORYX.CONFIG.EVENT_SHAPEADDED,
            shape: e
        })) : ORYX.Log.fatal("add: Parameter is not of type ORYX.Core.UIObject.");
    },
    remove: function(e) {
        this.children.member(e) ? (this.children = this.children.without(e), e.parent = void 0, 
        e instanceof ORYX.Core.Shape ? e instanceof ORYX.Core.Edge ? (e.removeMarkers(), 
        e.node = this.node.childNodes[0].childNodes[2].removeChild(e.node), this.edges = this.edges.without(e)) : (e.node = this.node.childNodes[0].childNodes[1].removeChild(e.node), 
        this.nodes = this.nodes.without(e)) : e.node = this.node.removeChild(e.node), e.bounds.unregisterCallback(this._changedCallback)) : ORYX.Log.warn("remove: ORYX.Core.UIObject is not a child of this object.");
    },
    addShapeObjects: function(e, t) {
        if (e) {
            var n = function(e, n) {
                try {
                    var i = ORYX.Core.StencilSet.stencil(this.getStencil().namespace() + e.stencil.id), r = "node" == i.type() ? ORYX.Core.Node : ORYX.Core.Edge, s = new r({
                        eventHandlerCallback: t
                    }, i);
                    return s.resourceId = e.resourceId, e.parent = "#" + (e.parent && e.parent.resourceId || n.resourceId), 
                    this.add(s), {
                        json: e,
                        object: s
                    };
                } catch (o) {
                    ORYX.Log.warn("LoadingContent: Stencil could not create.");
                }
            }.bind(this), i = function(e) {
                var t = [];
                return e.childShapes && e.childShapes.each(function(r) {
                    var s = n(r, e);
                    "undefined" != typeof s && t.push(s), t = t.concat(i(r));
                }), t;
            }.bind(this), r = i({
                childShapes: e,
                resourceId: this.resourceId
            });
            return r.each(function(e) {
                var t = [];
                for (field in e.json.properties) t.push({
                    prefix: "oryx",
                    name: field,
                    value: e.json.properties[field]
                });
                if (e.json.outgoing.each(function(e) {
                    t.push({
                        prefix: "raziel",
                        name: "outgoing",
                        value: "#" + e.resourceId
                    });
                }), e.object instanceof ORYX.Core.Edge) {
                    var n = e.json.target || e.json.outgoing[0];
                    n && t.push({
                        prefix: "raziel",
                        name: "target",
                        value: "#" + n.resourceId
                    });
                }
                e.json.bounds && t.push({
                    prefix: "oryx",
                    name: "bounds",
                    value: e.json.bounds.upperLeft.x + "," + e.json.bounds.upperLeft.y + "," + e.json.bounds.lowerRight.x + "," + e.json.bounds.lowerRight.y
                }), e.json.dockers && t.push({
                    prefix: "oryx",
                    name: "dockers",
                    value: e.json.dockers.inject("", function(e, t) {
                        return e + t.x + " " + t.y + " ";
                    }) + " #"
                }), t.push({
                    prefix: "raziel",
                    name: "parent",
                    value: e.json.parent
                }), e.__properties = t;
            }.bind(this)), r.each(function(e) {
                e.object instanceof ORYX.Core.Node && e.object.deserialize(e.__properties);
            }), r.each(function(e) {
                e.object instanceof ORYX.Core.Edge && e.object.deserialize(e.__properties);
            }), r.pluck("object");
        }
    },
    absoluteBounds: function() {
        return new ORYX.Core.Bounds(0, 0, this.getHTMLContainer().parentNode.offsetWidth, this.getHTMLContainer().parentNode.offsetHeight);
    },
    updateSize: function() {
        var e = 0, t = 0, n = 100;
        this.getChildShapes(!0, function(i) {
            var r = i.bounds;
            e = Math.max(e, r.lowerRight().x + n), t = Math.max(t, r.lowerRight().y + n);
        }), (this.bounds.width() < e || this.bounds.height() < t) && this.setSize({
            width: Math.max(this.bounds.width(), e),
            height: Math.max(this.bounds.height(), t)
        });
    },
    getRootNode: function() {
        return this.rootNode;
    },
    getSvgContainer: function() {
        return this.node.childNodes[1];
    },
    getHTMLContainer: function() {
        return this._htmlContainer;
    },
    getShapesWithSharedParent: function(e) {
        return !e || e.length < 1 ? [] : 1 == e.length ? e : e.findAll(function(t) {
            for (var n = t.parent; n; ) {
                if (e.member(n)) return !1;
                n = n.parent;
            }
            return !0;
        });
    },
    setSize: function(e, t) {
        e && e.width && e.height && (this.rootNode.parentNode && (this.rootNode.parentNode.style.width = e.width + "px", 
        this.rootNode.parentNode.style.height = e.height + "px"), this.rootNode.setAttributeNS(null, "width", e.width), 
        this.rootNode.setAttributeNS(null, "height", e.height), t || this.bounds.set({
            a: {
                x: 0,
                y: 0
            },
            b: {
                x: e.width / this.zoomLevel,
                y: e.height / this.zoomLevel
            }
        }));
    },
    getSVGRepresentation: function(e) {
        var t = this.getRootNode().cloneNode(!0);
        this._removeInvisibleElements(t);
        var n, i, r, s;
        try {
            var o = this.getRootNode().childNodes[1].getBBox();
            n = o.x, i = o.y, r = o.x + o.width, s = o.y + o.height;
        } catch (a) {
            this.getChildShapes(!0).each(function(e) {
                var t = e.absoluteBounds(), o = t.upperLeft(), a = t.lowerRight();
                void 0 == n ? (n = o.x, i = o.y, r = a.x, s = a.y) : (n = Math.min(n, o.x), i = Math.min(i, o.y), 
                r = Math.max(r, a.x), s = Math.max(s, a.y));
            });
        }
        var c, l, h, u, d = 50;
        void 0 == n ? (c = 0, l = 0, h = 0, u = 0) : (c = r - n, l = s - i, h = -n + d / 2, 
        u = -i + d / 2), t.setAttributeNS(null, "width", c + d), t.setAttributeNS(null, "height", l + d), 
        t.childNodes[1].firstChild.setAttributeNS(null, "transform", "translate(" + h + ", " + u + ")"), 
        t.childNodes[1].removeAttributeNS(null, "transform");
        try {
            var p = t.childNodes[1].childNodes[1];
            p.parentNode.removeChild(p);
        } catch (a) {}
        return e && ($A(t.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "tspan")).each(function(e) {
            e.textContent = e.textContent.escapeHTML();
        }), $A(t.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "text")).each(function(e) {
            0 == e.childNodes.length && (e.textContent = e.textContent.escapeHTML());
        })), $A(t.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "image")).each(function(e) {
            var t = e.getAttributeNS("http://www.w3.org/1999/xlink", "href");
            t.match("^(http|https)://") || -1 != t.indexOf("base64") || (t = window.location.protocol + "//" + window.location.host + t, 
            e.setAttributeNS("http://www.w3.org/1999/xlink", "href", t));
        }), $A(t.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "a")).each(function(e) {
            e.setAttributeNS("http://www.w3.org/1999/xlink", "xlink:href", (e.getAttributeNS("http://www.w3.org/1999/xlink", "href") || "").escapeHTML());
        }), t;
    },
    _removeInvisibleElements: function(e) {
        for (var t = 0; t < e.childNodes.length; ) {
            var n = e.childNodes[t];
            n.getAttributeNS && "hidden" === n.getAttributeNS(null, "visibility") ? e.removeChild(n) : (this._removeInvisibleElements(n), 
            t++);
        }
    },
    _delegateEvent: function(e) {
        !this.eventHandlerCallback || e.target != this.rootNode && e.target != this.rootNode.parentNode || this.eventHandlerCallback(e, this);
    },
    toString: function() {
        return "Canvas " + this.id;
    },
    toJSON: function() {
        var e = arguments.callee.$.toJSON.apply(this, arguments);
        return e.stencilset = {
            url: this.getStencil().stencilSet().source(),
            namespace: this.getStencil().stencilSet().namespace()
        }, e;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.UIEnableDrag = function(e, t, n) {
    this.uiObj = t;
    var i = t.bounds.upperLeft(), r = t.node.getScreenCTM();
    this.faktorXY = {
        x: r.a,
        y: r.d
    }, this.scrollNode = t.node.ownerSVGElement.parentNode.parentNode, this.offSetPosition = {
        x: Event.pointerX(e) - i.x * this.faktorXY.x,
        y: Event.pointerY(e) - i.y * this.faktorXY.y
    }, this.offsetScroll = {
        x: this.scrollNode.scrollLeft,
        y: this.scrollNode.scrollTop
    }, this.dragCallback = ORYX.Core.UIDragCallback.bind(this), this.disableCallback = ORYX.Core.UIDisableDrag.bind(this), 
    this.movedCallback = n ? n.movedCallback : void 0, this.upCallback = n ? n.upCallback : void 0, 
    document.documentElement.addEventListener(ORYX.CONFIG.EVENT_MOUSEUP, this.disableCallback, !0), 
    document.documentElement.addEventListener(ORYX.CONFIG.EVENT_MOUSEMOVE, this.dragCallback, !1);
}, ORYX.Core.UIDragCallback = function(e) {
    var t = {
        x: Event.pointerX(e) - this.offSetPosition.x,
        y: Event.pointerY(e) - this.offSetPosition.y
    };
    t.x -= this.offsetScroll.x - this.scrollNode.scrollLeft, t.y -= this.offsetScroll.y - this.scrollNode.scrollTop, 
    t.x /= this.faktorXY.x, t.y /= this.faktorXY.y, this.uiObj.bounds.moveTo(t), this.movedCallback && this.movedCallback(e), 
    Event.stop(e);
}, ORYX.Core.UIDisableDrag = function(e) {
    document.documentElement.removeEventListener(ORYX.CONFIG.EVENT_MOUSEMOVE, this.dragCallback, !1), 
    document.documentElement.removeEventListener(ORYX.CONFIG.EVENT_MOUSEUP, this.disableCallback, !0), 
    this.upCallback && this.upCallback(e), this.upCallback = void 0, this.movedCallback = void 0, 
    Event.stop(e);
}, !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Shape = {
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.dockers = [], this.magnets = [], 
        this._defaultMagnet, this.incoming = [], this.outgoing = [], this.nodes = [], this._dockerChangedCallback = this._dockerChanged.bind(this), 
        this._labels = new Hash(), this.node = ORYX.Editor.graft("http://www.w3.org/2000/svg", null, [ "g", {
            id: this.id
        }, [ "g", {
            "class": "stencils"
        }, [ "g", {
            "class": "me"
        } ], [ "g", {
            "class": "children",
            style: "overflow:hidden"
        } ], [ "g", {
            "class": "edge"
        } ] ], [ "g", {
            "class": "controls"
        }, [ "g", {
            "class": "dockers"
        } ], [ "g", {
            "class": "magnets"
        } ] ] ]);
    },
    update: function() {},
    _update: function() {},
    refresh: function() {
        arguments.callee.$.refresh.apply(this, arguments), this.node.ownerDocument && (this.propertiesChanged.each(function(e) {
            if (e.value) {
                var t = this.properties[e.key], n = this.getStencil().property(e.key);
                if (this.propertiesChanged[e.key] = !1, n.type() == ORYX.CONFIG.TYPE_CHOICE || n.type() == ORYX.CONFIG.TYPE_DYNAMICCHOICE) {
                    n.refToView().each(function(e) {
                        if ("" !== e) {
                            var i = this._labels[this.id + e];
                            i && ("fontsize" == n.id() ? t && n.item(t) && i.fontSize(n.item(t).value()) : i.text(n.item(t).value()));
                        }
                    }.bind(this));
                    var i = new Hash();
                    n.items().each(function(n) {
                        n.refToView().each(function(r) {
                            if ("" == r) return this.propertiesChanged[e.key] = !0, void 0;
                            var s = this.node.ownerDocument.getElementById(this.id + r);
                            return s ? (i[s.id] && t != n.value() || (s.setAttributeNS(null, "display", t == n.value() ? "inherit" : "none"), 
                            i[s.id] = s), ORYX.Editor.checkClassType(s, SVGImageElement) && s.setAttributeNS("http://www.w3.org/1999/xlink", "href", s.getAttributeNS("http://www.w3.org/1999/xlink", "href")), 
                            void 0) : (this.propertiesChanged[e.key] = !0, void 0);
                        }.bind(this));
                    }.bind(this));
                } else n.refToView().each(function(i) {
                    if ("" === i) return this.propertiesChanged[e.key] = !0, void 0;
                    var r = this.id + i, s = this.node.ownerDocument.getElementById(r);
                    if (!s || !s.ownerSVGElement) {
                        if (n.type() !== ORYX.CONFIG.TYPE_URL && n.type() !== ORYX.CONFIG.TYPE_DIAGRAM_LINK) return this.propertiesChanged[e.key] = !0, 
                        void 0;
                        var o = this.node.ownerDocument.getElementsByTagNameNS("http://www.w3.org/2000/svg", "a");
                        if (s = $A(o).find(function(e) {
                            return e.getAttributeNS(null, "id") === r;
                        }), !s) return this.propertiesChanged[e.key] = !0, void 0;
                    }
                    if (n.complexAttributeToView()) {
                        var a = this._labels[r];
                        if (a) try {
                            propJson = t.evalJSON();
                            var c = propJson[n.complexAttributeToView()];
                            a.text(c ? c : t);
                        } catch (l) {
                            a.text(t);
                        }
                    } else switch (n.type()) {
                      case ORYX.CONFIG.TYPE_BOOLEAN:
                        "string" == typeof t && (t = "true" === t), s.setAttributeNS(null, "display", t !== n.inverseBoolean() ? "inherit" : "none");
                        break;

                      case ORYX.CONFIG.TYPE_COLOR:
                        n.fill() && ("stop" === s.tagName.toLowerCase() ? (s.setAttributeNS(null, "stop-color", t), 
                        "radialgradient" === s.parentNode.tagName.toLowerCase() && ORYX.Utils.adjustGradient(s.parentNode, s)) : s.setAttributeNS(null, "fill", t)), 
                        n.stroke() && (s.setAttributeNS(null, "stroke", t), "stop" === s.tagName.toLowerCase() && (s.setAttributeNS(null, "stop-color", t), 
                        "radialgradient" === s.parentNode.tagName.toLowerCase() && ORYX.Utils.adjustGradient(s.parentNode, s)));
                        break;

                      case ORYX.CONFIG.TYPE_STRING:
                        var a = this._labels[r];
                        a && a.text(t);
                        break;

                      case ORYX.CONFIG.TYPE_INTEGER:
                        var a = this._labels[r];
                        a && a.text(t);
                        break;

                      case ORYX.CONFIG.TYPE_FLOAT:
                        if (n.fillOpacity() && s.setAttributeNS(null, "fill-opacity", t), n.strokeOpacity() && s.setAttributeNS(null, "stroke-opacity", t), 
                        !n.fillOpacity() && !n.strokeOpacity()) {
                            var a = this._labels[r];
                            a && a.text(t);
                        }
                        break;

                      case ORYX.CONFIG.TYPE_URL:
                      case ORYX.CONFIG.TYPE_CALLEDELEMENT:
                        var h = s.getAttributeNodeNS("", "onclick");
                        if (h) if (t && t.length > 0) {
                            var u = ORYX.EDITOR.getSerializedJSON(), d = jsonPath(u.evalJSON(), "$.properties.package");
                            Ext.Ajax.request({
                                url: ORYX.PATH + "calledelement",
                                method: "POST",
                                success: function(e) {
                                    try {
                                        h.textContent = e.responseText.length > 0 && "false" != e.responseText ? "new ImageViewer({title: 'Process Image', width: '650', height: '450', autoScroll: true, fixedcenter: true, src: '" + e.responseText + "',hideAction: 'close'}).show();" : "Ext.Msg.alert('Unable to show process image.');";
                                    } catch (t) {
                                        h.textContent = "Ext.Msg.alert('Unable to show process image:" + t + "');";
                                    }
                                }.bind(this),
                                failure: function() {
                                    h.textContent = "Ext.Msg.alert('Unable to show process image.');";
                                },
                                params: {
                                    profile: ORYX.PROFILE,
                                    uuid: ORYX.UUID,
                                    ppackage: d,
                                    pid: t,
                                    action: "imageview"
                                }
                            });
                        } else h.textContent = "Ext.Msg.alert('No Callable Element specified.');"; else if (t && t.length > 0) {
                            var u = ORYX.EDITOR.getSerializedJSON(), d = jsonPath(u.evalJSON(), "$.properties.package");
                            Ext.Ajax.request({
                                url: ORYX.PATH + "calledelement",
                                method: "POST",
                                success: function(e) {
                                    try {
                                        e.responseText.length > 0 && "false" != e.responseText ? s.setAttributeNS("", "onclick", "new ImageViewer({title: 'Process Image', width: '650', height: '450', autoScroll: true, fixedcenter: true, src: '" + e.responseText + "',hideAction: 'close'}).show();") : s.setAttributeNS("", "onclick", "Ext.Msg.alert('Unable to find process image.');");
                                    } catch (t) {
                                        s.setAttributeNS("", "onclick", "Ext.Msg.alert('Unable to find process image: " + t + "');"), 
                                        ORYX.EDITOR._pluginFacade.raiseEvent({
                                            type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
                                            ntype: "error",
                                            msg: "Error resolving process image :\n" + t,
                                            title: ""
                                        });
                                    }
                                }.bind(this),
                                failure: function() {
                                    s.setAttributeNS("", "onclick", "Ext.Msg.alert('Unable to find process image.');");
                                },
                                params: {
                                    profile: ORYX.PROFILE,
                                    uuid: ORYX.UUID,
                                    ppackage: d,
                                    pid: t,
                                    action: "imageview"
                                }
                            });
                        } else s.setAttributeNS("", "onclick", "Ext.Msg.alert('No Callable Element specified.');");
                        break;

                      case ORYX.CONFIG.TYPE_DIAGRAM_LINK:
                        var p = s.getAttributeNodeNS("http://www.w3.org/1999/xlink", "xlink:onclick");
                        p ? p.textContent = t : s.setAttributeNS("http://www.w3.org/1999/xlink", "xlink:onclick", t);
                    }
                }.bind(this));
            }
        }.bind(this)), this._labels.values().each(function(e) {
            e.update();
        }));
    },
    layout: function() {
        var e = this.getStencil().layout();
        this instanceof ORYX.Core.Node && e && e.each(function(e) {
            e.shape = this, e.forceExecution = !0, this._delegateEvent(e);
        }.bind(this));
    },
    getLabels: function() {
        return this._labels.values();
    },
    getDockers: function() {
        return this.dockers;
    },
    getMagnets: function() {
        return this.magnets;
    },
    getDefaultMagnet: function() {
        return this._defaultMagnet ? this._defaultMagnet : this.magnets.length > 0 ? this.magnets[0] : void 0;
    },
    getParentShape: function() {
        return this.parent;
    },
    getIncomingShapes: function(e) {
        return e && this.incoming.each(e), this.incoming;
    },
    getIncomingNodes: function(e) {
        return this.incoming.select(function(t) {
            var n = t instanceof ORYX.Core.Node;
            return n && e && e(t), n;
        });
    },
    getOutgoingShapes: function(e) {
        return e && this.outgoing.each(e), this.outgoing;
    },
    getOutgoingNodes: function(e) {
        return this.outgoing.select(function(t) {
            var n = t instanceof ORYX.Core.Node;
            return n && e && e(t), n;
        });
    },
    getAllDockedShapes: function(e) {
        var t = this.incoming.concat(this.outgoing);
        return e && t.each(e), t;
    },
    getCanvas: function() {
        return this.parent instanceof ORYX.Core.Canvas ? this.parent : this.parent instanceof ORYX.Core.Shape ? this.parent.getCanvas() : void 0;
    },
    getChildNodes: function(e, t) {
        if (e || t) {
            var n = [];
            return this.nodes.each(function(i) {
                i.isVisible && (t && t(i), n.push(i), e && i instanceof ORYX.Core.Shape && (n = n.concat(i.getChildNodes(e, t))));
            }), n;
        }
        return this.nodes.clone();
    },
    add: function(e, t) {
        if (e instanceof ORYX.Core.UIObject && !(e instanceof ORYX.Core.Edge)) if (this.children.member(e)) ORYX.Log.warn("add: ORYX.Core.UIObject is already a child of this object."); else {
            e.parent && e.parent.remove(e), void 0 != t ? this.children.splice(t, 0, e) : this.children.push(e), 
            e.parent = this;
            var n;
            if (e instanceof ORYX.Core.Node) n = this.node.childNodes[0].childNodes[1], this.nodes.push(e); else if (e instanceof ORYX.Core.Controls.Control) {
                var i = this.node.childNodes[1];
                e instanceof ORYX.Core.Controls.Docker ? (n = i.childNodes[0], this.dockers.length >= 2 ? this.dockers.splice(void 0 !== t ? Math.min(t, this.dockers.length - 1) : this.dockers.length - 1, 0, e) : this.dockers.push(e)) : e instanceof ORYX.Core.Controls.Magnet ? (n = i.childNodes[1], 
                this.magnets.push(e)) : n = i;
            } else n = this.node;
            e.node = void 0 != t && t < n.childNodes.length ? n.insertBefore(e.node, n.childNodes[t]) : n.appendChild(e.node), 
            this._changed(), this.eventHandlerCallback && this.eventHandlerCallback({
                type: ORYX.CONFIG.EVENT_SHAPEADDED,
                shape: e
            });
        } else ORYX.Log.warn("add: Parameter is not of type ORYX.Core.UIObject.");
    },
    remove: function(e) {
        this.children.member(e) ? (this.children = this.children.without(e), e.parent = void 0, 
        e instanceof ORYX.Core.Shape ? e instanceof ORYX.Core.Edge ? (e.removeMarkers(), 
        e.node = this.node.childNodes[0].childNodes[2].removeChild(e.node)) : (e.node = this.node.childNodes[0].childNodes[1].removeChild(e.node), 
        this.nodes = this.nodes.without(e)) : e instanceof ORYX.Core.Controls.Control && (e instanceof ORYX.Core.Controls.Docker ? (e.node = this.node.childNodes[1].childNodes[0].removeChild(e.node), 
        this.dockers = this.dockers.without(e)) : e instanceof ORYX.Core.Controls.Magnet ? (e.node = this.node.childNodes[1].childNodes[1].removeChild(e.node), 
        this.magnets = this.magnets.without(e)) : e.node = this.node.childNodes[1].removeChild(e.node)), 
        this._changed()) : ORYX.Log.warn("remove: ORYX.Core.UIObject is not a child of this object.");
    },
    getIntersectionPoint: function() {
        var e, t, n, i;
        switch (arguments.length) {
          case 2:
            e = arguments[0].x, t = arguments[0].y, n = arguments[1].x, i = arguments[1].y;
            break;

          case 4:
            e = arguments[0], t = arguments[1], n = arguments[2], i = arguments[3];
            break;

          default:
            throw "getIntersectionPoints needs two or four arguments";
        }
        var r, s, o, a, c = this.absoluteBounds();
        if (this.isPointIncluded(e, t, c) ? (r = e, s = t) : (o = e, a = t), this.isPointIncluded(n, i, c) ? (r = n, 
        s = i) : (o = n, a = i), !(r && s && o && a)) return void 0;
        for (var l, h, u = 0, d = 0; ;) {
            var u = Math.min(r, o) + (Math.max(r, o) - Math.min(r, o)) / 2, d = Math.min(s, a) + (Math.max(s, a) - Math.min(s, a)) / 2;
            this.isPointIncluded(u, d, c) ? (r = u, s = d) : (o = u, a = d);
            var p = Math.sqrt(Math.pow(r - o, 2) + Math.pow(s - a, 2));
            if (l = r + (o - r) / p, h = s + (a - s) / p, !this.isPointIncluded(l, h, c)) break;
        }
        return {
            x: l,
            y: h
        };
    },
    isPointIncluded: function() {
        return !1;
    },
    isPointOverOffset: function() {
        return this.isPointIncluded.apply(this, arguments);
    },
    _dockerChanged: function() {},
    createDocker: function(e, t) {
        var n = new ORYX.Core.Controls.Docker({
            eventHandlerCallback: this.eventHandlerCallback
        });
        return n.bounds.registerCallback(this._dockerChangedCallback), t && n.bounds.centerMoveTo(t), 
        this.add(n, e), n;
    },
    serialize: function() {
        var e = arguments.callee.$.serialize.apply(this);
        return e.push({
            name: "bounds",
            prefix: "oryx",
            value: this.bounds.serializeForERDF(),
            type: "literal"
        }), this.getOutgoingShapes().each(function(t) {
            e.push({
                name: "outgoing",
                prefix: "raziel",
                value: "#" + ERDF.__stripHashes(t.resourceId),
                type: "resource"
            });
        }.bind(this)), e.push({
            name: "parent",
            prefix: "raziel",
            value: "#" + ERDF.__stripHashes(this.parent.resourceId),
            type: "resource"
        }), e;
    },
    deserialize: function(e) {
        arguments.callee.$.deserialize.apply(this, arguments);
        var t = e.find(function(e) {
            return "oryx-bounds" == e.prefix + "-" + e.name;
        });
        if (t) {
            var n = t.value.replace(/,/g, " ").split(" ").without("");
            this instanceof ORYX.Core.Edge ? (this.dockers.first().bounds.centerMoveTo(parseFloat(n[0]), parseFloat(n[1])), 
            this.dockers.last().bounds.centerMoveTo(parseFloat(n[2]), parseFloat(n[3]))) : this.bounds.set(parseFloat(n[0]), parseFloat(n[1]), parseFloat(n[2]), parseFloat(n[3]));
        }
    },
    _init: function(e) {
        this._adjustIds(e, 0);
    },
    _adjustIds: function(e, t) {
        if (e instanceof Element) {
            var n = e.getAttributeNS(null, "id");
            n && "" !== n ? e.setAttributeNS(null, "id", this.id + n) : (e.setAttributeNS(null, "id", this.id + "_" + this.id + "_" + t), 
            t++);
            var i = e.getAttributeNS(null, "fill");
            if (i && i.include("url(#") && (i = i.replace(/url\(#/g, "url(#" + this.id), e.setAttributeNS(null, "fill", i)), 
            e.hasChildNodes()) for (var r = 0; r < e.childNodes.length; r++) t = this._adjustIds(e.childNodes[r], t);
        }
        return t;
    },
    toString: function() {
        return "ORYX.Core.Shape " + this.getId();
    }
}, ORYX.Core.Shape = ORYX.Core.AbstractShape.extend(ORYX.Core.Shape), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Controls || (ORYX.Core.Controls = {}), 
ORYX.Core.Controls.Control = ORYX.Core.UIObject.extend({
    toString: function() {
        return "Control " + this.id;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Controls || (ORYX.Core.Controls = {}), 
ORYX.Core.Controls.Magnet = ORYX.Core.Controls.Control.extend({
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.anchorLeft, this.anchorRight, 
        this.anchorTop, this.anchorBottom, this.bounds.set(0, 0, 16, 16), this.node = ORYX.Editor.graft("http://www.w3.org/2000/svg", null, [ "g", {
            "pointer-events": "all"
        }, [ "circle", {
            cx: "8",
            cy: "8",
            r: "4",
            stroke: "none",
            fill: "red",
            "fill-opacity": "0.3"
        } ] ]), this.hide();
    },
    update: function() {
        arguments.callee.$.update.apply(this, arguments);
    },
    _update: function() {
        arguments.callee.$.update.apply(this, arguments);
    },
    refresh: function() {
        arguments.callee.$.refresh.apply(this, arguments);
        var e = this.bounds.upperLeft();
        this.node.setAttributeNS(null, "transform", "translate(" + e.x + ", " + e.y + ")");
    },
    show: function() {
        arguments.callee.$.show.apply(this, arguments);
    },
    toString: function() {
        return "Magnet " + this.id;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Controls || (ORYX.Core.Controls = {}), 
ORYX.Core.Controls.Docker = ORYX.Core.Controls.Control.extend({
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.isMovable = !0, this.bounds.set(0, 0, 16, 16), 
        this.referencePoint = void 0, this._dockedShapeBounds = void 0, this._dockedShape = void 0, 
        this._oldRefPoint1 = void 0, this._oldRefPoint2 = void 0, this.anchorLeft, this.anchorRight, 
        this.anchorTop, this.anchorBottom, this.node = ORYX.Editor.graft("http://www.w3.org/2000/svg", null, [ "g" ]), 
        this._dockerNode = ORYX.Editor.graft("http://www.w3.org/2000/svg", this.node, [ "g", {
            "pointer-events": "all"
        }, [ "circle", {
            cx: "8",
            cy: "8",
            r: "8",
            stroke: "none",
            fill: "none"
        } ], [ "circle", {
            cx: "8",
            cy: "8",
            r: "3",
            stroke: "black",
            fill: "red",
            "stroke-width": "1"
        } ] ]), this._referencePointNode = ORYX.Editor.graft("http://www.w3.org/2000/svg", this.node, [ "g", {
            "pointer-events": "none"
        }, [ "circle", {
            cx: this.bounds.upperLeft().x,
            cy: this.bounds.upperLeft().y,
            r: 3,
            fill: "red",
            "fill-opacity": .4
        } ] ]), this.hide(), this.addEventHandlers(this.node), this._updateCallback = this._changed.bind(this);
    },
    update: function() {
        if (this._dockedShape) {
            if (this._dockedShapeBounds && this._dockedShape instanceof ORYX.Core.Node) {
                var e = this._dockedShapeBounds.width(), t = this._dockedShapeBounds.height();
                e || (e = 1), t || (t = 1);
                var n = this._dockedShape.bounds.width() / e, i = this._dockedShape.bounds.height() / t;
                (1 !== n || 1 !== i) && (this.referencePoint.x *= n, this.referencePoint.y *= i), 
                this._dockedShapeBounds = this._dockedShape.bounds.clone();
            }
            var r = this.parent.dockers.indexOf(this), s = this, o = this.parent.dockers.length > 1 ? 0 === r ? this.parent.dockers[r + 1] : this.parent.dockers[r - 1] : void 0, a = s.getDockedShape() ? s.getAbsoluteReferencePoint() : s.bounds.center(), c = o && o.getDockedShape() ? o.getAbsoluteReferencePoint() : o ? o.bounds.center() : void 0;
            if (!c) {
                var l = this._dockedShape.absoluteCenterXY(), h = this._dockedShape.bounds.width() * this._dockedShape.bounds.height();
                c = {
                    x: a.x + (l.x - a.x) * -h,
                    y: a.y + (l.y - a.y) * -h
                };
            }
            var u = void 0;
            if (u = this._dockedShape.getIntersectionPoint(a, c), u || (u = this.getAbsoluteReferencePoint()), 
            this.parent && this.parent.parent) {
                var d = this.parent.parent.absoluteXY();
                u.x -= d.x, u.y -= d.y;
            }
            this.bounds.centerMoveTo(u), this._oldRefPoint1 = a, this._oldRefPoint2 = c;
        }
        arguments.callee.$.update.apply(this, arguments);
    },
    refresh: function() {
        arguments.callee.$.refresh.apply(this, arguments);
        var e = this.bounds.upperLeft();
        if (this._dockerNode.setAttributeNS(null, "transform", "translate(" + e.x + ", " + e.y + ")"), 
        e = Object.clone(this.referencePoint), e && this._dockedShape) {
            var t;
            t = this.parent instanceof ORYX.Core.Edge ? this._dockedShape.absoluteXY() : this._dockedShape.bounds.upperLeft(), 
            e.x += t.x, e.y += t.y;
        } else e = this.bounds.center();
        this._referencePointNode.setAttributeNS(null, "transform", "translate(" + e.x + ", " + e.y + ")");
    },
    setReferencePoint: function(e) {
        this.referencePoint === e || this.referencePoint && e && this.referencePoint.x === e.x && this.referencePoint.y === e.y || (this.referencePoint = e, 
        this._changed());
    },
    getAbsoluteReferencePoint: function() {
        if (this.referencePoint && this._dockedShape) {
            var e = this._dockedShape.absoluteXY();
            return {
                x: this.referencePoint.x + e.x,
                y: this.referencePoint.y + e.y
            };
        }
        return void 0;
    },
    setDockedShape: function(e) {
        this._dockedShape && (this._dockedShape.bounds.unregisterCallback(this._updateCallback), 
        this === this.parent.dockers.first() ? (this.parent.incoming = this.parent.incoming.without(this._dockedShape), 
        this._dockedShape.outgoing = this._dockedShape.outgoing.without(this.parent)) : this === this.parent.dockers.last() && (this.parent.outgoing = this.parent.outgoing.without(this._dockedShape), 
        this._dockedShape.incoming = this._dockedShape.incoming.without(this.parent))), 
        this._dockedShape = e, this._dockedShapeBounds = void 0;
        var t = void 0;
        if (this._dockedShape) {
            this === this.parent.dockers.first() ? (this.parent.incoming.push(e), e.outgoing.push(this.parent)) : this === this.parent.dockers.last() && (this.parent.outgoing.push(e), 
            e.incoming.push(this.parent));
            var n = this.bounds, i = e.absoluteXY();
            t = {
                x: n.center().x - i.x,
                y: n.center().y - i.y
            }, this._dockedShapeBounds = this._dockedShape.bounds.clone(), this._dockedShape.bounds.registerCallback(this._updateCallback), 
            this.setDockerColor(ORYX.CONFIG.DOCKER_DOCKED_COLOR);
        } else this.setDockerColor(ORYX.CONFIG.DOCKER_UNDOCKED_COLOR);
        this.setReferencePoint(t), this._changed();
    },
    getDockedShape: function() {
        return this._dockedShape;
    },
    isDocked: function() {
        return !!this._dockedShape;
    },
    setDockerColor: function(e) {
        this._dockerNode.lastChild.setAttributeNS(null, "fill", e);
    },
    hide: function() {
        this.node.setAttributeNS(null, "visibility", "hidden"), this.children.each(function(e) {
            e.hide();
        });
    },
    show: function() {
        this.node.setAttributeNS(null, "visibility", "visible"), this.children.each(function(e) {
            e.show();
        });
    },
    toString: function() {
        return "Docker " + this.id;
    }
}), !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Node = {
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.isSelectable = !0, this.isMovable = !0, 
        this._dockerUpdated = !1, this._oldBounds = new ORYX.Core.Bounds(), this._svgShapes = [], 
        this.minimumSize = void 0, this.maximumSize = void 0, this.isHorizontallyResizable = !1, 
        this.isVerticallyResizable = !1, this.dataId = void 0, this._init(this._stencil.view());
    },
    setSelectable: function(e) {
        this.isSelectable = e;
    },
    setMovable: function(e) {
        this.isMovable = e;
    },
    _update: function() {
        if (this.dockers.invoke("update"), this.isChanged) {
            var e = this.bounds, t = this._oldBounds;
            if (this.isResized) {
                var n = e.width() / t.width(), i = e.height() / t.height();
                this._svgShapes.each(function(r) {
                    r.isHorizontallyResizable && (r.width = r.oldWidth * n), r.isVerticallyResizable && (r.height = r.oldHeight * i);
                    var s, o = r.anchorLeft, a = r.anchorRight;
                    a ? (s = t.width() - (r.oldX + r.oldWidth), o ? r.width = e.width() - r.x - s : r.x = e.width() - (s + r.width)) : o || (r.x = n * r.oldX, 
                    r.isHorizontallyResizable || (r.x = r.x + r.width * n / 2 - r.width / 2));
                    var c = r.anchorTop, l = r.anchorBottom;
                    l ? (s = t.height() - (r.oldY + r.oldHeight), c ? r.height = e.height() - r.y - s : r._isYLocked || (r.y = e.height() - (s + r.height))) : c || (r.y = i * r.oldY, 
                    r.isVerticallyResizable || (r.y = r.y + r.height * i / 2 - r.height / 2));
                });
                var r = {
                    x: 0,
                    y: 0
                };
                this.isHorizontallyResizable || e.width() === t.width() || (r.x = t.width() - e.width()), 
                this.isVerticallyResizable || e.height() === t.height() || (r.y = t.height() - e.height()), 
                (0 !== r.x || 0 !== r.y) && e.extend(r), r = {
                    x: 0,
                    y: 0
                };
                var s, o;
                this.minimumSize && (ORYX.Log.debug("Shape (%0)'s min size: (%1x%2)", this, this.minimumSize.width, this.minimumSize.height), 
                s = this.minimumSize.width - e.width(), s > 0 && (r.x += s), o = this.minimumSize.height - e.height(), 
                o > 0 && (r.y += o)), this.maximumSize && (ORYX.Log.debug("Shape (%0)'s max size: (%1x%2)", this, this.maximumSize.width, this.maximumSize.height), 
                s = e.width() - this.maximumSize.width, s > 0 && (r.x -= s), o = e.height() - this.maximumSize.height, 
                o > 0 && (r.y -= o)), (0 !== r.x || 0 !== r.y) && e.extend(r);
                var a, c, l, h, u, d, p, n = e.width() / t.width(), i = e.height() / t.height();
                this.magnets.each(function(r) {
                    a = r.anchorLeft, c = r.anchorRight, l = r.anchorTop, h = r.anchorBottom, u = r.bounds.center(), 
                    d = a ? u.x : c ? e.width() - (t.width() - u.x) : u.x * n, p = l ? u.y : h ? e.height() - (t.height() - u.y) : u.y * i, 
                    (u.x !== d || u.y !== p) && r.bounds.centerMoveTo(d, p);
                }), this.getLabels().each(function(r) {
                    a = r.anchorLeft, c = r.anchorRight, l = r.anchorTop, h = r.anchorBottom, a || (c ? r.x = e.width() - (t.width() - r.oldX) : r.x *= n), 
                    l || (h ? r.y = e.height() - (t.height() - r.oldY) : r.y *= i);
                });
                var f = this.dockers[0];
                f && (f.bounds.unregisterCallback(this._dockerChangedCallback), this._dockerUpdated || (f.bounds.centerMoveTo(this.bounds.center()), 
                this._dockerUpdated = !1), f.update(), f.bounds.registerCallback(this._dockerChangedCallback)), 
                this.isResized = !1;
            }
            this.refresh(), this.isChanged = !1, this._oldBounds = this.bounds.clone();
        }
        this.children.each(function(e) {
            e instanceof ORYX.Core.Controls.Docker || e._update();
        }), this.dockers.length > 0 && !this.dockers.first().getDockedShape() && this.dockers.each(function(e) {
            e.bounds.centerMoveTo(this.bounds.center());
        }.bind(this));
    },
    refresh: function() {
        arguments.callee.$.refresh.apply(this, arguments);
        var e = this.bounds.upperLeft().x, t = this.bounds.upperLeft().y;
        this.node.firstChild.setAttributeNS(null, "transform", "translate(" + e + ", " + t + ")"), 
        this.node.childNodes[1].childNodes[1].setAttributeNS(null, "transform", "translate(" + e + ", " + t + ")"), 
        this._svgShapes.each(function(e) {
            e.update();
        });
    },
    _dockerChanged: function() {
        var e = this.dockers[0];
        this.bounds.centerMoveTo(e.bounds.center()), this._dockerUpdated = !0;
    },
    _initSVGShapes: function(e) {
        var t = [];
        try {
            var n = new ORYX.Core.SVG.SVGShape(e);
            t.push(n);
        } catch (i) {}
        if (e.hasChildNodes()) for (var r = 0; r < e.childNodes.length; r++) t = t.concat(this._initSVGShapes(e.childNodes[r]));
        return t;
    },
    isPointIncluded: function(e, t, n) {
        var i = n && n instanceof ORYX.Core.Bounds ? n : this.absoluteBounds();
        if (!i.isIncluded(e, t)) return !1;
        var r = i.upperLeft(), s = e - r.x, o = t - r.y, a = 0;
        do var c = this._svgShapes[a++].isPointIncluded(s, o); while (!c && a < this._svgShapes.length);
        return c;
    },
    isPointOverOffset: function(e, t) {
        var n = arguments.callee.$.isPointOverOffset.apply(this, arguments);
        if (n) {
            var i = this.absoluteBounds();
            if (i.widen(-ORYX.CONFIG.BORDER_OFFSET), !i.isIncluded(e, t)) return !0;
        }
        return !1;
    },
    serialize: function() {
        var e = arguments.callee.$.serialize.apply(this);
        this.dockers.each(function(t) {
            if (t.getDockedShape()) {
                var n = t.referencePoint;
                n = n ? n : t.bounds.center(), e.push({
                    name: "docker",
                    prefix: "oryx",
                    value: $H(n).values().join(","),
                    type: "literal"
                });
            }
        }.bind(this));
        try {
            var t = this.getStencil().serialize();
            t.type && (t.shape = this, t.data = e, t.result = void 0, t.forceExecution = !0, 
            this._delegateEvent(t), t.result && (e = t.result));
        } catch (n) {}
        return e;
    },
    deserialize: function(e) {
        arguments.callee.$.deserialize.apply(this, [ e ]);
        try {
            var t = this.getStencil().deserialize();
            t.type && (t.shape = this, t.data = e, t.result = void 0, t.forceExecution = !0, 
            this._delegateEvent(t), t.result && (e = t.result));
        } catch (n) {}
        var i = e.findAll(function(e) {
            return "raziel-outgoing" == e.prefix + "-" + e.name;
        });
        if (i.each(function(e) {
            if (this.parent) {
                var t = this.getCanvas().getChildShapeByResourceId(e.value);
                t && (t instanceof ORYX.Core.Edge ? (t.dockers.first().setDockedShape(this), t.dockers.first().setReferencePoint(t.dockers.first().bounds.center())) : t.dockers.length > 0 && t.dockers.first().setDockedShape(this));
            }
        }.bind(this)), 1 === this.dockers.length) {
            var r;
            if (r = e.find(function(e) {
                return "oryx-dockers" == e.prefix + "-" + e.name;
            })) {
                var s = r.value.replace(/,/g, " ").split(" ").without("").without("#");
                2 === s.length && this.dockers[0].getDockedShape() ? this.dockers[0].setReferencePoint({
                    x: parseFloat(s[0]),
                    y: parseFloat(s[1])
                }) : this.dockers[0].bounds.centerMoveTo(parseFloat(s[0]), parseFloat(s[1]));
            }
        }
    },
    _init: function(e) {
        arguments.callee.$._init.apply(this, arguments);
        var t = e.getElementsByTagName("g")[0], n = e.ownerDocument.createAttributeNS(null, "title");
        n.nodeValue = this.getStencil().title(), t.setAttributeNode(n);
        var i = e.ownerDocument.createAttributeNS(null, "id");
        i.nodeValue = this.id, t.setAttributeNode(i);
        var r = this.node.childNodes[0].childNodes[0];
        t = r.appendChild(t), this.addEventHandlers(t);
        var s = t.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "minimumSize");
        if (s) {
            s = s.replace("/,/g", " ");
            var o = s.split(" ");
            o = o.without(""), this.minimumSize = o.length > 1 ? {
                width: parseFloat(o[0]),
                height: parseFloat(o[1])
            } : {
                width: 1,
                height: 1
            };
        }
        var a = t.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "maximumSize");
        if (a) {
            a = a.replace("/,/g", " ");
            var c = a.split(" ");
            c = c.without(""), c.length > 1 && (this.maximumSize = {
                width: parseFloat(c[0]),
                height: parseFloat(c[1])
            });
        }
        if (this.minimumSize && this.maximumSize && (this.minimumSize.width > this.maximumSize.width || this.minimumSize.height > this.maximumSize.height)) throw this + ": Minimum Size must be greater than maxiumSize.";
        this._svgShapes = this._initSVGShapes(t);
        var l = {
            x: void 0,
            y: void 0
        }, h = {
            x: void 0,
            y: void 0
        }, u = this;
        this._svgShapes.each(function(e) {
            l.x = void 0 !== l.x ? Math.min(l.x, e.x) : e.x, l.y = void 0 !== l.y ? Math.min(l.y, e.y) : e.y, 
            h.x = void 0 !== h.x ? Math.max(h.x, e.x + e.width) : e.x + e.width, h.y = void 0 !== h.y ? Math.max(h.y, e.y + e.height) : e.y + e.height, 
            e.isHorizontallyResizable && (u.isHorizontallyResizable = !0, u.isResizable = !0), 
            e.isVerticallyResizable && (u.isVerticallyResizable = !0, u.isResizable = !0), e.anchorTop && e.anchorBottom && (u.isVerticallyResizable = !0, 
            u.isResizable = !0), e.anchorLeft && e.anchorRight && (u.isHorizontallyResizable = !0, 
            u.isResizable = !0);
        }), this._svgShapes.each(function(e) {
            e.x -= l.x, e.y -= l.y, e.update();
        });
        var d = l.x, p = l.y;
        h.x -= d, h.y -= p, l.x = 0, l.y = 0, 0 === h.x && (h.x = 1), 0 === h.y && (h.y = 1), 
        this._oldBounds.set(l, h), this.bounds.set(l, h);
        var f = e.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_ORYX, "magnets");
        if (f && f.length > 0) {
            f = $A(f[0].getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_ORYX, "magnet"));
            var u = this;
            f.each(function(e) {
                var t = new ORYX.Core.Controls.Magnet({
                    eventHandlerCallback: u.eventHandlerCallback
                }), n = parseFloat(e.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "cx")), i = parseFloat(e.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "cy"));
                t.bounds.centerMoveTo({
                    x: n - d,
                    y: i - p
                });
                var r = e.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "anchors");
                if (r) {
                    r = r.replace("/,/g", " "), r = r.split(" ").without("");
                    for (var s = 0; s < r.length; s++) switch (r[s].toLowerCase()) {
                      case "left":
                        t.anchorLeft = !0;
                        break;

                      case "right":
                        t.anchorRight = !0;
                        break;

                      case "top":
                        t.anchorTop = !0;
                        break;

                      case "bottom":
                        t.anchorBottom = !0;
                    }
                }
                if (u.add(t), !this._defaultMagnet) {
                    var o = e.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "default");
                    o && "yes" === o.toLowerCase() && (u._defaultMagnet = t);
                }
            });
        } else {
            var g = new ORYX.Core.Controls.Magnet();
            g.bounds.centerMoveTo(this.bounds.width() / 2, this.bounds.height() / 2), this.add(g);
        }
        var m = e.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_ORYX, "docker");
        if (m && m.length > 0) {
            m = m[0];
            var O = this.createDocker(), R = parseFloat(m.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "cx")), E = parseFloat(m.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "cy"));
            O.bounds.centerMoveTo({
                x: R - d,
                y: E - p
            });
            var S = m.getAttributeNS(ORYX.CONFIG.NAMESPACE_ORYX, "anchors");
            if (S) {
                S = S.replace("/,/g", " "), S = S.split(" ").without("");
                for (var N = 0; N < S.length; N++) switch (S[N].toLowerCase()) {
                  case "left":
                    O.anchorLeft = !0;
                    break;

                  case "right":
                    O.anchorRight = !0;
                    break;

                  case "top":
                    O.anchorTop = !0;
                    break;

                  case "bottom":
                    O.anchorBottom = !0;
                }
            }
        }
        var v = t.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "text");
        $A(v).each(function(e) {
            var t = new ORYX.Core.SVG.Label({
                textElement: e,
                shapeId: this.id
            });
            t.x -= d, t.y -= p, this._labels[t.id] = t;
        }.bind(this));
    },
    createDocker: function() {
        var e = new ORYX.Core.Controls.Docker({
            eventHandlerCallback: this.eventHandlerCallback
        });
        return e.bounds.registerCallback(this._dockerChangedCallback), this.dockers.push(e), 
        e.parent = this, e.bounds.registerCallback(this._changedCallback), e;
    },
    toString: function() {
        return this._stencil.title() + " " + this.id;
    }
}, ORYX.Core.Node = ORYX.Core.Shape.extend(ORYX.Core.Node), NAMESPACE_SVG = "http://www.w3.org/2000/svg", 
NAMESPACE_ORYX = "http://www.b3mn.org/oryx", !ORYX) var ORYX = {};

if (ORYX.Core || (ORYX.Core = {}), ORYX.Core.Edge = {
    construct: function(e, t) {
        arguments.callee.$.construct.apply(this, arguments), this.isMovable = !0, this.isSelectable = !0, 
        this._dockerUpdated = !1, this._markers = new Hash(), this._paths = [], this._interactionPaths = [], 
        this._dockersByPath = new Hash(), this._markersByPath = new Hash(), this.attachedNodePositionData = new Hash();
        var n = this.node.childNodes[0].childNodes[0];
        n = ORYX.Editor.graft("http://www.w3.org/2000/svg", n, [ "g", {
            "pointer-events": "painted"
        } ]), this.addEventHandlers(n), this._oldBounds = this.bounds.clone(), this._init(this._stencil.view()), 
        t instanceof Array && this.deserialize(t);
    },
    setSelectable: function(e) {
        this.isSelectable = e;
    },
    setMovable: function(e) {
        this.isMovable = e;
    },
    _update: function(e) {
        if (this._dockerUpdated || this.isChanged || e) {
            this.dockers.invoke("update"), (0 === this.bounds.width() || 0 === this.bounds.height()) && (this.bounds.moveBy({
                x: 0 === this.bounds.width() ? -1 : 0,
                y: 0 === this.bounds.height() ? -1 : 0
            }), this.bounds.extend({
                x: 0 === this.bounds.width() ? 2 : 0,
                y: 0 === this.bounds.height() ? 2 : 0
            }));
            var t = this.bounds.upperLeft(), n = this._oldBounds.upperLeft(), i = 0 === this._oldBounds.width() ? this.bounds.width() : this._oldBounds.width(), r = 0 === this._oldBounds.height() ? this.bounds.height() : this._oldBounds.height(), s = t.x - n.x, o = t.y - n.y, a = this.bounds.width() / i, c = this.bounds.height() / r;
            if (this.dockers.each(function(e) {
                if (e.bounds.unregisterCallback(this._dockerChangedCallback), !this._dockerUpdated && (e.bounds.moveBy(s, o), 
                1 !== a || 1 !== c)) {
                    var n = e.bounds.upperLeft().x - t.x, i = e.bounds.upperLeft().y - t.y;
                    e.bounds.moveTo(t.x + n * a, t.y + i * c);
                }
                e.update(), e.bounds.registerCallback(this._dockerChangedCallback);
            }.bind(this)), this._dockerUpdated) {
                var l = this.dockers.first().bounds.center(), h = this.dockers.first().bounds.center();
                this.dockers.each(function(e) {
                    var t = e.bounds.center();
                    l.x = Math.min(l.x, t.x), l.y = Math.min(l.y, t.y), h.x = Math.max(h.x, t.x), h.y = Math.max(h.y, t.y);
                }.bind(this)), this.bounds.set(Object.clone(l), Object.clone(h));
            }
            this.getLabels().each(function(e) {
                switch (e.edgePosition) {
                  case "freeMoved":
                    e.x = e.x, e.y = e.y;
                    break;

                  case "starttop":
                    var t = this._getAngle(this.dockers[0], this.dockers[1]), n = this.dockers.first().bounds.center();
                    90 >= t || t > 270 ? (e.horizontalAlign("left"), e.verticalAlign("bottom"), e.x = n.x + e.getOffsetTop(), 
                    e.y = n.y - e.getOffsetTop(), e.rotate(360 - t, n)) : (e.horizontalAlign("right"), 
                    e.verticalAlign("bottom"), e.x = n.x - e.getOffsetTop(), e.y = n.y - e.getOffsetTop(), 
                    e.rotate(180 - t, n));
                    break;

                  case "startbottom":
                    var t = this._getAngle(this.dockers[0], this.dockers[1]), n = this.dockers.first().bounds.center();
                    90 >= t || t > 270 ? (e.horizontalAlign("left"), e.verticalAlign("top"), e.x = n.x + e.getOffsetBottom(), 
                    e.y = n.y + e.getOffsetBottom(), e.rotate(360 - t, n)) : (e.horizontalAlign("right"), 
                    e.verticalAlign("top"), e.x = n.x - e.getOffsetBottom(), e.y = n.y + e.getOffsetBottom(), 
                    e.rotate(180 - t, n));
                    break;

                  case "midtop":
                    var i = this.dockers.length;
                    if (0 == i % 2) {
                        var t = this._getAngle(this.dockers[i / 2 - 1], this.dockers[i / 2]), r = this.dockers[i / 2 - 1].bounds.center(), s = this.dockers[i / 2].bounds.center(), n = {
                            x: (r.x + s.x) / 2,
                            y: (r.y + s.y) / 2
                        };
                        e.horizontalAlign("center"), e.verticalAlign("bottom"), e.x = n.x, e.y = n.y - e.getOffsetTop(), 
                        90 >= t || t > 270 ? e.rotate(360 - t, n) : e.rotate(180 - t, n);
                    } else {
                        var o = parseInt(i / 2), t = this._getAngle(this.dockers[o], this.dockers[o + 1]), n = this.dockers[o].bounds.center();
                        90 >= t || t > 270 ? (e.horizontalAlign("left"), e.verticalAlign("bottom"), e.x = n.x + e.getOffsetTop(), 
                        e.y = n.y - e.getOffsetTop(), e.rotate(360 - t, n)) : (e.horizontalAlign("right"), 
                        e.verticalAlign("bottom"), e.x = n.x - e.getOffsetTop(), e.y = n.y - e.getOffsetTop(), 
                        e.rotate(180 - t, n));
                    }
                    break;

                  case "midbottom":
                    var i = this.dockers.length;
                    if (0 == i % 2) {
                        var t = this._getAngle(this.dockers[i / 2 - 1], this.dockers[i / 2]), r = this.dockers[i / 2 - 1].bounds.center(), s = this.dockers[i / 2].bounds.center(), n = {
                            x: (r.x + s.x) / 2,
                            y: (r.y + s.y) / 2
                        };
                        e.horizontalAlign("center"), e.verticalAlign("top"), e.x = n.x, e.y = n.y + e.getOffsetTop(), 
                        90 >= t || t > 270 ? e.rotate(360 - t, n) : e.rotate(180 - t, n);
                    } else {
                        var o = parseInt(i / 2), t = this._getAngle(this.dockers[o], this.dockers[o + 1]), n = this.dockers[o].bounds.center();
                        90 >= t || t > 270 ? (e.horizontalAlign("left"), e.verticalAlign("top"), e.x = n.x + e.getOffsetBottom(), 
                        e.y = n.y + e.getOffsetBottom(), e.rotate(360 - t, n)) : (e.horizontalAlign("right"), 
                        e.verticalAlign("top"), e.x = n.x - e.getOffsetBottom(), e.y = n.y + e.getOffsetBottom(), 
                        e.rotate(180 - t, n));
                    }
                    break;

                  case "endtop":
                    var a = this.dockers.length, t = this._getAngle(this.dockers[a - 2], this.dockers[a - 1]), n = this.dockers.last().bounds.center();
                    90 >= t || t > 270 ? (e.horizontalAlign("right"), e.verticalAlign("bottom"), e.x = n.x - e.getOffsetTop(), 
                    e.y = n.y - e.getOffsetTop(), e.rotate(360 - t, n)) : (e.horizontalAlign("left"), 
                    e.verticalAlign("bottom"), e.x = n.x + e.getOffsetTop(), e.y = n.y - e.getOffsetTop(), 
                    e.rotate(180 - t, n));
                    break;

                  case "endbottom":
                    var a = this.dockers.length, t = this._getAngle(this.dockers[a - 2], this.dockers[a - 1]), n = this.dockers.last().bounds.center();
                    90 >= t || t > 270 ? (e.horizontalAlign("right"), e.verticalAlign("top"), e.x = n.x - e.getOffsetBottom(), 
                    e.y = n.y + e.getOffsetBottom(), e.rotate(360 - t, n)) : (e.horizontalAlign("left"), 
                    e.verticalAlign("top"), e.x = n.x + e.getOffsetBottom(), e.y = n.y + e.getOffsetBottom(), 
                    e.rotate(180 - t, n));
                }
            }.bind(this)), this.children.each(function(e) {
                e instanceof ORYX.Core.Node && this.calculatePositionOfAttachedChildNode.call(this, e);
            }.bind(this)), this.refreshAttachedNodes(), this.refresh(), this.isChanged = !1, 
            this._dockerUpdated = !1, this._oldBounds = this.bounds.clone();
        }
    },
    movePointToUpperLeftOfNode: function(e, t) {
        e.x -= t.width() / 2, e.y -= t.height() / 2;
    },
    refreshAttachedNodes: function() {
        this.attachedNodePositionData.values().each(function(e) {
            var t = e.segment.docker1.bounds.center(), n = e.segment.docker2.bounds.center();
            this.relativizePoint(t), this.relativizePoint(n);
            var i = new Object();
            i.x = t.x + e.relativDistanceFromDocker1 * (n.x - t.x), i.y = t.y + e.relativDistanceFromDocker1 * (n.y - t.y), 
            this.movePointToUpperLeftOfNode(i, e.node.bounds), e.node.bounds.moveTo(i), e.node._update();
        }.bind(this));
    },
    calculatePositionOfAttachedChildNode: function(e) {
        var t = new Object();
        t.x = 0, t.y = 0, this.attachedNodePositionData[e.getId()] ? e.isChanged && this.findEdgeSegmentForNode(e) : (this.attachedNodePositionData[e.getId()] = new Object(), 
        this.attachedNodePositionData[e.getId()].relativDistanceFromDocker1 = 0, this.attachedNodePositionData[e.getId()].node = e, 
        this.attachedNodePositionData[e.getId()].segment = new Object(), this.findEdgeSegmentForNode(e));
    },
    findEdgeSegmentForNode: function(e) {
        var t = this.dockers.length, n = void 0;
        for (i = 1; t > i; i++) {
            var r = this.dockers[i - 1].bounds.center(), s = this.dockers[i].bounds.center();
            this.relativizePoint(r), this.relativizePoint(s);
            var o = e.bounds.center(), a = ORYX.Core.Math.distancePointLinie(r, s, o, !0);
            (a || 0 == a) && (!n && 0 != n || n > a) && (n = a, this.attachedNodePositionData[e.getId()].segment.docker1 = this.dockers[i - 1], 
            this.attachedNodePositionData[e.getId()].segment.docker2 = this.dockers[i]), a || n || 0 == n || (this.attachedNodePositionData[e.getId()].relativDistanceFromDocker1 = ORYX.Core.Math.getDistancePointToPoint(o, r) < ORYX.Core.Math.getDistancePointToPoint(o, s) ? 0 : 1, 
            this.attachedNodePositionData[e.getId()].segment.docker1 = this.dockers[i - 1], 
            this.attachedNodePositionData[e.getId()].segment.docker2 = this.dockers[i]);
        }
        (n || 0 == n) && (this.attachedNodePositionData[e.getId()].relativDistanceFromDocker1 = this.getLineParameterForPosition(this.attachedNodePositionData[e.getId()].segment.docker1, this.attachedNodePositionData[e.getId()].segment.docker2, e));
    },
    getLineParameterForPosition: function(e, t, n) {
        var i = e.bounds.center(), r = t.bounds.center();
        this.relativizePoint(i), this.relativizePoint(r);
        var s = ORYX.Core.Math.getPointOfIntersectionPointLine(i, r, n.bounds.center(), !0);
        if (!s) return 0;
        var o = ORYX.Core.Math.getDistancePointToPoint(s, i) / ORYX.Core.Math.getDistancePointToPoint(i, r);
        return o;
    },
    relativizePoint: function(e) {
        e.x -= this.bounds.upperLeft().x, e.y -= this.bounds.upperLeft().y;
    },
    refresh: function() {
        arguments.callee.$.refresh.apply(this, arguments);
        var e;
        if (this._paths.each(function(t, n) {
            var i = this._dockersByPath[t.id], r = void 0, s = void 0;
            e ? s = "M" + e.x + " " + e.y : (r = i[0].bounds.center(), e = r, s = "M" + r.x + " " + r.y);
            for (var o = 1; o < i.length; o++) r = i[o].bounds.center(), s = s + "L" + r.x + " " + r.y + " ", 
            e = r;
            t.setAttributeNS(null, "d", s), this._interactionPaths[n].setAttributeNS(null, "d", s);
        }.bind(this)), this.getChildNodes().length > 0) {
            var t = this.bounds.upperLeft().x, n = this.bounds.upperLeft().y;
            this.node.firstChild.childNodes[1].setAttributeNS(null, "transform", "translate(" + t + ", " + n + ")");
        }
    },
    getIntersectionPoint: function() {
        var e = Math.floor(this.dockers.length / 2);
        return ORYX.Core.Math.midPoint(this.dockers[e - 1].bounds.center(), this.dockers[e].bounds.center());
    },
    isPointIncluded: function(e, t) {
        var n = this.absoluteBounds().isIncluded(e, t, ORYX.CONFIG.OFFSET_EDGE_BOUNDS), i = void 0;
        if (n && this.dockers.length > 0) {
            var r, s, o = 0;
            do r = this.dockers[o].bounds.center(), s = this.dockers[++o].bounds.center(), i = ORYX.Core.Math.isPointInLine(e, t, r.x, r.y, s.x, s.y, ORYX.CONFIG.OFFSET_EDGE_BOUNDS); while (!i && o < this.dockers.length - 1);
        }
        return i;
    },
    isPointOverOffset: function() {
        return !1;
    },
    _getAngle: function(e, t) {
        var n = e.absoluteCenterXY(), i = t.absoluteCenterXY();
        if (n.x == i.x && n.y == i.y) return 0;
        var r = 180 * Math.asin(Math.sqrt(Math.pow(n.y - i.y, 2)) / Math.sqrt(Math.pow(i.x - n.x, 2) + Math.pow(n.y - i.y, 2))) / Math.PI;
        return i.x >= n.x && i.y <= n.y ? r : i.x < n.x && i.y <= n.y ? 180 - r : i.x < n.x && i.y > n.y ? 180 + r : 360 - r;
    },
    alignDockers: function() {
        this._update(!0);
        var e = this.dockers.first().bounds.center(), t = this.dockers.last().bounds.center(), n = t.x - e.x, i = t.y - e.y, r = this.dockers.length - 1;
        this.dockers.each(function(t, s) {
            var o = s / r;
            t.bounds.unregisterCallback(this._dockerChangedCallback), t.bounds.moveTo(e.x + o * n, e.y + o * i), 
            t.bounds.registerCallback(this._dockerChangedCallback);
        }.bind(this)), this._dockerChanged();
    },
    add: function(e) {
        if (arguments.callee.$.add.apply(this, arguments), e instanceof ORYX.Core.Controls.Docker && this.dockers.include(e)) {
            var t = this._dockersByPath.values()[0];
            t && t.splice(this.dockers.indexOf(e), 0, e), this.handleChildShapesAfterAddDocker(e);
        }
    },
    handleChildShapesAfterAddDocker: function(e) {
        if (!e instanceof ORYX.Core.Controls.Docker) return void 0;
        var t = this.dockers.indexOf(e);
        if (!(t > 0 && t < this.dockers.length - 1)) return void 0;
        var n = this.dockers[t - 1], i = this.dockers[t + 1], r = this.getAttachedNodePositionDataForSegment(n, i), s = ORYX.Core.Math.getDistancePointToPoint(n.bounds.center(), e.bounds.center()), o = ORYX.Core.Math.getDistancePointToPoint(i.bounds.center(), e.bounds.center());
        if (s + o) {
            var a = s / (s + o);
            r.each(function(t) {
                if (t.value.relativDistanceFromDocker1 < a) t.value.segment.docker2 = e, t.value.relativDistanceFromDocker1 = t.value.relativDistanceFromDocker1 / a; else {
                    t.value.segment.docker1 = e;
                    var n = 1 - a, i = t.value.relativDistanceFromDocker1 - a;
                    t.value.relativDistanceFromDocker1 = i / n;
                }
            }), this.refreshAttachedNodes();
        }
    },
    getAttachedNodePositionDataForSegment: function(e, t) {
        if (!(e instanceof ORYX.Core.Controls.Docker && t instanceof ORYX.Core.Controls.Docker)) return [];
        var n = this.attachedNodePositionData.findAll(function(n) {
            return n.value.segment.docker1 === e && n.value.segment.docker2 === t;
        });
        return n ? n : [];
    },
    remove: function(e) {
        arguments.callee.$.remove.apply(this, arguments), this.attachedNodePositionData[e.getId()] && delete this.attachedNodePositionData[e.getId()], 
        e instanceof ORYX.Core.Controls.Docker && this.handleChildShapesAfterRemoveDocker(e);
    },
    handleChildShapesAfterRemoveDocker: function(e) {
        e instanceof ORYX.Core.Controls.Docker && (this.attachedNodePositionData.each(function(t) {
            if (t.value.segment.docker1 === e) {
                var n = this.dockers.indexOf(t.value.segment.docker2);
                if (-1 == n) return;
                t.value.segment.docker1 = this.dockers[n - 1];
            } else if (t.value.segment.docker2 === e) {
                var n = this.dockers.indexOf(t.value.segment.docker1);
                if (-1 == n) return;
                t.value.segment.docker2 = this.dockers[n + 1];
            }
        }.bind(this)), this.refreshAttachedNodes());
    },
    addDocker: function(e, t) {
        var n, i;
        return this._dockersByPath.any(function(r) {
            return r.value.any(function(s) {
                if (n) {
                    var o = n.bounds.center(), a = s.bounds.center();
                    if (ORYX.Core.Math.isPointInLine(e.x, e.y, o.x, o.y, a.x, a.y, 10)) {
                        var c = this._paths.find(function(e) {
                            return e.id === r.key;
                        });
                        if (c) {
                            var l = c.getAttributeNS(NAMESPACE_ORYX, "allowDockers");
                            if (l && "no" === l.toLowerCase()) return !0;
                        }
                        var h = t ? t : this.createDocker(this.dockers.indexOf(n) + 1, e);
                        return h.bounds.centerMoveTo(e), t && this.add(h, this.dockers.indexOf(n) + 1), 
                        i = h, !0;
                    }
                    return n = s, !1;
                }
                return n = s, !1;
            }.bind(this));
        }.bind(this)), i;
    },
    removeDocker: function(e) {
        this.dockers.length > 2 && this.dockers.first() !== e && this._dockersByPath.any(function(t) {
            return t.value.member(e) ? e === t.value.last() ? !0 : (this.remove(e), this._dockersByPath[t.key] = t.value.without(e), 
            this.isChanged = !0, this._dockerChanged(), !0) : !1;
        }.bind(this));
    },
    removeUnusedDockers: function() {
        var e = $H({});
        return this.dockers.each(function(t, n) {
            if (0 != n && n != this.dockers.length - 1) {
                var i = this.dockers[n - 1];
                -1 != e.values().indexOf(i) && this.dockers[n - 2] && (i = this.dockers[n - 2]);
                var r = this.dockers[n + 1], s = i.getDockedShape() && i.referencePoint ? i.getAbsoluteReferencePoint() : i.bounds.center(), o = r.getDockedShape() && r.referencePoint ? r.getAbsoluteReferencePoint() : r.bounds.center(), a = t.bounds.center();
                ORYX.Core.Math.isPointInLine(a.x, a.y, s.x, s.y, o.x, o.y, 1) && (e[n] = t);
            }
        }.bind(this)), e.each(function(e) {
            this.removeDocker(e.value);
        }.bind(this)), e.values().length > 0 && this._update(!0), e;
    },
    _init: function(e) {
        arguments.callee.$._init.apply(this, arguments);
        var t, n, i, r, s = e.getElementsByTagNameNS(NAMESPACE_SVG, "defs");
        if (s.length > 0) {
            s = s[0];
            var o, a = $A(s.getElementsByTagNameNS(NAMESPACE_SVG, "marker")), c = this;
            a.each(function(e) {
                try {
                    o = new ORYX.Core.SVG.SVGMarker(e.cloneNode(!0)), c._markers[o.id] = o;
                    var t, n = $A(o.element.getElementsByTagNameNS(NAMESPACE_SVG, "text"));
                    n.each(function(e) {
                        t = new ORYX.Core.SVG.Label({
                            textElement: e,
                            shapeId: this.id
                        }), c._labels[t.id] = t;
                    });
                } catch (i) {}
            });
        }
        var l = e.getElementsByTagNameNS(NAMESPACE_SVG, "g");
        if (l.length <= 0) throw "Edge: No g element found.";
        var h = l[0];
        h.setAttributeNS(null, "id", null);
        var u = !0;
        $A(h.childNodes).each(function(e, s) {
            if (ORYX.Editor.checkClassType(e, SVGPathElement)) {
                e = e.cloneNode(!1);
                var o = this.id + "_" + s;
                e.setAttributeNS(null, "id", o), this._paths.push(e);
                var a = [], c = e.getAttributeNS(null, "marker-start");
                if (c && "" !== c) {
                    c = c.strip(), c = c.replace(/^url\(#/, "");
                    var l = this.id.concat(c.replace(/\)$/, ""));
                    e.setAttributeNS(null, "marker-start", "url(#" + l + ")"), a.push(this._markers[l]);
                }
                if (c = e.getAttributeNS(null, "marker-mid"), c && "" !== c) {
                    c = c.strip(), c = c.replace(/^url\(#/, "");
                    var h = this.id.concat(c.replace(/\)$/, ""));
                    e.setAttributeNS(null, "marker-mid", "url(#" + h + ")"), a.push(this._markers[h]);
                }
                if (c = e.getAttributeNS(null, "marker-end"), c && "" !== c) {
                    c = c.strip(), c = c.replace(/^url\(#/, "");
                    var d = this.id.concat(c.replace(/\)$/, ""));
                    e.setAttributeNS(null, "marker-end", "url(#" + d + ")"), a.push(this._markers[d]);
                }
                this._markersByPath[o] = a;
                var p = new PathParser(), f = new ORYX.Core.SVG.PointsPathHandler();
                if (p.setHandler(f), p.parsePath(e), f.points.length < 4) throw "Edge: Path has to have two or more points specified.";
                this._dockersByPath[o] = [];
                for (var g = 0; g < f.points.length; g += 2) {
                    var m = f.points[g], O = f.points[g + 1];
                    if (u || g > 0) {
                        var R = new ORYX.Core.Controls.Docker({
                            eventHandlerCallback: this.eventHandlerCallback
                        });
                        R.bounds.centerMoveTo(m, O), R.bounds.registerCallback(this._dockerChangedCallback), 
                        this.add(R, this.dockers.length), t ? (t = Math.min(m, t), n = Math.min(O, n)) : (t = m, 
                        n = O), i ? (i = Math.max(m, i), r = Math.max(O, r)) : (i = m, r = O);
                    }
                }
                u = !1;
            }
        }.bind(this)), this.bounds.set(t, n, i, r), (0 === this.bounds.width() || 0 === this.bounds.height()) && (this.bounds.extend({
            x: 0 === this.bounds.width() ? 2 : 0,
            y: 0 === this.bounds.height() ? 2 : 0
        }), this.bounds.moveBy({
            x: 0 === this.bounds.width() ? -1 : 0,
            y: 0 === this.bounds.height() ? -1 : 0
        })), this._oldBounds = this.bounds.clone(), this._paths.reverse();
        var d = [];
        this._paths.each(function(e) {
            d.push(this.node.childNodes[0].childNodes[0].childNodes[0].appendChild(e));
        }.bind(this)), this._paths = d, this._paths.each(function(e) {
            var t = e.cloneNode(!1);
            t.setAttributeNS(null, "id", void 0), t.setAttributeNS(null, "stroke-width", 10), 
            t.setAttributeNS(null, "visibility", "hidden"), t.setAttributeNS(null, "stroke-dasharray", "none"), 
            t.setAttributeNS(null, "stroke", "black"), t.setAttributeNS(null, "fill", "none"), 
            this._interactionPaths.push(this.node.childNodes[0].childNodes[0].childNodes[0].appendChild(t));
        }.bind(this)), this._paths.reverse(), this._interactionPaths.reverse();
        var p = e.getElementsByTagNameNS(ORYX.CONFIG.NAMESPACE_SVG, "text");
        $A(p).each(function(e) {
            var t = new ORYX.Core.SVG.Label({
                textElement: e,
                shapeId: this.id
            });
            this.node.childNodes[0].childNodes[0].appendChild(t.node), this._labels[t.id] = t;
        }.bind(this)), this.node.childNodes[0].childNodes[0].setAttributeNS(null, "title", this.getStencil().title()), 
        this.propertiesChanged.each(function(e) {
            e.value = !0;
        });
    },
    addMarkers: function(e) {
        this._markers.each(function(t) {
            e.ownerDocument.getElementById(t.value.id) || (t.value.element = e.appendChild(t.value.element));
        });
    },
    removeMarkers: function() {
        var e = this.node.ownerSVGElement;
        if (e) {
            var t = e.getElementsByTagNameNS(NAMESPACE_SVG, "defs");
            t.length > 0 && (t = t[0], this._markers.each(function(e) {
                var n = t.ownerDocument.getElementById(e.value.id);
                n && (e.value.element = t.removeChild(e.value.element));
            }));
        }
    },
    _dockerChanged: function() {
        this._dockerUpdated = !0;
    },
    serialize: function() {
        var e = arguments.callee.$.serialize.apply(this), t = "";
        this._dockersByPath.each(function(e) {
            e.value.each(function(e) {
                var n = e.getDockedShape() && e.referencePoint ? e.referencePoint : e.bounds.center();
                t = t.concat(n.x + " " + n.y + " ");
            }), t += " # ";
        }.bind(this)), e.push({
            name: "dockers",
            prefix: "oryx",
            value: t,
            type: "literal"
        });
        var n = this.dockers.last(), i = n.getDockedShape();
        i && e.push({
            name: "target",
            prefix: "raziel",
            value: "#" + ERDF.__stripHashes(i.resourceId),
            type: "resource"
        });
        try {
            var r = this.getStencil().serialize();
            r.type && (r.shape = this, r.data = e, r.result = void 0, r.forceExecution = !0, 
            this._delegateEvent(r), r.result && (e = r.result));
        } catch (s) {}
        return e;
    },
    deserialize: function(e) {
        try {
            var t = this.getStencil().deserialize();
            t.type && (t.shape = this, t.data = e, t.result = void 0, t.forceExecution = !0, 
            this._delegateEvent(t), t.result && (e = t.result));
        } catch (n) {}
        var i, r = e.find(function(e) {
            return "raziel-target" == e.prefix + "-" + e.name;
        });
        r && (i = this.getCanvas().getChildShapeByResourceId(r.value));
        var s = e.findAll(function(e) {
            return "raziel-outgoing" == e.prefix + "-" + e.name;
        });
        s.each(function(e) {
            if (this.parent) {
                var t = this.getCanvas().getChildShapeByResourceId(e.value);
                t && (t == i ? (this.dockers.last().setDockedShape(t), this.dockers.last().setReferencePoint({
                    x: t.bounds.width() / 2,
                    y: t.bounds.height() / 2
                })) : t instanceof ORYX.Core.Edge && t.dockers.first().setDockedShape(this));
            }
        }.bind(this)), arguments.callee.$.deserialize.apply(this, [ e ]);
        var o = e.find(function(e) {
            return "oryx" === e.prefix && "dockers" === e.name;
        });
        if (o) {
            var a = o.value.split("#").without("").without(" ");
            a.each(function(e, t) {
                var n = e.replace(/,/g, " ").split(" ").without("");
                if (0 === n.length % 2) {
                    var i = this._paths[t];
                    if (i) {
                        if (0 === t) for (;this._dockersByPath[i.id].length > 2; ) this.removeDocker(this._dockersByPath[i.id][1]); else for (;this._dockersByPath[i.id].length > 1; ) this.removeDocker(this._dockersByPath[i.id][0]);
                        var r = this._dockersByPath[i.id];
                        if (0 === t) {
                            var s = parseFloat(n.shift()), o = parseFloat(n.shift());
                            r.first().getDockedShape() ? r.first().setReferencePoint({
                                x: s,
                                y: o
                            }) : r.first().bounds.centerMoveTo(s, o);
                        }
                        o = parseFloat(n.pop()), s = parseFloat(n.pop()), r.last().getDockedShape() ? r.last().setReferencePoint({
                            x: s,
                            y: o
                        }) : r.last().bounds.centerMoveTo(s, o);
                        for (var a = 0; a < n.length; a++) {
                            s = parseFloat(n[a]), o = parseFloat(n[++a]);
                            var c = this.createDocker();
                            c.bounds.centerMoveTo(s, o);
                        }
                    }
                }
            }.bind(this));
        } else this.alignDockers();
        this._changed();
    },
    toString: function() {
        return this.getStencil().title() + " " + this.id;
    },
    getTarget: function() {
        return this.dockers.last() ? this.dockers.last().getDockedShape() : null;
    },
    getSource: function() {
        return this.dockers.first() ? this.dockers.first().getDockedShape() : null;
    },
    isDocked: function() {
        var e = !1;
        return this.dockers.each(function(t) {
            if (t.isDocked()) throw e = !0, $break;
        }), e;
    },
    toJSON: function() {
        var e = arguments.callee.$.toJSON.apply(this, arguments);
        return this.getTarget() && (e.target = {
            resourceId: this.getTarget().resourceId
        }), e;
    }
}, ORYX.Core.Edge = ORYX.Core.Shape.extend(ORYX.Core.Edge), !ORYX) var ORYX = {};

if (ORYX.Plugins || (ORYX.Plugins = {}), ORYX.Plugins.AbstractPlugin = Clazz.extend({
    facade: null,
    construct: function(e) {
        this.facade = e, this.facade.registerOnEvent(ORYX.CONFIG.EVENT_LOADED, this.onLoaded.bind(this));
    },
    onLoaded: function() {},
    onSelectionChanged: function() {},
    showOverlay: function(e, t, n, i) {
        e instanceof Array || (e = [ e ]), e = e.map(function(e) {
            var t = e;
            return "string" == typeof e && (t = this.facade.getCanvas().getChildShapeByResourceId(e), 
            t = t || this.facade.getCanvas().getChildById(e, !0)), t;
        }.bind(this)).compact(), this.overlayID || (this.overlayID = this.type + ORYX.Editor.provideId()), 
        this.facade.raiseEvent({
            type: ORYX.CONFIG.EVENT_OVERLAY_SHOW,
            id: this.overlayID,
            shapes: e,
            attributes: t,
            node: n,
            nodePosition: i || "NW"
        });
    },
    hideOverlay: function() {
        this.facade.raiseEvent({
            type: ORYX.CONFIG.EVENT_OVERLAY_HIDE,
            id: this.overlayID
        });
    },
    doTransform: function(e, t) {
        if (!t || !e) return "";
        var n = new DOMParser(), i = n.parseFromString(e, "text/xml");
        source = t, new Ajax.Request(source, {
            asynchronous: !1,
            method: "get",
            onSuccess: function(e) {
                xsl = e.responseText;
            }.bind(this),
            onFailure: function(e) {
                ORYX.Log.error("XSL load failed" + e);
            }.bind(this)
        });
        var r = new XSLTProcessor(), s = new DOMParser(), o = s.parseFromString(xsl, "text/xml");
        r.importStylesheet(o);
        try {
            var a = r.transformToFragment(i, document), c = new XMLSerializer().serializeToString(a);
            return c = c.startsWith("<?xml") ? c : '<?xml version="1.0" encoding="UTF-8"?>' + c;
        } catch (l) {
            return -1;
        }
    },
    openXMLWindow: function(e) {
        window.open("data:application/xml," + encodeURIComponent(e), "_blank", "resizable=yes,width=600,height=600,toolbar=0,scrollbars=yes");
    },
    openDownloadWindow: function(e, t) {
        var n = window.open("");
        if (null != n) {
            n.document.open(), n.document.write("<html><body>");
            var i = n.document.createElement("form");
            n.document.body.appendChild(i);
            var r = function(e, t) {
                var n = document.createElement("input");
                return n.name = e, n.type = "hidden", n.value = t, n;
            };
            i.appendChild(r("download", t)), i.appendChild(r("file", e)), i.method = "POST", 
            n.document.write("</body></html>"), n.document.close(), i.action = ORYX.PATH + "/download", 
            i.submit();
        }
    },
    getSerializedDOM: function() {
        var e = DataManager.serializeDOM(this.facade);
        return e = '<?xml version="1.0" encoding="utf-8"?><html xmlns="http://www.w3.org/1999/xhtml" xmlns:b3mn="http://b3mn.org/2007/b3mn" xmlns:ext="http://b3mn.org/2007/ext" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:atom="http://b3mn.org/2007/atom+xhtml"><head profile="http://purl.org/NET/erdf/profile"><link rel="schema.dc" href="http://purl.org/dc/elements/1.1/" /><link rel="schema.dcTerms" href="http://purl.org/dc/terms/ " /><link rel="schema.b3mn" href="http://b3mn.org" /><link rel="schema.oryx" href="http://oryx-editor.org/" /><link rel="schema.raziel" href="http://raziel.org/" /><base href="' + location.href.split("?")[0] + '" />' + "</head><body>" + e + "</body></html>";
    },
    enableReadOnlyMode: function() {
        this.facade.disableEvent(ORYX.CONFIG.EVENT_MOUSEDOWN), this._stopSelectionChange = function() {
            this.facade.getSelection().length > 0 && this.facade.setSelection([]);
        }, this.facade.registerOnEvent(ORYX.CONFIG.EVENT_SELECTION_CHANGED, this._stopSelectionChange.bind(this));
    },
    disableReadOnlyMode: function() {
        this.facade.enableEvent(ORYX.CONFIG.EVENT_MOUSEDOWN), this._stopSelectionChange && (this.facade.unregisterOnEvent(ORYX.CONFIG.EVENT_SELECTION_CHANGED, this._stopSelectionChange.bind(this)), 
        this._stopSelectionChange = void 0);
    },
    getRDFFromDOM: function() {
        try {
            var e = "";
            source = ORYX.PATH + "lib/extract-rdf.xsl", new Ajax.Request(source, {
                asynchronous: !1,
                method: "get",
                onSuccess: function(t) {
                    e = t.responseText;
                }.bind(this),
                onFailure: function(e) {
                    ORYX.Log.error("XSL load failed" + e);
                }.bind(this)
            });
            var t = new DOMParser(), n = t.parseFromString(this.getSerializedDOM(), "text/xml"), i = t.parseFromString(e, "text/xml"), r = new XSLTProcessor();
            r.importStylesheet(i);
            var s = r.transformToFragment(n, document), o = new XMLSerializer();
            return o.serializeToString(s);
        } catch (a) {
            return this.facade.raiseEvent({
                type: ORYX.CONFIG.EVENT_NOTIFICATION_SHOW,
                ntype: "error",
                msg: "Error: " + error,
                title: ""
            }), "";
        }
    },
    isStencilSetExtensionLoaded: function(e) {
        return this.facade.getStencilSets().values().any(function(t) {
            return t.extensions().keys().any(function(t) {
                return t == e;
            }.bind(this));
        }.bind(this));
    },
    doLayout: function(e) {
        this.facade.raiseEvent({
            type: ORYX.CONFIG.EVENT_LAYOUT,
            shapes: e
        });
    },
    layoutEdges: function(e, t, n) {
        var i = t.findAll(function(e) {
            return e.dockers.length > 2;
        }.bind(this));
        if (i.length > 0) {
            var r = e.absoluteXY(), s = {
                x: r.x - n.x,
                y: r.y - n.y
            };
            r.x += e.bounds.width() / 2, r.y += e.bounds.height() / 2, oldCenter = Object.clone(r), 
            oldCenter.x -= n ? n.x : 0, oldCenter.y -= n ? n.y : 0, {
                x: r.x - e.bounds.width() / 2,
                y: r.y - e.bounds.height() / 2
            }, {
                x: r.x + e.bounds.width() / 2,
                y: r.y + e.bounds.height() / 2
            };
            var o = function(e, t) {
                var i = e.center().x - t.center().x, r = e.center().y - t.center().y;
                Math.abs(i) < 3 ? e.moveBy({
                    x: (n.xs ? n.xs * (e.center().x - s.x) + n.x + s.x - e.center().x : n.x) - i,
                    y: 0
                }) : Math.abs(r) < 3 && e.moveBy({
                    x: 0,
                    y: (n.ys ? n.ys * (e.center().y - s.y) + n.y + s.y - e.center().y : n.y) - r
                });
            }, a = function(e) {
                var t = e.dockers.first().getDockedShape(), n = e.dockers.last().getDockedShape();
                return t && (t = t.absoluteBounds(), t.widen(5)), n && (n = n.absoluteBounds(), 
                n.widen(20)), e.dockers.any(function(i, r) {
                    var s = i.bounds.center();
                    return 0 != r && r != e.dockers.length - 1 && (t && t.isIncluded(s) || n && n.isIncluded(s));
                });
            };
            i.each(function(t) {
                if (t.dockers.first().getDockedShape() === e) {
                    var n = t.dockers[1];
                    o(n.bounds, t.dockers.first().bounds) && n.update();
                } else if (t.dockers.last().getDockedShape() === e) {
                    var i = t.dockers[t.dockers.length - 2];
                    o(i.bounds, t.dockers.last().bounds) && i.update();
                }
                return t._update(!0), t.removeUnusedDockers(), a(t) ? (this.doLayout(t), void 0) : void 0;
            }.bind(this));
        }
        t.each(function(e) {
            if (2 == e.dockers.length) {
                var t = e.dockers.first().bounds.center(), n = e.dockers.last().bounds.center();
                (Math.abs(t.x - n.x) < 2 || Math.abs(t.y - n.y) < 2) && (e.dockers.first().update(), 
                e.dockers.last().update(), this.doLayout(e));
            }
        }.bind(this));
    }
}), !ORYX) var ORYX = {};

if (ORYX.Plugins || (ORYX.Plugins = {}), ORYX.Plugins.AbstractLayouter = ORYX.Plugins.AbstractPlugin.extend({
    layouted: [],
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.facade.registerOnEvent(ORYX.CONFIG.EVENT_LAYOUT, this._initLayout.bind(this));
    },
    isIncludedInLayout: function(e) {
        return this.layouted instanceof Array || (this.layouted = [ this.layouted ].compact()), 
        this.layouted.length <= 0 ? !0 : this.layouted.any(function(t) {
            return "string" == typeof t ? e.getStencil().id().include(t) : e instanceof t;
        });
    },
    _initLayout: function(e) {
        var t = [ e.shapes ].flatten().compact(), n = t.findAll(function(e) {
            return this.isIncludedInLayout(e);
        }.bind(this));
        n.length > 0 && this.layout(n);
    },
    layout: function() {
        throw new Error("Layouter has to implement the layout function.");
    },
    getChildShapesWithout: function(e, t) {
        if ("function" == typeof e.getChildShapes) {
            var n = e.getChildShapes(!1);
            return n.findAll(function(e) {
                return !t.member(e.getStencil().id());
            });
        }
        return [];
    }
}), !ORYX) var ORYX = {};

ORYX.Plugins || (ORYX.Plugins = {}), ORYX.Plugins.AbstractDragTracker = ORYX.Plugins.AbstractPlugin.extend({
    shapes: [ null ],
    construct: function() {
        arguments.callee.$.construct.apply(this, arguments), this.facade.registerOnEvent(ORYX.CONFIG.EVENT_DRAG_TRACKER_DRAG, function(e) {
            this.isIncludedInShapes(e.shapes) && this.drag(e.shapes, e.bounds);
        }.bind(this)), this.facade.registerOnEvent(ORYX.CONFIG.EVENT_DRAG_TRACKER_RESIZE, function(e) {
            this.isIncludedInShapes(e.shapes) && this.resize(e.shapes, e.bounds);
        }.bind(this)), this.facade.registerOnEvent(ORYX.CONFIG.EVENT_RESIZE_END, function(e) {
            this.isIncludedInShapes(e.shapes) && this.resizeEnd(e.shapes);
        }.bind(this)), this.facade.registerOnEvent(ORYX.CONFIG.EVENT_DROP_SHAPE, function(e) {
            this.isIncludedInShapes(e.shape) && this.newShape(e.shape);
        }.bind(this));
    },
    drag: function() {},
    resize: function() {},
    resizeEnd: function() {},
    newShape: function() {},
    isIncludedInShapes: function(e) {
        return e instanceof Array ? (included = !1, e.each(function(e) {
            return this.isIncludedInShapes(e) ? (included = !0, void 0) : void 0;
        }.bind(this)), included) : (this.shapes instanceof Array || (this.shapes = [ this.shapes ].compact()), 
        this.shapes.length <= 0 ? !0 : this.shapes.any(function(t) {
            return "string" == typeof t ? e.getStencil().id().include(t) : e instanceof t;
        }));
    }
}), function() {
    function e() {
        this.Diff_Timeout = 1, this.Diff_EditCost = 4, this.Match_Threshold = .5, this.Match_Distance = 1e3, 
        this.Patch_DeleteThreshold = .5, this.Patch_Margin = 4, this.Match_MaxBits = 32;
    }
    e.prototype.diff_main = function(e, t, n, i) {
        if ("undefined" == typeof i && (i = 0 >= this.Diff_Timeout ? Number.MAX_VALUE : new Date().getTime() + 1e3 * this.Diff_Timeout), 
        null == e || null == t) throw Error("Null input. (diff_main)");
        if (e == t) return e ? [ [ 0, e ] ] : [];
        "undefined" == typeof n && (n = !0);
        var r = n, s = this.diff_commonPrefix(e, t), n = e.substring(0, s), e = e.substring(s), t = t.substring(s), s = this.diff_commonSuffix(e, t), o = e.substring(e.length - s), e = e.substring(0, e.length - s), t = t.substring(0, t.length - s), e = this.diff_compute_(e, t, r, i);
        return n && e.unshift([ 0, n ]), o && e.push([ 0, o ]), this.diff_cleanupMerge(e), 
        e;
    }, e.prototype.diff_compute_ = function(e, t, n, i) {
        if (!e) return [ [ 1, t ] ];
        if (!t) return [ [ -1, e ] ];
        var r = e.length > t.length ? e : t, s = e.length > t.length ? t : e, o = r.indexOf(s);
        return -1 != o ? (n = [ [ 1, r.substring(0, o) ], [ 0, s ], [ 1, r.substring(o + s.length) ] ], 
        e.length > t.length && (n[0][0] = n[2][0] = -1), n) : 1 == s.length ? [ [ -1, e ], [ 1, t ] ] : (r = this.diff_halfMatch_(e, t)) ? (s = r[0], 
        e = r[1], o = r[2], t = r[3], r = r[4], s = this.diff_main(s, o, n, i), n = this.diff_main(e, t, n, i), 
        s.concat([ [ 0, r ] ], n)) : n && 100 < e.length && 100 < t.length ? this.diff_lineMode_(e, t, i) : this.diff_bisect_(e, t, i);
    }, e.prototype.diff_lineMode_ = function(e, t, n) {
        var i = this.diff_linesToChars_(e, t), e = i.chars1, t = i.chars2, i = i.lineArray, e = this.diff_main(e, t, !1, n);
        this.diff_charsToLines_(e, i), this.diff_cleanupSemantic(e), e.push([ 0, "" ]);
        for (var r = i = t = 0, s = "", o = ""; t < e.length; ) {
            switch (e[t][0]) {
              case 1:
                r++, o += e[t][1];
                break;

              case -1:
                i++, s += e[t][1];
                break;

              case 0:
                if (i >= 1 && r >= 1) {
                    for (e.splice(t - i - r, i + r), t = t - i - r, i = this.diff_main(s, o, !1, n), 
                    r = i.length - 1; r >= 0; r--) e.splice(t, 0, i[r]);
                    t += i.length;
                }
                i = r = 0, o = s = "";
            }
            t++;
        }
        return e.pop(), e;
    }, e.prototype.diff_bisect_ = function(e, t, n) {
        for (var i = e.length, r = t.length, s = Math.ceil((i + r) / 2), o = s, a = 2 * s, c = Array(a), l = Array(a), h = 0; a > h; h++) c[h] = -1, 
        l[h] = -1;
        c[o + 1] = 0, l[o + 1] = 0;
        for (var h = i - r, u = 0 != h % 2, d = 0, p = 0, f = 0, g = 0, m = 0; s > m && !(new Date().getTime() > n); m++) {
            for (var O = -m + d; m - p >= O; O += 2) {
                var R, E = o + O;
                R = O == -m || O != m && c[E - 1] < c[E + 1] ? c[E + 1] : c[E - 1] + 1;
                for (var S = R - O; i > R && r > S && e.charAt(R) == t.charAt(S); ) R++, S++;
                if (c[E] = R, R > i) p += 2; else if (S > r) d += 2; else if (u && (E = o + h - O, 
                E >= 0 && a > E && -1 != l[E])) {
                    var N = i - l[E];
                    if (R >= N) return this.diff_bisectSplit_(e, t, R, S, n);
                }
            }
            for (O = -m + f; m - g >= O; O += 2) {
                for (E = o + O, N = O == -m || O != m && l[E - 1] < l[E + 1] ? l[E + 1] : l[E - 1] + 1, 
                R = N - O; i > N && r > R && e.charAt(i - N - 1) == t.charAt(r - R - 1); ) N++, 
                R++;
                if (l[E] = N, N > i) g += 2; else if (R > r) f += 2; else if (!u && (E = o + h - O, 
                E >= 0 && a > E && -1 != c[E] && (R = c[E], S = o + R - E, N = i - N, R >= N))) return this.diff_bisectSplit_(e, t, R, S, n);
            }
        }
        return [ [ -1, e ], [ 1, t ] ];
    }, e.prototype.diff_bisectSplit_ = function(e, t, n, i, r) {
        var s = e.substring(0, n), o = t.substring(0, i), e = e.substring(n), t = t.substring(i), s = this.diff_main(s, o, !1, r), r = this.diff_main(e, t, !1, r);
        return s.concat(r);
    }, e.prototype.diff_linesToChars_ = function(e, t) {
        function n(e) {
            for (var t = "", n = 0, s = -1, o = i.length; s < e.length - 1; ) {
                s = e.indexOf("\n", n), -1 == s && (s = e.length - 1);
                var a = e.substring(n, s + 1), n = s + 1;
                (r.hasOwnProperty ? r.hasOwnProperty(a) : void 0 !== r[a]) ? t += String.fromCharCode(r[a]) : (t += String.fromCharCode(o), 
                r[a] = o, i[o++] = a);
            }
            return t;
        }
        var i = [], r = {};
        i[0] = "";
        var s = n(e), o = n(t);
        return {
            chars1: s,
            chars2: o,
            lineArray: i
        };
    }, e.prototype.diff_charsToLines_ = function(e, t) {
        for (var n = 0; n < e.length; n++) {
            for (var i = e[n][1], r = [], s = 0; s < i.length; s++) r[s] = t[i.charCodeAt(s)];
            e[n][1] = r.join("");
        }
    }, e.prototype.diff_commonPrefix = function(e, t) {
        if (!e || !t || e.charAt(0) != t.charAt(0)) return 0;
        for (var n = 0, i = Math.min(e.length, t.length), r = i, s = 0; r > n; ) e.substring(s, r) == t.substring(s, r) ? s = n = r : i = r, 
        r = Math.floor((i - n) / 2 + n);
        return r;
    }, e.prototype.diff_commonSuffix = function(e, t) {
        if (!e || !t || e.charAt(e.length - 1) != t.charAt(t.length - 1)) return 0;
        for (var n = 0, i = Math.min(e.length, t.length), r = i, s = 0; r > n; ) e.substring(e.length - r, e.length - s) == t.substring(t.length - r, t.length - s) ? s = n = r : i = r, 
        r = Math.floor((i - n) / 2 + n);
        return r;
    }, e.prototype.diff_commonOverlap_ = function(e, t) {
        var n = e.length, i = t.length;
        if (0 == n || 0 == i) return 0;
        if (n > i ? e = e.substring(n - i) : i > n && (t = t.substring(0, n)), n = Math.min(n, i), 
        e == t) return n;
        for (var i = 0, r = 1; ;) {
            var s = e.substring(n - r), s = t.indexOf(s);
            if (-1 == s) return i;
            r += s, (0 == s || e.substring(n - r) == t.substring(0, r)) && (i = r, r++);
        }
    }, e.prototype.diff_halfMatch_ = function(e, t) {
        function n(e, t, n) {
            for (var i, r, s, a, c = e.substring(n, n + Math.floor(e.length / 4)), l = -1, h = ""; -1 != (l = t.indexOf(c, l + 1)); ) {
                var u = o.diff_commonPrefix(e.substring(n), t.substring(l)), d = o.diff_commonSuffix(e.substring(0, n), t.substring(0, l));
                h.length < d + u && (h = t.substring(l - d, l) + t.substring(l, l + u), i = e.substring(0, n - d), 
                r = e.substring(n + u), s = t.substring(0, l - d), a = t.substring(l + u));
            }
            return 2 * h.length >= e.length ? [ i, r, s, a, h ] : null;
        }
        if (0 >= this.Diff_Timeout) return null;
        var i = e.length > t.length ? e : t, r = e.length > t.length ? t : e;
        if (4 > i.length || 2 * r.length < i.length) return null;
        var s, o = this, a = n(i, r, Math.ceil(i.length / 4)), i = n(i, r, Math.ceil(i.length / 2));
        if (!a && !i) return null;
        s = i ? a ? a[4].length > i[4].length ? a : i : i : a;
        var c;
        return e.length > t.length ? (a = s[0], i = s[1], r = s[2], c = s[3]) : (r = s[0], 
        c = s[1], a = s[2], i = s[3]), s = s[4], [ a, i, r, c, s ];
    }, e.prototype.diff_cleanupSemantic = function(e) {
        for (var t = !1, n = [], i = 0, r = null, s = 0, o = 0, a = 0, c = 0, l = 0; s < e.length; ) 0 == e[s][0] ? (n[i++] = s, 
        o = c, a = l, l = c = 0, r = e[s][1]) : (1 == e[s][0] ? c += e[s][1].length : l += e[s][1].length, 
        r && r.length <= Math.max(o, a) && r.length <= Math.max(c, l) && (e.splice(n[i - 1], 0, [ -1, r ]), 
        e[n[i - 1] + 1][0] = 1, i--, i--, s = i > 0 ? n[i - 1] : -1, l = c = a = o = 0, 
        r = null, t = !0)), s++;
        for (t && this.diff_cleanupMerge(e), this.diff_cleanupSemanticLossless(e), s = 1; s < e.length; ) -1 == e[s - 1][0] && 1 == e[s][0] && (t = e[s - 1][1], 
        n = e[s][1], i = this.diff_commonOverlap_(t, n), r = this.diff_commonOverlap_(n, t), 
        i >= r ? (i >= t.length / 2 || i >= n.length / 2) && (e.splice(s, 0, [ 0, n.substring(0, i) ]), 
        e[s - 1][1] = t.substring(0, t.length - i), e[s + 1][1] = n.substring(i), s++) : (r >= t.length / 2 || r >= n.length / 2) && (e.splice(s, 0, [ 0, t.substring(0, r) ]), 
        e[s - 1][0] = 1, e[s - 1][1] = n.substring(0, n.length - r), e[s + 1][0] = -1, e[s + 1][1] = t.substring(r), 
        s++), s++), s++;
    }, e.prototype.diff_cleanupSemanticLossless = function(t) {
        function n(t, n) {
            if (!t || !n) return 6;
            var i = t.charAt(t.length - 1), r = n.charAt(0), s = i.match(e.nonAlphaNumericRegex_), o = r.match(e.nonAlphaNumericRegex_), a = s && i.match(e.whitespaceRegex_), c = o && r.match(e.whitespaceRegex_), i = a && i.match(e.linebreakRegex_), r = c && r.match(e.linebreakRegex_), l = i && t.match(e.blanklineEndRegex_), h = r && n.match(e.blanklineStartRegex_);
            return l || h ? 5 : i || r ? 4 : s && !a && c ? 3 : a || c ? 2 : s || o ? 1 : 0;
        }
        for (var i = 1; i < t.length - 1; ) {
            if (0 == t[i - 1][0] && 0 == t[i + 1][0]) {
                var r = t[i - 1][1], s = t[i][1], o = t[i + 1][1], a = this.diff_commonSuffix(r, s);
                if (a) var c = s.substring(s.length - a), r = r.substring(0, r.length - a), s = c + s.substring(0, s.length - a), o = c + o;
                for (var a = r, c = s, l = o, h = n(r, s) + n(s, o); s.charAt(0) === o.charAt(0); ) {
                    var r = r + s.charAt(0), s = s.substring(1) + o.charAt(0), o = o.substring(1), u = n(r, s) + n(s, o);
                    u >= h && (h = u, a = r, c = s, l = o);
                }
                t[i - 1][1] != a && (a ? t[i - 1][1] = a : (t.splice(i - 1, 1), i--), t[i][1] = c, 
                l ? t[i + 1][1] = l : (t.splice(i + 1, 1), i--));
            }
            i++;
        }
    }, e.nonAlphaNumericRegex_ = /[^a-zA-Z0-9]/, e.whitespaceRegex_ = /\s/, e.linebreakRegex_ = /[\r\n]/, 
    e.blanklineEndRegex_ = /\n\r?\n$/, e.blanklineStartRegex_ = /^\r?\n\r?\n/, e.prototype.diff_cleanupEfficiency = function(e) {
        for (var t = !1, n = [], i = 0, r = null, s = 0, o = !1, a = !1, c = !1, l = !1; s < e.length; ) 0 == e[s][0] ? (e[s][1].length < this.Diff_EditCost && (c || l) ? (n[i++] = s, 
        o = c, a = l, r = e[s][1]) : (i = 0, r = null), c = l = !1) : (-1 == e[s][0] ? l = !0 : c = !0, 
        r && (o && a && c && l || r.length < this.Diff_EditCost / 2 && 3 == o + a + c + l) && (e.splice(n[i - 1], 0, [ -1, r ]), 
        e[n[i - 1] + 1][0] = 1, i--, r = null, o && a ? (c = l = !0, i = 0) : (i--, s = i > 0 ? n[i - 1] : -1, 
        c = l = !1), t = !0)), s++;
        t && this.diff_cleanupMerge(e);
    }, e.prototype.diff_cleanupMerge = function(e) {
        e.push([ 0, "" ]);
        for (var t, n = 0, i = 0, r = 0, s = "", o = ""; n < e.length; ) switch (e[n][0]) {
          case 1:
            r++, o += e[n][1], n++;
            break;

          case -1:
            i++, s += e[n][1], n++;
            break;

          case 0:
            i + r > 1 ? (0 !== i && 0 !== r && (t = this.diff_commonPrefix(o, s), 0 !== t && (n - i - r > 0 && 0 == e[n - i - r - 1][0] ? e[n - i - r - 1][1] += o.substring(0, t) : (e.splice(0, 0, [ 0, o.substring(0, t) ]), 
            n++), o = o.substring(t), s = s.substring(t)), t = this.diff_commonSuffix(o, s), 
            0 !== t && (e[n][1] = o.substring(o.length - t) + e[n][1], o = o.substring(0, o.length - t), 
            s = s.substring(0, s.length - t))), 0 === i ? e.splice(n - r, i + r, [ 1, o ]) : 0 === r ? e.splice(n - i, i + r, [ -1, s ]) : e.splice(n - i - r, i + r, [ -1, s ], [ 1, o ]), 
            n = n - i - r + (i ? 1 : 0) + (r ? 1 : 0) + 1) : 0 !== n && 0 == e[n - 1][0] ? (e[n - 1][1] += e[n][1], 
            e.splice(n, 1)) : n++, i = r = 0, o = s = "";
        }
        for ("" === e[e.length - 1][1] && e.pop(), i = !1, n = 1; n < e.length - 1; ) 0 == e[n - 1][0] && 0 == e[n + 1][0] && (e[n][1].substring(e[n][1].length - e[n - 1][1].length) == e[n - 1][1] ? (e[n][1] = e[n - 1][1] + e[n][1].substring(0, e[n][1].length - e[n - 1][1].length), 
        e[n + 1][1] = e[n - 1][1] + e[n + 1][1], e.splice(n - 1, 1), i = !0) : e[n][1].substring(0, e[n + 1][1].length) == e[n + 1][1] && (e[n - 1][1] += e[n + 1][1], 
        e[n][1] = e[n][1].substring(e[n + 1][1].length) + e[n + 1][1], e.splice(n + 1, 1), 
        i = !0)), n++;
        i && this.diff_cleanupMerge(e);
    }, e.prototype.diff_xIndex = function(e, t) {
        var n, i = 0, r = 0, s = 0, o = 0;
        for (n = 0; n < e.length && (1 !== e[n][0] && (i += e[n][1].length), -1 !== e[n][0] && (r += e[n][1].length), 
        !(i > t)); n++) s = i, o = r;
        return e.length != n && -1 === e[n][0] ? o : o + (t - s);
    }, e.prototype.diff_prettyHtml = function(e) {
        for (var t = [], n = /&/g, i = /</g, r = />/g, s = /\n/g, o = 0; o < e.length; o++) {
            var a = e[o][0], c = e[o][1], c = c.replace(n, "&amp;").replace(i, "&lt;").replace(r, "&gt;").replace(s, "&para;<br>");
            switch (a) {
              case 1:
                t[o] = '<ins style="background:#e6ffe6;">' + c + "</ins>";
                break;

              case -1:
                t[o] = '<del style="background:#ffe6e6;">' + c + "</del>";
                break;

              case 0:
                t[o] = "<span>" + c + "</span>";
            }
        }
        return t.join("");
    }, e.prototype.diff_text1 = function(e) {
        for (var t = [], n = 0; n < e.length; n++) 1 !== e[n][0] && (t[n] = e[n][1]);
        return t.join("");
    }, e.prototype.diff_text2 = function(e) {
        for (var t = [], n = 0; n < e.length; n++) -1 !== e[n][0] && (t[n] = e[n][1]);
        return t.join("");
    }, e.prototype.diff_levenshtein = function(e) {
        for (var t = 0, n = 0, i = 0, r = 0; r < e.length; r++) {
            var s = e[r][0], o = e[r][1];
            switch (s) {
              case 1:
                n += o.length;
                break;

              case -1:
                i += o.length;
                break;

              case 0:
                t += Math.max(n, i), i = n = 0;
            }
        }
        return t += Math.max(n, i);
    }, e.prototype.diff_toDelta = function(e) {
        for (var t = [], n = 0; n < e.length; n++) switch (e[n][0]) {
          case 1:
            t[n] = "+" + encodeURI(e[n][1]);
            break;

          case -1:
            t[n] = "-" + e[n][1].length;
            break;

          case 0:
            t[n] = "=" + e[n][1].length;
        }
        return t.join("	").replace(/%20/g, " ");
    }, e.prototype.diff_fromDelta = function(e, t) {
        for (var n = [], i = 0, r = 0, s = t.split(/\t/g), o = 0; o < s.length; o++) {
            var a = s[o].substring(1);
            switch (s[o].charAt(0)) {
              case "+":
                try {
                    n[i++] = [ 1, decodeURI(a) ];
                } catch (c) {
                    throw Error("Illegal escape in diff_fromDelta: " + a);
                }
                break;

              case "-":
              case "=":
                var l = parseInt(a, 10);
                if (isNaN(l) || 0 > l) throw Error("Invalid number in diff_fromDelta: " + a);
                a = e.substring(r, r += l), n[i++] = "=" == s[o].charAt(0) ? [ 0, a ] : [ -1, a ];
                break;

              default:
                if (s[o]) throw Error("Invalid diff operation in diff_fromDelta: " + s[o]);
            }
        }
        if (r != e.length) throw Error("Delta length (" + r + ") does not equal source text length (" + e.length + ").");
        return n;
    }, e.prototype.match_main = function(e, t, n) {
        if (null == e || null == t || null == n) throw Error("Null input. (match_main)");
        return n = Math.max(0, Math.min(n, e.length)), e == t ? 0 : e.length ? e.substring(n, n + t.length) == t ? n : this.match_bitap_(e, t, n) : -1;
    }, e.prototype.match_bitap_ = function(e, t, n) {
        function i(e, i) {
            var r = e / t.length, o = Math.abs(n - i);
            return s.Match_Distance ? r + o / s.Match_Distance : o ? 1 : r;
        }
        if (t.length > this.Match_MaxBits) throw Error("Pattern too long for this browser.");
        var r = this.match_alphabet_(t), s = this, o = this.Match_Threshold, a = e.indexOf(t, n);
        -1 != a && (o = Math.min(i(0, a), o), a = e.lastIndexOf(t, n + t.length), -1 != a && (o = Math.min(i(0, a), o)));
        for (var c, l, h, u = 1 << t.length - 1, a = -1, d = t.length + e.length, p = 0; p < t.length; p++) {
            for (c = 0, l = d; l > c; ) i(p, n + l) <= o ? c = l : d = l, l = Math.floor((d - c) / 2 + c);
            d = l, c = Math.max(1, n - l + 1);
            var f = Math.min(n + l, e.length) + t.length;
            for (l = Array(f + 2), l[f + 1] = (1 << p) - 1; f >= c; f--) {
                var g = r[e.charAt(f - 1)];
                if (l[f] = 0 === p ? (1 | l[f + 1] << 1) & g : 1 | ((1 | l[f + 1] << 1) & g | (h[f + 1] | h[f]) << 1) | h[f + 1], 
                l[f] & u && (g = i(p, f - 1), o >= g)) {
                    if (o = g, a = f - 1, !(a > n)) break;
                    c = Math.max(1, 2 * n - a);
                }
            }
            if (i(p + 1, n) > o) break;
            h = l;
        }
        return a;
    }, e.prototype.match_alphabet_ = function(e) {
        for (var t = {}, n = 0; n < e.length; n++) t[e.charAt(n)] = 0;
        for (n = 0; n < e.length; n++) t[e.charAt(n)] |= 1 << e.length - n - 1;
        return t;
    }, e.prototype.patch_addContext_ = function(e, t) {
        if (0 != t.length) {
            for (var n = t.substring(e.start2, e.start2 + e.length1), i = 0; t.indexOf(n) != t.lastIndexOf(n) && n.length < this.Match_MaxBits - this.Patch_Margin - this.Patch_Margin; ) i += this.Patch_Margin, 
            n = t.substring(e.start2 - i, e.start2 + e.length1 + i);
            i += this.Patch_Margin, (n = t.substring(e.start2 - i, e.start2)) && e.diffs.unshift([ 0, n ]), 
            (i = t.substring(e.start2 + e.length1, e.start2 + e.length1 + i)) && e.diffs.push([ 0, i ]), 
            e.start1 -= n.length, e.start2 -= n.length, e.length1 += n.length + i.length, e.length2 += n.length + i.length;
        }
    }, e.prototype.patch_make = function(t, n, i) {
        var r;
        if ("string" == typeof t && "string" == typeof n && "undefined" == typeof i) r = t, 
        n = this.diff_main(r, n, !0), 2 < n.length && (this.diff_cleanupSemantic(n), this.diff_cleanupEfficiency(n)); else if (t && "object" == typeof t && "undefined" == typeof n && "undefined" == typeof i) n = t, 
        r = this.diff_text1(n); else if ("string" == typeof t && n && "object" == typeof n && "undefined" == typeof i) r = t; else {
            if ("string" != typeof t || "string" != typeof n || !i || "object" != typeof i) throw Error("Unknown call format to patch_make.");
            r = t, n = i;
        }
        if (0 === n.length) return [];
        for (var i = [], t = new e.patch_obj(), s = 0, o = 0, a = 0, c = r, l = 0; l < n.length; l++) {
            var h = n[l][0], u = n[l][1];
            switch (s || 0 === h || (t.start1 = o, t.start2 = a), h) {
              case 1:
                t.diffs[s++] = n[l], t.length2 += u.length, r = r.substring(0, a) + u + r.substring(a);
                break;

              case -1:
                t.length1 += u.length, t.diffs[s++] = n[l], r = r.substring(0, a) + r.substring(a + u.length);
                break;

              case 0:
                u.length <= 2 * this.Patch_Margin && s && n.length != l + 1 ? (t.diffs[s++] = n[l], 
                t.length1 += u.length, t.length2 += u.length) : u.length >= 2 * this.Patch_Margin && s && (this.patch_addContext_(t, c), 
                i.push(t), t = new e.patch_obj(), s = 0, c = r, o = a);
            }
            1 !== h && (o += u.length), -1 !== h && (a += u.length);
        }
        return s && (this.patch_addContext_(t, c), i.push(t)), i;
    }, e.prototype.patch_deepCopy = function(t) {
        for (var n = [], i = 0; i < t.length; i++) {
            var r = t[i], s = new e.patch_obj();
            s.diffs = [];
            for (var o = 0; o < r.diffs.length; o++) s.diffs[o] = r.diffs[o].slice();
            s.start1 = r.start1, s.start2 = r.start2, s.length1 = r.length1, s.length2 = r.length2, 
            n[i] = s;
        }
        return n;
    }, e.prototype.patch_apply = function(e, t) {
        if (0 == e.length) return [ t, [] ];
        var e = this.patch_deepCopy(e), n = this.patch_addPadding(e), t = n + t + n;
        this.patch_splitMax(e);
        for (var i = 0, r = [], s = 0; s < e.length; s++) {
            var o, a = e[s].start2 + i, c = this.diff_text1(e[s].diffs), l = -1;
            if (c.length > this.Match_MaxBits ? (o = this.match_main(t, c.substring(0, this.Match_MaxBits), a), 
            -1 != o && (l = this.match_main(t, c.substring(c.length - this.Match_MaxBits), a + c.length - this.Match_MaxBits), 
            -1 == l || o >= l) && (o = -1)) : o = this.match_main(t, c, a), -1 == o) r[s] = !1, 
            i -= e[s].length2 - e[s].length1; else if (r[s] = !0, i = o - a, a = -1 == l ? t.substring(o, o + c.length) : t.substring(o, l + this.Match_MaxBits), 
            c == a) t = t.substring(0, o) + this.diff_text2(e[s].diffs) + t.substring(o + c.length); else if (a = this.diff_main(c, a, !1), 
            c.length > this.Match_MaxBits && this.diff_levenshtein(a) / c.length > this.Patch_DeleteThreshold) r[s] = !1; else {
                this.diff_cleanupSemanticLossless(a);
                for (var h, c = 0, l = 0; l < e[s].diffs.length; l++) {
                    var u = e[s].diffs[l];
                    0 !== u[0] && (h = this.diff_xIndex(a, c)), 1 === u[0] ? t = t.substring(0, o + h) + u[1] + t.substring(o + h) : -1 === u[0] && (t = t.substring(0, o + h) + t.substring(o + this.diff_xIndex(a, c + u[1].length))), 
                    -1 !== u[0] && (c += u[1].length);
                }
            }
        }
        return t = t.substring(n.length, t.length - n.length), [ t, r ];
    }, e.prototype.patch_addPadding = function(e) {
        for (var t = this.Patch_Margin, n = "", i = 1; t >= i; i++) n += String.fromCharCode(i);
        for (i = 0; i < e.length; i++) e[i].start1 += t, e[i].start2 += t;
        var i = e[0], r = i.diffs;
        if (0 == r.length || 0 != r[0][0]) r.unshift([ 0, n ]), i.start1 -= t, i.start2 -= t, 
        i.length1 += t, i.length2 += t; else if (t > r[0][1].length) {
            var s = t - r[0][1].length;
            r[0][1] = n.substring(r[0][1].length) + r[0][1], i.start1 -= s, i.start2 -= s, i.length1 += s, 
            i.length2 += s;
        }
        return i = e[e.length - 1], r = i.diffs, 0 == r.length || 0 != r[r.length - 1][0] ? (r.push([ 0, n ]), 
        i.length1 += t, i.length2 += t) : t > r[r.length - 1][1].length && (s = t - r[r.length - 1][1].length, 
        r[r.length - 1][1] += n.substring(0, s), i.length1 += s, i.length2 += s), n;
    }, e.prototype.patch_splitMax = function(t) {
        for (var n = this.Match_MaxBits, i = 0; i < t.length; i++) if (!(t[i].length1 <= n)) {
            var r = t[i];
            t.splice(i--, 1);
            for (var s = r.start1, o = r.start2, a = ""; 0 !== r.diffs.length; ) {
                var c = new e.patch_obj(), l = !0;
                for (c.start1 = s - a.length, c.start2 = o - a.length, "" !== a && (c.length1 = c.length2 = a.length, 
                c.diffs.push([ 0, a ])); 0 !== r.diffs.length && c.length1 < n - this.Patch_Margin; ) {
                    var a = r.diffs[0][0], h = r.diffs[0][1];
                    1 === a ? (c.length2 += h.length, o += h.length, c.diffs.push(r.diffs.shift()), 
                    l = !1) : -1 === a && 1 == c.diffs.length && 0 == c.diffs[0][0] && h.length > 2 * n ? (c.length1 += h.length, 
                    s += h.length, l = !1, c.diffs.push([ a, h ]), r.diffs.shift()) : (h = h.substring(0, n - c.length1 - this.Patch_Margin), 
                    c.length1 += h.length, s += h.length, 0 === a ? (c.length2 += h.length, o += h.length) : l = !1, 
                    c.diffs.push([ a, h ]), h == r.diffs[0][1] ? r.diffs.shift() : r.diffs[0][1] = r.diffs[0][1].substring(h.length));
                }
                a = this.diff_text2(c.diffs), a = a.substring(a.length - this.Patch_Margin), h = this.diff_text1(r.diffs).substring(0, this.Patch_Margin), 
                "" !== h && (c.length1 += h.length, c.length2 += h.length, 0 !== c.diffs.length && 0 === c.diffs[c.diffs.length - 1][0] ? c.diffs[c.diffs.length - 1][1] += h : c.diffs.push([ 0, h ])), 
                l || t.splice(++i, 0, c);
            }
        }
    }, e.prototype.patch_toText = function(e) {
        for (var t = [], n = 0; n < e.length; n++) t[n] = e[n];
        return t.join("");
    }, e.prototype.patch_fromText = function(t) {
        var n = [];
        if (!t) return n;
        for (var t = t.split("\n"), i = 0, r = /^@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@$/; i < t.length; ) {
            var s = t[i].match(r);
            if (!s) throw Error("Invalid patch string: " + t[i]);
            var o = new e.patch_obj();
            for (n.push(o), o.start1 = parseInt(s[1], 10), "" === s[2] ? (o.start1--, o.length1 = 1) : "0" == s[2] ? o.length1 = 0 : (o.start1--, 
            o.length1 = parseInt(s[2], 10)), o.start2 = parseInt(s[3], 10), "" === s[4] ? (o.start2--, 
            o.length2 = 1) : "0" == s[4] ? o.length2 = 0 : (o.start2--, o.length2 = parseInt(s[4], 10)), 
            i++; i < t.length; ) {
                s = t[i].charAt(0);
                try {
                    var a = decodeURI(t[i].substring(1));
                } catch (c) {
                    throw Error("Illegal escape in patch_fromText: " + a);
                }
                if ("-" == s) o.diffs.push([ -1, a ]); else if ("+" == s) o.diffs.push([ 1, a ]); else if (" " == s) o.diffs.push([ 0, a ]); else {
                    if ("@" == s) break;
                    if ("" !== s) throw Error('Invalid patch mode "' + s + '" in: ' + a);
                }
                i++;
            }
        }
        return n;
    }, e.patch_obj = function() {
        this.diffs = [], this.start2 = this.start1 = null, this.length2 = this.length1 = 0;
    }, e.patch_obj.prototype.toString = function() {
        var e, t;
        e = 0 === this.length1 ? this.start1 + ",0" : 1 == this.length1 ? this.start1 + 1 : this.start1 + 1 + "," + this.length1, 
        t = 0 === this.length2 ? this.start2 + ",0" : 1 == this.length2 ? this.start2 + 1 : this.start2 + 1 + "," + this.length2, 
        e = [ "@@ -" + e + " +" + t + " @@\n" ];
        var n;
        for (t = 0; t < this.diffs.length; t++) {
            switch (this.diffs[t][0]) {
              case 1:
                n = "+";
                break;

              case -1:
                n = "-";
                break;

              case 0:
                n = " ";
            }
            e[t + 1] = n + encodeURI(this.diffs[t][1]) + "\n";
        }
        return e.join("").replace(/%20/g, " ");
    }, this.diff_match_patch = e, this.DIFF_DELETE = -1, this.DIFF_INSERT = 1, this.DIFF_EQUAL = 0;
}(), Ext.ns("Extensive.grid"), Extensive.grid.ItemDeleter = Ext.define("Ext.grid.RowSelectionModel", {
    width: 30,
    sortable: !1,
    dataIndex: 0,
    menuDisabled: !0,
    fixed: !0,
    id: "deleter",
    initEvents: function() {
        Extensive.grid.ItemDeleter.superclass.initEvents.call(this), this.grid.on("cellclick", function(e, t, n) {
            if (n == e.getColumnModel().getIndexById("deleter")) {
                var i = e.getStore().getAt(t);
                e.getStore().remove(i), e.getView().refresh();
            }
        });
    },
    renderer: function() {
        return '<div class="extensive-remove" style="width: 15px; height: 16px;"></div>';
    }
}), ImageViewer = Ext.define("Ext.window.Window", {
    initComponent: function() {
        var e = Ext.id();
        ORYX.EDITOR.imagePreviewSVG = this.src, this.bodyCfg = {
            id: "imageviewerid",
            layout: "anchor",
            autoCreate: !0,
            closeAction: "close",
            title: "Image Viewer",
            plain: !0,
            modal: !0,
            collapsible: !1,
            resizeable: !0,
            shadow: !0,
            html: '<iframe id="imageViewFrame" name="imageViewFrame" frameborder="0" scrolling="auto" width="100%" height="400" src="' + ORYX.PATH + "imageview/imageview.html?" + e + '"></iframe>',
            width: 400,
            height: 400,
            autoScroll: !0,
            fixedcenter: !0
        }, ImageViewer.superclass.initComponent.apply(this, arguments);
    },
    onRender: function() {
        ImageViewer.superclass.onRender.apply(this, arguments), this.body.on("load", this.onImageLoad, this, {
            single: !0
        });
    },
    onImageLoad: function() {},
    setSrc: function(e) {
        this.body.on("load", this.onImageLoad, this, {
            single: !0
        }), this.body.dom.src = e;
    },
    initEvents: function() {
        ImageViewer.superclass.initEvents.apply(this, arguments), this.resizer && (this.resizer.preserveRatio = !0);
    }
}), SVGViewer = Ext.define("Ext.window.Window", {
    initComponent: function() {
        var e = Ext.id();
        this.bodyCfg = {
            id: "svgviewerid",
            layout: "anchor",
            autoCreate: !0,
            closeAction: "close",
            title: "SVG Viewer",
            plain: !0,
            modal: !0,
            collapsible: !1,
            resizeable: !0,
            shadow: !0,
            html: '<iframe id="svgViewFrame" name="svgViewFrame" frameborder="0" scrolling="auto" width="100%" height="400" src="' + ORYX.PATH + "localhistory/svgview.html?" + e + '"></iframe>',
            width: 400,
            height: 400,
            autoScroll: !0,
            fixedcenter: !0
        }, SVGViewer.superclass.initComponent.apply(this, arguments);
    },
    onRender: function() {
        SVGViewer.superclass.onRender.apply(this, arguments), this.body.on("load", this.onSVGLoad, this, {
            single: !0
        });
    },
    onSVGLoad: function() {},
    setSrc: function(e) {
        this.body.on("load", this.onSVGLoad, this, {
            single: !0
        }), this.body.dom.src = e;
    },
    initEvents: function() {
        SVGViewer.superclass.initEvents.apply(this, arguments), this.resizer && (this.resizer.preserveRatio = !0);
    }
}), Ext.ns("Oryx.Plugins"), ORYX.Plugins.PanelCollapsedTitlePlugin = function() {
    var e = "x-panel-header-rotated", t = !!document.implementation.hasFeature("http://www.w3.org/TR/SVG11/feature#BasicStructure", "1.1"), n = function() {
        var n = "east" == this.region || "west" == this.region, i = "overflow: visible; padding: 0; border: none; background: none;";
        if (n && t) {
            this.collapsedHeader = this.ownerCt.layout[this.region].getCollapsedEl().createChild({
                tag: "div",
                style: "height: 100%; overflow: hidden;"
            });
            var r = "http://www.w3.org/2000/svg", s = document.createElementNS(r, "svg");
            this.collapsedHeader.dom.appendChild(s), s.setAttribute("width", "100%"), s.setAttribute("height", "100%");
            var o = document.createElementNS(r, "text");
            "west" == this.region ? (o.setAttribute("x", 12), o.setAttribute("y", 100), o.setAttribute("transform", "rotate(270 12 100)")) : (o.setAttribute("x", 6), 
            o.setAttribute("y", 1), o.setAttribute("transform", "rotate(90 6 1)")), o.setAttribute("class", "x-panel-header " + e), 
            s.appendChild(o), this.collapsedHeaderText = document.createTextNode(this.title), 
            o.appendChild(this.collapsedHeaderText);
            var a = Ext.fly(o).getStyle("color");
            o.setAttribute("style", i + ";fill: " + a + ";");
        } else {
            var c = "position: relative;";
            n ? c += "white-space: nowrap; writing-mode: tb-rl; top: 1px; left: 3px;" : (c += "top: 2px;", 
            i += "padding-left: 4px; margin-right: 18px;"), this.collapsedHeader = this.ownerCt.layout[this.region].getCollapsedEl().createChild({
                tag: "div",
                style: i,
                cls: "x-panel-header " + e,
                html: '<span style="' + c + '">' + this.title + "</span>"
            }), this.collapsedHeaderText = this.collapsedHeader.first();
        }
        this.collapsedIconCls && this.setCollapsedIconClass(this.collapsedIconCls);
    };
    return this.init = function(e) {
        e.collapsible && ("east" == e.region || "west" == e.region, e.on("render", function() {
            this.ownerCt.rendered && this.ownerCt.layout.hasLayout ? n.call(e) : this.ownerCt.on("afterlayout", n, e, {
                single: !0
            });
        }, e));
    }, this;
};