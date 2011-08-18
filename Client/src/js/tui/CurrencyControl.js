Ext.ns("itasks.tui");

itasks.tui.CurrencyControl = itasks.tui.extendControl(Ext.form.TextField,{
	width: 100,
	height: 25,
	fieldClass: "x-form-field x-form-num-field",
	style: "text-align: right",
	maskRe: new RegExp('[0123456789\\.]'),
	afterRender: function(ct){
		this.extSuperclass.afterRender.apply(this,arguments);
		
		this.currencyEl = ct.createChild({tag: 'span', cn: this.currencyLabel});
		this.currencyEl.alignTo(this.getEl(),'tl',[5,5]);
	},
	setValue: function(v) {
		if (Ext.isArray(v)) {
			this.extSuperclass.setValue.call(this,v[1]/100);
		} else {
			this.extSuperclass.setValue.call(this,v);
		}
		this.determineCurrencyLabel(v);
		if (this.currencyEl)
			this.currencyEl.update(this.currencyLabel);
	},
	normalize: function(s) {
		if(s == "")
			return s;
		
		var padl = function(x) {
			return (x.length == 0) ? "0" : x;
		}
		var padr = function(x) {
			if(x.length == 0) return "00";
			if(x.length == 1) return x + "0";
			return x[0] + x[1];
		}
		
		var parts = s.split('.');
		
		if(parts.length == 1)
			return s + ".00";
		else
			return padl(parts[0]) + "." + padr(parts[1]);
		
	},
	beforeBlur: function() {
		this.extSuperclass.setValue.call(this,this.normalize(this.getRawValue()));
	},
	determineCurrencyLabel: function(v) {
		if (Ext.isArray(v)) {
			switch (v[0]) {
				case 'EUR':
					this.currencyLabel = "&euro;";
					break;
				case 'GBP':
					this.currencyLabel = "&pound;";
					break;
				case 'USD':
					this.currencyLabel = "$";
					break;
				case 'JPY':
					this.currencyLabel = "&yen;"
					break;
			}
		} else {
			this.currencyLabel = "&euro;"; //Use the default currency
		}
	}
});

Ext.reg("itasks.tui.Currency",itasks.tui.CurrencyControl);