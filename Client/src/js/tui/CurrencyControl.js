Ext.ns("itasks.tui");

itasks.tui.CurrencyControl = Ext.extend(Ext.form.TextField,{
	width: 100,
	fieldClass: "x-form-field x-form-num-field",
	style: "text-align: right",
	initEvents: function () {
		this.maskRe = new RegExp('[0123456789\\.]');
		itasks.tui.CurrencyControl.superclass.initEvents.call(this);
	},
	initComponent: function() {
		if(this.staticDisplay){			
			this.autoCreate = {tag: 'div', style: 'overflow: auto; padding-top: 4px', html: this.value};
		}
		
		this.msgTarget = 'side';
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.CurrencyControl.superclass.initComponent.apply(this,arguments);
	},
	onRender: function(ct, position) {
		itasks.tui.CurrencyControl.superclass.onRender.call(this,ct,position);
		//Overlay currency symbol on top of input
		var cl = ct.createChild({tag: 'span', style: 'position: absolute;', cn: this.currencyLabel});
		cl.setLocation(this.getEl().getLeft() + 5, this.getEl().getTop() + 5);
	},
	afterRender: function(){
		itasks.tui.CurrencyControl.superclass.afterRender.call(this,arguments);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
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
		this.setValue(this.normalize(this.getRawValue()));
	},
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.CurrencyControl.superclass.setValue.call(this,value);
		}
		
		if(this.activeError) this.setError(this.activeError);
	},
	setError: function(msg){		
		(function() {
			if(msg == "") this.clearInvalid();
			else this.markInvalid(msg);
		}).defer(50,this);
	},
	setHint: function(msg){
		(function() {
			if(msg == "") itasks.tui.common.clearHint(this);
			else itasks.tui.common.markHint(this,msg);
		}).defer(50,this);
	}
});

Ext.reg("itasks.tui.Currency",itasks.tui.CurrencyControl);