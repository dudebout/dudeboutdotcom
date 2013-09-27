PATH := ./bootstrap/node_modules/.bin:${PATH}

LESS_BOOTSTRAP := variables mixins normalize scaffolding grid type component-animations buttons button-groups utilities responsive-utilities responsive-768px-979px responsive-767px-max
LESS_SPECIFIC := override buttons specific floats links publications code
LESS_ALL = $(addprefix bootstrap/less/, $(LESS_BOOTSTRAP)) $(addprefix specific/, $(LESS_SPECIFIC)) font-awesome/less/font-awesome
LESS_MODULES = $(addsuffix .less, $(LESS_ALL))

JS_MODULE_NAMES := collapse transition
JS_MODULES = $(addprefix bootstrap/js/bootstrap-, $(addsuffix .js, $(JS_MODULE_NAMES))) specific/specific.js

LESS = dudeboutdotcom.less
CSS = ../static/css/dudeboutdotcom.css
JS = ../static/js/dudeboutdotcom.js

.PHONY: all lint clean watch

all: $(CSS) $(JS)

$(LESS): $(LESS_MODULES)
	cat $+ > $@

$(CSS): $(LESS)
	recess --compress $< > $@

$(JS): $(JS_MODULES)
	cat $+ > dudeboutdotcom.tmp.js
	uglifyjs -nc dudeboutdotcom.tmp.js > $@
	rm dudeboutdotcom.tmp.js

lint: $(LESS)
	recess $<

clean:
	rm -f $(LESS) $(CSS) $(JS)

#
# WATCH LESS FILES
#
watch: all
	echo "Watching less files..."; \
	watchr -e "watch('specific/.*\.less') { system 'make' }"
