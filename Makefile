install: css build
	stack install

css:
	sass static/screen.scss static/screen.css

build:
	stack build
