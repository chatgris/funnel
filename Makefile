.PHONY: install

install:
	gem install bundler
	bundle install
	mix deps.get

morning:
	desi start
