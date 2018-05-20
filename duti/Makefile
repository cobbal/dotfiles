update-duti: duti
	duti duti

duti: duti-gen.rkt
	racket duti-gen.rkt | tee duti

clean:
	rm -f duti

.PHONY: duti update-duti
