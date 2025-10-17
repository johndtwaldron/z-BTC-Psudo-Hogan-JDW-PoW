.PHONY: lint, up build bank settle final e2e validate clean

lint-sh:
\tshellcheck scripts/*.sh

lint-md:
\tmdl -g -c .mdlrc .

lint-docker:
\thadolint cobol/Dockerfile

up:
\tdocker compose -f compose/docker-compose.yaml up -d

build:
\tbash cobol/build.sh
\tmvn -q -DskipTests package -f java/pom.xml

bank:
\tbash scripts/hbanktrx.sh

settle:
\tbash scripts/hsettle.sh

final:
\tbash scripts/hfinal.sh

validate:
\tbash jcl/validate.sh

e2e: up build validate bank settle final

clean:
\tdocker compose -f compose/docker-compose.yaml down -v || true
\trm -rf cobol/bin || true
