# Hogan-Style Crypto Settlement POC (Bash-only orchestration)

![CI](https://github.com/<your-username>/hogan-crypto-settlement-poc/actions/workflows/ci.yml/badge.svg)

**Goal:** Demonstrate hybrid core-banking flow (COBOL/JCL mindset + MQ + Java + Bitcoin regtest) with modern CI/CD — all runnable locally on Windows (Git Bash/WSL), macOS, and Linux.

## What’s inside
- **COBOL batch** (GnuCOBOL, dockerized) for ledger updates & finalization.
- **Pseudo-JCL**: Real-looking `.JCL` members for readability, plus **Bash job drivers** that execute equivalent steps.
- **Messaging**: IBM MQ (Developer image via Docker). Queues: `TRANX.REQUEST`, `SETTLE.NOTIFY`, `SETTLE.CONFIRM`.
- **Java** (Spring Boot) microservices using IBM MQ JMS:
  - API → validates transfer and MQPUT to `TRANX.REQUEST`
  - Core → MQGET → writes `work.csv` for COBOL
  - Settle → MQGET `SETTLE.NOTIFY` → Bitcoin Core (regtest) JSON-RPC → MQPUT `SETTLE.CONFIRM`
  - Confirm → MQGET → writes `confirm.csv`
- **Bitcoin**: `bitcoind` in **regtest** (Docker). Java JSON-RPC to broadcast and mine blocks for instant confirmations.
- **QA**: Cucumber-JVM BDD and E2E smoke tests.
- **CI**: GitHub Actions (Ubuntu runner) runs Docker, builds COBOL/Java, executes the end-to-end job.

## Run locally

Prereqs: Docker, Git, Java 17+, Maven.  
Windows: run with **Git Bash** or **WSL**.

```bash
# 1) start services (MQ + bitcoind)
docker compose -f compose/docker-compose.yaml up -d

# 2) build COBOL & Java
bash cobol/build.sh
mvn -q -DskipTests package -f java/pom.xml

# 3) optional: validate JCL <-> Bash parity
bash jcl/validate.sh

# 4) run the “jobs”
bash scripts/hbanktrx.sh
bash scripts/hsettle.sh
bash scripts/hfinal.sh
