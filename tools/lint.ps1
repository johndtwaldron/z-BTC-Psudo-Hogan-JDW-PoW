param([switch]$Fix)
$ErrorActionPreference = 'Stop'

# ShellCheck
try {
  shellcheck --version | Out-Null
  Write-Host "ShellCheck: running..."
  Get-ChildItem scripts -Filter *.sh | % { & shellcheck $_.FullName }
} catch { Write-Warning "ShellCheck not found. Install with: winget install koalaman.shellcheck" }

# Hadolint
try {
  hadolint --version | Out-Null
  Write-Host "Hadolint: running..."
  hadolint cobol\Dockerfile
} catch { Write-Warning "Hadolint not found. Use the Windows EXE install snippet." }

# Markdownlint (Node CLI)
try {
  markdownlint --version | Out-Null
  Write-Host "markdownlint: running..."
  $args = @(".")
  if (Test-Path .\.markdownlint.json) { $args += @("--config", ".\.markdownlint.json") }
  if ($Fix) { $args += "--fix" }
  & markdownlint @args
} catch { Write-Warning "markdownlint not found. npm i -g markdownlint-cli" }
