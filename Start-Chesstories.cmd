@echo off
setlocal
cd /d "%~dp0chesstory-web"
where node >nul 2>nul
if errorlevel 1 goto missing_node
where java >nul 2>nul
if errorlevel 1 goto missing_java
if not exist "node_modules\esbuild\package.json" (
  call npm ci
  if errorlevel 1 goto failed
)
if not exist "..\chesstory-runtime\target\runtime-classpath.txt" (
  call npm run build:runtime
  if errorlevel 1 goto failed
)
if not exist "dist\index.html" (
  call npm run build
  if errorlevel 1 goto failed
)
node scripts\serve.mjs
if errorlevel 1 goto failed
exit /b 0
:missing_node
echo Node.js 22 or newer is required.
goto failed
:missing_java
echo Java 21 is required.
:failed
echo See README.md for setup and rebuild instructions.
pause
exit /b 1
