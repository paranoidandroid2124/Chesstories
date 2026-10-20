@echo off
setlocal
cd /d "%~dp0chesstory-web"
where node >nul 2>nul
if errorlevel 1 goto missing_node
node scripts\start.mjs
if errorlevel 1 goto failed
exit /b 0
:missing_node
echo Node.js 22 or newer is required.
:failed
echo See README.md for setup and update instructions.
pause
exit /b 1
