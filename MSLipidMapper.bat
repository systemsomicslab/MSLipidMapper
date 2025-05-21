@echo off
set IMAGE_NAME=mslipidmapper

echo [INFO] Checking if Docker is running...
docker info >nul 2>&1
if errorlevel 1 (
  echo [ERROR] Docker Desktop is not running. Please start it and try again.
  pause
  exit /b
)

echo [INFO] Building Docker image...
docker build -t %IMAGE_NAME% .

echo [INFO] Starting container in the background...
start /B docker run --rm -p 7860:7860 -p 9000:9000 %IMAGE_NAME%

:: Wait until port 7860 is open (max 30 seconds)
set /a counter=0
:waitloop
timeout /t 1 >nul
powershell -Command "(Invoke-WebRequest -Uri http://localhost:7860 -UseBasicParsing).StatusCode" >nul 2>&1
if %errorlevel%==0 goto startbrowser
set /a counter+=1
if %counter% GEQ 30 (
    echo [ERROR] Timeout: App did not start within 30 seconds.
    pause
    exit /b
)
goto waitloop

:startbrowser
echo [INFO] Launching browser at http://localhost:7860 ...
start "" http://localhost:7860

echo [INFO] Container is running. Close this window to stop.
pause

