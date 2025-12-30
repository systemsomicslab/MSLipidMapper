@echo off
setlocal enabledelayedexpansion

set IMAGE_NAME=mslipidmapper
set CONTAINER_NAME=mslipidmapper_app
set SHINY_PORT=3838
set API_PORT=7310

echo [INFO] Checking if Docker is running...
docker info >nul 2>&1
if errorlevel 1 (
  echo [ERROR] Docker Desktop is not running. Please start it and try again.
  pause
  exit /b 1
)

echo [INFO] Building Docker image...
docker build -t %IMAGE_NAME% .
if errorlevel 1 (
  echo [ERROR] Docker build failed.
  pause
  exit /b 1
)

echo [INFO] Removing existing container (if any)...
docker rm -f %CONTAINER_NAME% >nul 2>&1

echo [INFO] Starting container in the background...
docker run -d --rm --name %CONTAINER_NAME% ^
  -p %SHINY_PORT%:%SHINY_PORT% ^
  -p %API_PORT%:%API_PORT% ^
  %IMAGE_NAME% >nul

if errorlevel 1 (
  echo [ERROR] Failed to start container.
  pause
  exit /b 1
)

echo [INFO] Waiting for Shiny at http://localhost:%SHINY_PORT% (max 60 seconds)...
set /a counter=0
:waitloop
timeout /t 1 >nul

powershell -Command ^
  "try { (Invoke-WebRequest -Uri 'http://localhost:%SHINY_PORT%/' -UseBasicParsing -TimeoutSec 2).StatusCode | Out-Null; exit 0 } catch { exit 1 }" ^
  >nul 2>&1

if %errorlevel%==0 goto startbrowser

set /a counter+=1
if !counter! GEQ 60 (
  echo [ERROR] Timeout: App did not start within 60 seconds.
  echo [INFO] Container logs:
  docker logs %CONTAINER_NAME%
  echo.
  pause
  docker rm -f %CONTAINER_NAME% >nul 2>&1
  exit /b 1
)
goto waitloop

:startbrowser
echo [INFO] Launching browser at http://localhost:%SHINY_PORT% ...
start "" "http://localhost:%SHINY_PORT%/"

echo [INFO] API should be available at http://localhost:%API_PORT% (if enabled).
echo [INFO] Container is running: %CONTAINER_NAME%
echo [INFO] Press any key to stop the container and exit.
pause >nul

echo [INFO] Stopping container...
docker rm -f %CONTAINER_NAME% >nul 2>&1

echo [INFO] Done.
endlocal
