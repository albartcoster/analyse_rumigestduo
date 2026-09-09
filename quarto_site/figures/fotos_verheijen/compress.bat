@echo off
rem Batch compress all JPG images in the current folder
rem Set desired quality level (0-100)
set QUALITY=40
rem Set prefix for compressed files
set PREFIX=compressed_

rem Loop for .jpg files, skip files starting with PREFIX
for %%i in (*.jpg) do (
    if /I not "%%i"=="%PREFIX%%%i" (
        rem Check if file already has the prefix
        echo %%i | findstr /B /I "%PREFIX%" >nul
        if errorlevel 1 (
            magick "%%i" -quality %QUALITY% "%PREFIX%%%i"
            echo Compressed %%i to %PREFIX%%%i at quality %QUALITY%
        ) else (
            echo Skipping %%i (already compressed)
        )
    )
)

rem Loop for .jpeg files, skip files starting with PREFIX
for %%i in (*.jpeg) do (
    if /I not "%%i"=="%PREFIX%%%i" (
        echo %%i | findstr /B /I "%PREFIX%" >nul
        if errorlevel 1 (
            magick "%%i" -quality %QUALITY% "%PREFIX%%%i"
            echo Compressed %%i to %PREFIX%%%i at quality %QUALITY%
        ) else (
            echo Skipping %%i (already compressed)
        )
    )
)

echo Compression completed.
pause