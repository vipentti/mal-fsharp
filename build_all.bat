@echo off


set DEST_DIR="D:\Projects\mal\vsharp"

set step0=step0_repl
set step1=step1_read_print
set step2=step2_eval
set step3=step3_env
set step4=step4_if_fn_do
set step5=step5_tco
set step6=step6_file
set step7=step7_quote
set step8=step8_macros
set step9=step9_try
set stepA=stepA_mal

set ENABLED_STEPS=%step1% %step2% %step3% %step4% %step5% %step6% %step7% %step8%

(for %%a in (%ENABLED_STEPS%) do (
   echo Step:  %%a
   CALL build.bat %%a.exe %1 %2 %DEST_DIR%
))

rem CALL build.bat step4_if_fn_do.exe %1 %2 %DEST_DIR%