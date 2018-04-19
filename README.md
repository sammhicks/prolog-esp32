# prolog-esp32
Running Prolog code on a microcontroller


## Dependencies

 + [The ESP32 toolchain](http://esp-idf.readthedocs.io/en/latest/get-started/index.html)
 + [SWI Prolog](http://www.swi-prolog.org/)


## Installation instructions

 1. Clone this repository. You'll need to include the `--recurse-submodules` as this repository has submodules
 2. Navigate to the `<repository root>/wam` directory
 3. Configure the project using `make menuconfig.` The `Prolog on ESP32 Configuration` menu contains WAM specific configuration
 4. Connect your ESP32 and build, flash, and monitor the project using `make flash monitor`
 5. Connect your machine to the WiFi AP on the ESP32 (the name and password are configurable in step 3)
 6. In a separate terminal, navigate to the `<repository root>/terminal` directory
 7. Run the terminal using `swipl terminal.pl`. The terminal predicates are documented in `terminal/terminal.pl`
