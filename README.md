**maketape**:
Purpose is to create a AWStape [hercules virtual tape]. Hercules is a IBM S/370+ mainframe emulator. This allows you to get your local [Linux in my case] data into a AWStape [which is really just a Linux file] that can very quickly be loaded  into a MVS [an~~~~ IBM mainframe OS] dataset. As far as MVS is concerened this AWStape is a real "tape". You can write a MVS program or use a IBM utility [IEBGENER for example] to process the "tape".

**tapedump**:
It's purpose is to examine and provide details on**** the virtual tape created by maketape 

If possible use Jay Mosley's C programs...mich faster. Most importantly these programs were written by Jay Mosley...not me. He stated that he used the final release of Microsoft's Professional Development System to create the program. I don't think there was a suitable BASIC compiler available on Linux at the time. You can Google "jay mosley awstape" for all the details. I just made some small changes so it would run natively on Linux using the very nice FreeBASIC compiler. The executables are small and fast. I also tried QB64 but the executables were huge...by comparison. See the comments within the programs on how to compile with FreeBASIC.

I've included Jay's original source in the "original" subfolder...probably not the best use of git.
