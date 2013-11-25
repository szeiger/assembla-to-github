Merge Assembla tickets into github issues
-----------------------------------------

Create ~/.github with your authentication credentials:

    oauth=...

Put dump.js from the Assembla backup into the current directory.

Configure the variables at the top of Main.scala. Set create=true to write to
the gitub repo. Otherwise only the verification steps are performed.

When you run with create=true, a transaction log is written (to log.txt by
default). If the process aborts due to some error that cannot be checked
beforehand (e.g. missing labels or missing user permissions when assigning a
ticket), you can fix the error manually and then simply run the migration
again. It will skip all steps that have already been logged.
