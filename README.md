ct_surefire
=============

This is a surefire report generator that can be used to generate
surefire xml reports for your Erlang Common Test Runs. Unlike the
native support that is in Common Test (cth_surefire.erl), ct_surefire
is installed as an event_handler instead of a CT Hook.

You install the event handler as per usual for Common Test:
    ct_run -suite /path/to/suite_SUITE.erl -event_handler ct_surefire -pa /path/to/ct_surefire.beam

You can make sure there are HTTP links in the surefire testcase reports which can be browsed in e.g. Jenkins. For this to work you need to send in a parameter:
    ct_run -suite /path/to/suite_SUITE.erl -event_handler_init ct_surefire http://base/path/to/your/logs/url -pa /path/to/ct_surefire.beam

You can also control where the reports will be written by using the
-surefire_dest_dir flag
    ct_run -suite /path/to/suite_SUITE.erl -event_handler_init ct_surefire http://base/path/to/your/logs/url -pa /path/to/ct_surefire.beam -surefire_dest_dir /path/to/where/you/like

The reports will have the name junit_report.xml
