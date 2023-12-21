<?php
$vars = fopen('cvars.csv','r');
$csv = fgetcsv($vars,10000000,';');
$csv = array_combine($csv,fgetcsv($vars,10000000,';'));
var_dump($csv[$argv[1]]);
