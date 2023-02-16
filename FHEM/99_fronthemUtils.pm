##############################################
# $Id: 99_fronthemUtils.pm 0 2015-11-10 08:00:00Z herrmannj $
# modified: 2018-04-14 00:00:00Z raman
package main;

use strict;
use warnings;
use JSON;
use Time::HiRes qw(gettimeofday);
#use Time::Seconds;

sub
fronthemUtils_Initialize($$)
{
  my ($hash) = @_;
}

sub fronthem_decodejson($) {
 return decode_json($_[0]);
}  

sub fronthem_encodejson($) {
 return encode_json($_[0]);
} 

sub
fronthem_ActualTimeStamp($)
{
	my ( $time ) = @_;	
	$time =  $time * 1000;
	return sprintf("%.0f", $time) * 1;
}

sub
fronthem_TimeStamp($)
{
  my ($date) = @_;
  my ($year,$mon,$mday,$hour,$min,$sec) = split(/[\s_:-]+/, $date);
  my $time = timelocal($sec,$min,$hour,$mday,$mon-1,$year);
  return $time * 1000;
}

# evaluates smartVISU duration format with up to 4 digits (instead of 2)
# TO DO: loop through the parameter with more terms e.g. "1y 3m 5d 10h" and sum the results up
# alkazaa
# DONE: used split() command and foreach loop around the "if ($period ... " statement
# The individual time components in combined parameter like e.g. "1y 3m 5d 10h" are simply added.
# Bonus: a leading term with 0 like e.g. "0w" will not contribute to the total time but it can
# be used to select the aggregation period for aggregation modes 'avg', 'min', 'sum' etc. in sub fronthem_Duration
sub
fronthem_Time($$)
{
	my ($time, $period) = @_;
	my $temp;
	
	if ($period =~ /^(.*?)\s*(\d{13})/) { # alkazaa
		return int($2/1000 + 0.5);
	}
	if ($period eq "now") {
		return $time;
	}
	# alkazaa
	# the next 'if' is meant to parse a tmin or tmax parameter of the form "0w 2022-12-01 12:34:56"
	# It converts correctly for further processing, however, it is not working with SmartVISU, since
	# within the SV plot.period widget such a tmin statement does not produce the correct x-axis
	if ($period =~ /^(.*?)\s*(\d{4})-(\d{2})-(\d{2})\s(\d{2}):(\d{2}):(\d{2})/) {
		return timelocal($7,$6,$5,$4,$3-1,$2);
	}

	my @periods = split(' ', $period);  # alkazaa: split composed time specifications like "1y 3m 5d 10h" 
	foreach my $period (@periods) 	    # into an array like ("1y","3m","5d","10h") and loop over array elements
	{
		if ($period =~ /^(\d{1,4})(s|i|h|d|w|m|y)/)
		{
			my $newTime = 0;
			if ($2 eq "s")
			{
				$newTime = $1;
			}
			elsif ($2 eq "i")
			{
				$newTime = $1 * 60;
			}
			elsif ($2 eq "h")
			{
				$newTime = $1 * 3600;
			}
			elsif ($2 eq "d")
			{
				$newTime = $1 * 3600 * 24;
			}
			elsif ($2 eq "w")
			{
				$newTime = $1 * 3600 * 24 * 7;
			}
			elsif ($2 eq "m")
			{
				$newTime = $1 * 3600 * 24 * 30;
			}
			elsif ($2 eq "y")
			{
				$newTime = $1 * 3600 * 24 * 365;
			}
			$time = $time - $newTime; 		
		}

	}
	return $time;
}

# select the database evaluation mode from smartVISU duration tmin 
sub
fronthem_Duration($)     
{
	my ($duration) = @_;
	if ($duration =~ /^(\d{1,4})(s|i|h|d|w|m|y|q|x)/)
	{
		if ($2 eq "s" )
		{
			return "timerange";
		}
		elsif ($2 eq "i")
		{
			if ($1 eq "1") {
				return "timerange";
			}
			return "minutestats";
		}
		elsif ($2 eq "q") {
			return "quarterhourstats";
		}
		elsif ($2 eq "h")
		{
			if ($1 eq "1") {
				return "timerange";
			}
			return "hourstats";
		}
		elsif ($2 eq "x") {
			return "quarterdaystats";
		}
		elsif ($2 eq "d")
		{
			if ($1 eq "1") {
				return "hourstats";
			}
			return "daystats";
		}
		elsif ($2 eq "w")
		{
			if ($1 eq "1") {
				return "daystats";
			}
			return "weekstats";
		}
		elsif ($2 eq "m")
		{
			if ($1 eq "1") {
				return "weekstats";
			}
			return "monthstats";
		}
		elsif ($2 eq "y")
		{
			if ($1 eq "1") {
				return "monthstats";
			}
			return "yearstats";
		}
	}	
	return "timerange";
}

sub fronthem_sunrise($) {
  my ($hour,$min,$sec) = split(/:/, sunrise_abs($_[0]));
  return $hour . ':' . $min;
} 

sub fronthem_sunset($) {
  my ($hour,$min,$sec) = split(/:/, sunset_abs($_[0]));
  return $hour . ':' . $min;
} 

###############################################################################
#
# Umsetzen der UZSU-Settings für ein device
#
###############################################################################
#
# ACHTUNG: falls von einer vorherigen fronthem-Version noch das notify definiert ist 
# " define UZSU notify .*:uzsu:.* { UZSU_execute($NAME, $EVTPART1) } "
# dann muss dieses gelöscht werden, da UZSU_Execute sonst doppelt ausgeführt wird 
# und die Einstellungen wieder löscht!!
#
# Damit die Einstellungen gespeichert werden können, folgendes Attribut setzen:
# attr global autosave 1
#
#
sub UZSU_execute($$;$)
{
  my ($device, $uzsu, $save) = @_;
  $save = (defined($save) ? $save : "na");
  my $rg = AttrVal('rg_uzsu_'.$device, "room", "na");   
  fhem('delete wdt_uzsu_'.$device.'.*') if($rg ne "na");
  fhem('delete rg_uzsu_'.$device) if($rg ne "na");
	
  for (my $i = 0; $i < @{$uzsu->{list}}; $i++) {
    if ($uzsu->{list}[$i]->{active}) {
      my %rrule = UZSU_getRrules($uzsu->{list}[$i]{rrule});
      my $holiday = $uzsu->{list}[$i]{holiday}{weekend} && $uzsu->{list}[$i]{holiday}{workday} ? '' : $uzsu->{list}[$i]{holiday}{weekend} ? $rrule{'BYDAY'} ne '' ? ',$we' : '$we' : $uzsu->{list}[$i]{holiday}{workday} ? $rrule{'BYDAY'} ne '' ? ',!$we' : '!$we' : '';
			
      my $time = $uzsu->{list}[$i]{event} eq "time" ?  $uzsu->{list}[$i]{time} : '{'.$uzsu->{list}[$i]->{event} . '_abs("REAL"' . ($uzsu->{list}[$i]->{timeOffset} ne '' ? ',' . $uzsu->{list}[$i]->{timeOffset} * 60 : '') . ($uzsu->{list}[$i]->{timeMin} ne '' ? ', "' . $uzsu->{list}[$i]->{timeMin} . '"' : '') . ($uzsu->{list}[$i]->{timeMax} ne '' ? ', "' . $uzsu->{list}[$i]->{timeMax} . '"' : '') . ')}';
      my $condition = UZSU_getCommand($uzsu->{list}[$i]{condition});
			
      my $weekdayTimer = $rrule{'BYDAY'} . $holiday . ($rrule{'BYDAY'} ne '' || $holiday ne '' ? "|" : '') . $time . "|" . $uzsu->{list}[$i]{value};	
      my $delayedExec = UZSU_getCommand($uzsu->{list}[$i]{delayedExec});
						
      fhem('defmod wdt_uzsu_' . $device . '_' . $i . ' WeekdayTimer ' . $device . ' en ' . $weekdayTimer . $condition);
      fhem('attr wdt_uzsu_' . $device . '_' . $i . ' room UZSU');
      fhem('attr wdt_uzsu_' . $device . '_' . $i . ' group ' . $device);
      fhem('setreading wdt_uzsu_' . $device . '_' . $i . ' weekdays ' . $weekdayTimer);
      fhem('defmod rg_uzsu_' . $device . ' readingsGroup wdt_uzsu_' . $device . '.*');
      fhem('attr rg_uzsu_' . $device . ' room UZSU');
	  if ($delayedExec) {
        fhem('attr wdt_uzsu_' . $device . '_' . $i . ' delayedExecutionCond ' . $delayedExec);
      }
    }
  }
  if ($uzsu->{active}) {
    fhem('attr NAME=wdt_uzsu_' . $device . '_.*' . ' disable 0');
  }
  else {
    fhem('attr NAME=wdt_uzsu_' . $device . '_.*' . ' disable 1');
  }
  fhem('save', 1) if ($save eq 'save');	
}

###############################################################################
#
# UZSU-Hilfsfunktionen
#
###############################################################################
sub UZSU_getRrules($)
{
	my ($split) = @_; 
	my @a = split(/;/, $split);
	my %hash;
	foreach (@a){
		my ($key,$val) = split(/=/, $_);
		$hash{$key} = $val;
	}
	
	if (exists($hash{'BYDAY'}))
	{
		return %hash;
	}
	else {
		$hash{'BYDAY'} = '';
		return %hash;
	}
}

sub UZSU_getCommand($)
{
  my ($command) = @_;
  
  if($command->{active} && $command->{type} ne "String")
  {
    if($command->{deviceString} =~ /^AttrVal|InternalVal|ReadingsVal\("\S+"\s?,\s?"\S+"\s?,\s?"\S*"\)$/)
    {
      return ' (' . $command->{deviceString} . ' ' . $command->{type} . ' "' . $command->{value} . '")';
    }
    elsif($command->{deviceString} =~ /^Value\("\S+"\)$/)
    {
      return ' (' . $command->{deviceString} . ' ' . $command->{type} . ' "' . $command->{value} . '")';
    }
  }
  elsif($command->{active} && $command->{type} eq "String" && $command->{deviceString} ne '')
  {
    if($command->{deviceString} =~ /^fhem ".+"( if\(.+\))?$/)
    {
      return ' {' . $command->{deviceString} . '}';
    }
    elsif($command->{deviceString} =~ /(setstate|setreading|set)\s+([a-zA-Z]+)\s*$/)
    {
      return ' ' . $1 . ' $NAME ' . $2 . ' $EVENT';
    }
    else
    {
      return ' (' . $command->{deviceString} . ')';
    }
  }
  return '';
}


###############################################################################
#
# Sammeln der Log-Daten in einem Dummy-Device
# (Dummy-Device wir automatisch erstellt, wenn für zu überwachende Devices
#  svReadins gesetzt wird - Erklärung siehe unten!)
#
#
###############################################################################
#
# Nicht vergessen! In FHEM notify definieren!
#
# define Log notify .* { Log_SetList($NAME,$EVENT) }
#
# und folgende Attribute in "attr global userattr" hinzufügen:
#
# svEvents svReadins svRegex
#
# svReadins: Für das Device das Reading setzen, das überwacht werden soll.
#            z.B. wenn state überwacht werden soll:
#
#                 attr <device> svReadins state battery:Batterie
#
#            z.B. wenn state und battery überwacht und
#                 battery in Batterie geändert werden soll:
#
#                 attr <device> svReadins state battery:Batterie
#
#            Zur Übersicht der überwachten Devices kann eine readingsGroup 
#            angelegt werden:
#
#                 define rg_svLog readingsGroup <Name>,<svReadings> .*:?svReadins,?
#				  attr rg_svLog nameStyle style="color:red;;font-weight:bold"
#
# svEvents:  Wird automatisch für das Dummy-Device (Log-Liste) angelegt, kann aber
#            an die jeweiligen Bedürdnisse angepasst werden. 
#    
# svRegex:   Hier kann für das Dummy-Device (Log-Liste) eine Liste mit Events,
#            die ersetzt werden sollen, angegeben werden.
#
# 			 z.B. attr <device> svRegex low:schwach opened:geöffnet closed:geschlossen connected:verbunden disconnected:getrennt
#
sub
Log_SetList($$)
{
	my ($device, $event) = @_;
	my $reading = "";
    my $message = "";
		
	if ($event =~ qr/(.*?):\s+(.*)/p) {
		$reading = $1;
        $message = $2;
    } else {
        $reading = "state";
        $message = $event;
    }
	
	my @info = split(" ", AttrVal($device, "svReadins", ""));
	my $list = "";
	my $newReading = "";
	foreach (@info) {
		if ($_ =~ qr/(.*):(.+)/p) {
			$list = $1;
			$newReading = $2;
		} else {
			$list = $_;
		}
		
		if ($list eq $reading)
		{			
			fhem ("set" . " svLog_" . $list . " block");
			
			if ($newReading ne "") {
				$reading = $newReading;
			}

			if(!$defs{"svLog_" . $list}) {
				fhem ("define" . " svLog_" . $list . " dummy");
				fhem("attr" . " svLog_" . $list . " svEvents info:alive,ok,online,[O|o]pened,[C|c]onnected,[C|c]losed,[G|g]eschlossen warning:low,[O|o]pen,[O|o]ffen,offline,overload,unreachable error:dead,[D|d]isconnected,unknown,IOerr");
			}

			my @state = split(" ", AttrVal("svLog_" . $list, "svEvents", ""));
			foreach (@state) {
				if ($_ =~ qr/(.*):(.+)/p) {
					my $z = $2;
					$z =~ s/,/|/g;
									
					if($reading eq "state") {
						fhem ("setreading" . " svLog_" . $list . " " . $device . " " . $message);
					} elsif ($message =~ /.*:?\s?($z)/) {
						fhem ("setreading" . " svLog_" . $list . " " . $device . " " . $reading . " " . $1);
					} else {
						fhem ("setreading" . " svLog_" . $list . " " . $device . " " . $reading . " " . $message);
					}
				}
			}
			fhem ("set" . " svLog_" . $list . " send");			
		}	
	}
}

###############################################################################
#
# Log-Hilfsfunktionen
#
###############################################################################
sub
Log_GetList($)
{
	my ($name) = @_;
	my $list = "";
	my $level = "info"; # info = green, error = red,  warning = yellow (levels for smartvisu status.log)
		
	my @info = split(" ", AttrVal($name, "svRegex", ""));
	my @state = split(" ", AttrVal($name, "svEvents", ""));

	my $i = 1;
	foreach my $key (sort { $b cmp $a } keys %{$defs{$name}{READINGS}})
	{
		my $reading = $defs{$name}{READINGS}{$key}{VAL};
		
		foreach (@state) {
			if ($_ =~ qr/(.*):(.+)/p) {
				my $newLevel = $1;
				my $states = $2;
				$states =~ s/,/|/g;
				
				if($reading =~ /.*:?\s?($states)/)
				{
					$level = $newLevel;
							
					foreach (@info) {
						if ($_ =~ qr/(.*):(.+)/p) {
							my $a = $1;
							my $b = $2;	
							$reading =~ s/$a/$b/g;
						}
					}
				}
			}
		}
		
		my $device = AttrVal( $key, "alias", $key );
		my $timestring = $defs{$name}{READINGS}{$key}{TIME};
		$timestring =~ s/\s/T/g; # Compatibility for older Browser "2018-03-10T18:34:53"				
		$list .= '{"message":"'. $device . " - " . $reading . '","time":"' . $timestring . '","level":"' . $level . '"}' . ($i == keys(%{$defs{$name}{READINGS}}) ? "" : ",") if ($key ne "state");
			
		$i++;		
	}
	return $list;	
}


package fronthem;
use strict;
use warnings;

###############################################################################
# For use with UZSU-Widget in SV and UZSU-notify in fhem
# Setreading a device reading using JSON conversion (gadval => reading=decode_json() => setval => encode_json(reading) )
# the reading ("uzsu") must be created manually for each UZSU-enabled device in fhem using "setreading <device> uzsu {}"
# in the fhem commandline
###############################################################################

sub UZSU(@)
{
  my ($param) = @_;
  my $cmd = $param->{cmd};
  my $gad = $param->{gad};
  my $gadval = $param->{gadval};

  my $device = $param->{device};
  my $reading = $param->{reading};
  my $event = $param->{event};
  
  my @args = @{$param->{args}};
  my $cache = $param->{cache};
  my $save = '';
  
  if (@args == 1 && $args[0] eq 'save')
  {
    $save = 'save';
  }

  if ($param->{cmd} eq 'get')
  {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'send')
  {
    $param->{gad} = $gad;
	$param->{gadval} = main::fronthem_decodejson(main::ReadingsVal($device, $reading, '{"active": false, "list": []}'));
    $param->{gads} = [];
    $param->{gadval}->{sunrise} = main::fronthem_sunrise("REAL");
    $param->{gadval}->{sunset} = main::fronthem_sunset("REAL");
	
    return undef;
  }
  elsif ($param->{cmd} eq 'rcv')
  {
    main::UZSU_execute($device, $gadval, $save);
	$gadval = main::fronthem_encodejson($gadval);
	$gadval =~ s/;/;;/ig;
	$param->{result} = main::fhem("setreading $device $reading $gadval");
	$param->{results} = [];
    return 'done';
  }
  elsif ($param->{cmd} eq '?')
  {
    return 'usage: UZSU';
  }
  return undef;
}

###############################################################################
#
# connect fhem device with on|off state to switch
#
###############################################################################
sub AnAus(@)
{
  my ($param) = @_;
  my $cmd = $param->{cmd};
  my $gad = $param->{gad};
  my $gadval = $param->{gadval};

  my $device = $param->{device};
  my $reading = $param->{reading};
  my $event = $param->{event};
  
  my @args = @{$param->{args}};
  my $cache = $param->{cache};

  if ($param->{cmd} eq 'get')
  {
    $event = ($reading eq 'state')?main::Value($device):main::ReadingsVal($device, $reading, 'aus');
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'send')
  {
    $param->{gad} = $gad;
		$param->{gadval} = (lc($event) eq 'an')?'1':'0';
		$param->{gads} = [];
    return undef;
  }
  elsif ($param->{cmd} eq 'rcv')
  {
		$param->{result} = ($gadval)?'an':'aus';
		$param->{results} = [];
    return undef;
  }
  elsif ($param->{cmd} eq '?')
  {
    return 'usage: AnAus';
  }
  return undef;
}

###############################################################################
#
# direct invert numerical values
# @param max 
#
###############################################################################
sub NumInvert(@)
{
  my ($param) = @_;
  my $cmd = $param->{cmd};
  my $gad = $param->{gad};
  my $gadval = $param->{gadval};

  my $device = $param->{device};
  my $reading = $param->{reading};
  my $event = $param->{event};
  
  my @args = @{$param->{args}};
  my $cache = $param->{cache};
  my $max = 100;
  if (defined($args[0])) 
  {
	$max = $args[0];
  }
 
  if ($param->{cmd} eq 'get')
  {
    $event = ($reading eq 'state')?main::Value($device):main::ReadingsVal($device, $reading, '');
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'send')
  {
    return "NumInvert converter got [$event] from $device, $reading but cant interpret it as a number" unless $event =~ /\D*([+-]{0,1}\d+[.]{0,1}\d*).*?/;
	$event = $max - $1;
    $param->{gad} = $gad;
    $param->{gadval} = $event;
    $param->{gads} = [];
    return undef;
  }
  elsif ($param->{cmd} eq 'rcv')
  {	
	return "NumInvert converter received [$gadval] but cant interpret it as a number" unless $gadval =~ /\D*([+-]{0,1}\d+[.]{0,1}\d*).*?/;
    $gadval = $max - $1;
	$param->{result} = $gadval;
	$param->{results} = [];
    return undef;
  }
  elsif ($param->{cmd} eq '?')
  {
    return 'usage: NumInvert';
  }
  return undef;
}

###############################################################################
#
# Send readings collected in dummy device as status.log
#
###############################################################################
sub Log(@)
{
  my ($param) = @_;
  my $cmd = $param->{cmd};
  my $gad = $param->{gad};
  my $gadval = $param->{gadval};

  my $device = $param->{device};
  my $reading = $param->{reading};
  my $event = $param->{event};
  
  my @args = @{$param->{args}};
  my $cache = $param->{cache};

  if ($param->{cmd} eq 'get')
  {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'log')
  {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'send')
  {
	use Encode qw(decode encode);
	my $list = main::Log_GetList($device);
	$list = encode("utf8", '[' . $list . ']');
    $param->{gad} = $gad;	
	$param->{gadval} = main::fronthem_decodejson($list);
	$param->{gads} = [];

    return undef;
  }
  elsif ($param->{cmd} eq 'rcv')
  {
		$param->{result} = $gadval;
		$param->{results} = [];
    return undef;
  }
  elsif ($param->{cmd} eq '?')
  {
    return 'usage: log';
  }
  return undef;
}

###############################################################################
#
# Plot data from fhem database
# @param <name of database> 
#
###############################################################################
sub Plot(@)
{
  my ($param) = @_;
  my $cmd = $param->{cmd};
  my $gad = $param->{gad};
  my $gadval = $param->{gadval};

  my $device = $param->{device};
  my $reading = $param->{reading};
  my $event = $param->{event};
  
  my $mode = $param->{mode};
  my $start = $param->{start};
  my $end = $param->{end};
  my $count = $param->{count};
  my $interval = $param->{interval};
  my $updatemode = $param->{updatemode};
  
  my @args = @{$param->{args}};
  my $cache = $param->{cache};

  return "error $gad: converter syntax: missing paramter: name of database" if (@args != 1);

  if ($param->{cmd} eq 'get') {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'plot') {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'send') {
		my @data = {
			"item" => $gad . '.' . $mode . '.' . $start . '.' . $end  . '.' . $count,
			"updatemode" => $updatemode,
			"plotdata" => [],
		};

		if ($updatemode eq 'point') {
			if ($mode eq "raw") {
				push(@{$data[0]->{plotdata}[0]},  main::fronthem_ActualTimeStamp(main::gettimeofday()));
				push(@{$data[0]->{plotdata}[0]}, sprintf("%#.4f", $event) * 1);
			}						
			elsif ($mode eq "avg") {
				
			}
			elsif ($mode eq "sum") {
				
			}
			elsif ($mode eq "min") {
				
			}
			elsif ($mode eq "max") {
				
			}
		}
		else {	# alkazaa
			my $string = '';
			my $duration = "timerange";
			if ($mode ne "raw") { # for all modes except 'raw' the aggregation period is evaluated from the pirst term in argument '$start'
				$duration = main::fronthem_Duration($start); 
			}
			my $from = main::FmtDateTime(main::fronthem_Time(time(), $start));
			my $to = main::FmtDateTime(main::fronthem_Time(time(), $end));	
		
			my $use_DbRep_functions = "0";	
			if (($mode eq 'avg') && ($count > 2)) {
				$use_DbRep_functions = "1";
				# The SQL-query described in https://forum.fhem.de/index.php/topic,130769.msg1249925.html#msg1249925 
				# divides the [$start...$end] interval in '$count' bins and averages over each bin
				$string = 'get '.$args[0].'Report sqlCmdBlocking SET @weighted="yes", @device = "'.$device.'", @reading = "'.$reading.'", @start = "'.$from.'", @end = "'.$to.'", @count = '.$count.';; SELECT tim, CASE WHEN @weighted="yes" THEN CAST(sum(val * (to_seconds(nexttim) - to_seconds(tim))) / sum((to_seconds(nexttim) - to_seconds(tim))) AS DECIMAL(12,4)) ELSE CAST(sum(val) / count(val) AS DECIMAL(12,4)) END AS avrg FROM ( SELECT timestamp as tim, value as val, LEAD(timestamp) over (order by timestamp) nexttim, truncate(@count * (to_seconds(timestamp) - to_seconds(@start)) / (to_seconds(@end) - to_seconds(@start)),0)/@count as avgtim FROM history WHERE TIMESTAMP BETWEEN @start AND @end AND DEVICE LIKE @device AND READING LIKE @reading ) select1 group by avgtim;';				
			}
			if (($mode eq 'avg') && ($count == 2)) { 
				$use_DbRep_functions = "2";
				
				# With $mode='avg' and $count = 2, an SQL query similar to the one used in 'get ... webchart ...' in 93_DbLog.pm
				# is used. It is, however, modified to return a time-weighted average with an averaging interval given by $duration.
				# A value leaking into the next averaging interval is considered proportionately in both intervals
				# TO DO: adapt SQL query for sqLite and PostgreSQL
				
								############## BEGIN assembly of SQL query ##############
				$string = 'get '.$args[0].'Report sqlCmdBlocking SET @weighted="yes", @device="'.$device.'", @reading="'.$reading.'", @start="'.$from.'", @end="'.$to.'";; SELECT avgtim, CASE @weighted WHEN "yes" THEN CAST((SUM(val * weight)/SUM(weight)) AS DECIMAL(12,4)) ELSE CAST(sum(val) / count(val) AS DECIMAL(12,4)) END AS avrg FROM ( SELECT tim, avgtim, ';
				if ($duration eq 'weekstats') { $string .= 'date_format(tim, "%Y-%u 00:00:00")';}
				else { $string .= 'avgtim';  };				
				$string .= ' AS grouptim, CASE WHEN avgtim!=preavgtim THEN to_seconds(nexttim)-to_seconds(avgtim) WHEN avgtim!=nextavgtim THEN to_seconds(nextavgtim)-to_seconds(tim) ELSE to_seconds(nexttim)-to_seconds(tim) END AS weight, CASE WHEN avgtim!=preavgtim THEN CASE WHEN @weighted="yes" THEN CASE WHEN avgtim!=nextavgtim THEN (preval*(to_seconds(tim)-to_seconds(avgtim)) +  val * (to_seconds(nextavgtim) - to_seconds(tim))) / (to_seconds(nextavgtim) - to_seconds(avgtim)) ELSE (preval * (to_seconds(tim) - to_seconds(avgtim)) + val * (to_seconds(nexttim) - to_seconds(tim))) / (to_seconds(nexttim) - to_seconds(avgtim)) END ELSE val END ELSE val END AS val FROM ( SELECT tim, nexttim, val, preval, avgtim, LAG(avgtim) OVER (ORDER BY tim) AS preavgtim, LEAD(avgtim) OVER (ORDER BY tim) AS nextavgtim FROM ( SELECT timestamp AS tim, LEAD(timestamp) OVER (ORDER BY timestamp) AS nexttim, value AS val, LAG(value) OVER (ORDER BY timestamp) AS preval, ';
				if    ($duration eq 'minutestats') { 
					$string .= 'date_format(timestamp, "%Y-%m-%d %H:%i:00")';}
				if    ($duration eq 'quarterhourstats') { 
					$string .= 'concat(date_format(timestamp,"%Y-%m-%d %H:"),LPAD(15*(date_format(timestamp,"%i") div 15),2,"0"),":00")';}
				elsif ($duration eq 'hourstats') { 
					$string .= 'date_format(timestamp, "%Y-%m-%d %H:00:00")';}
				elsif ($duration eq 'quarterdaystats') { 
					$string .= 'concat(date_format(timestamp, "%Y-%m-%d "),LPAD(6*(date_format(timestamp, "%H") div 6),2,"0"),":00:00")';}
				elsif ($duration eq 'daystats') { 
					$string .= 'date_format(timestamp, "%Y-%m-%d 00:00:00")';}
				elsif ($duration eq 'weekstats') { 
					$string .= 'date_format(timestamp, "%Y-%m-%d 00:00:00")';}
				elsif ($duration eq 'monthstats') { 
					$string .= 'date_format(timestamp, "%Y-%m-01 00:00:00")';}
				elsif ($duration eq 'quarteryearstats') { 
					$string .= 'concat(date_format(timestamp, "%Y-"),LPAD(3*(date_format(timestamp, "%m") div 3),2,"0"),"-01 00:00:00")';}
				elsif ($duration eq 'yearstats') { 
					$string .= 'date_format(timestamp, "%Y-01-01 00:00:00")';};
				$string .= ' AS avgtim FROM history WHERE TIMESTAMP BETWEEN @start AND @end AND DEVICE LIKE @device AND READING LIKE @reading ORDER BY timestamp ) select3 ) select2 ) select1 GROUP BY grouptim';
				
								############## END assembly of SQL query ##############
			}
			
			if ($use_DbRep_functions ne "0") {
				# Define a DbRep device connected to the DbLog device and set its 'sqlResultFormat' attribute to 'mline'.
				# The name of the DbRep device is that of the DbLog device, with 'Report' appended.
				# There is no error checking implemented yet
				main::fhem("defmod -silent ".$args[0]."Report DbRep ".$args[0]);
				main::fhem("attr -silent ".$args[0]."Report sqlResultFormat mline");
				
				############## execute the query:
				$string = main::fhem($string,1); 
				
				############## parse the query result:
				my @a = split("\n",$string);
				my @xypair; my $line;
				my $i = 0;
				foreach $line (@a) {
					@xypair = split('\|', $line);
					if ($xypair[1] ne "") {
						push(@{$data[0]->{plotdata}[$i]},  main::fronthem_TimeStamp($xypair[0]));
						push(@{$data[0]->{plotdata}[$i]},  sprintf("%#.4f", $xypair[1]) * 1);
						$i++;
					}
				}
			}
			else { # perform the processing as in older 99_fronthemUtils.pm versions, using 
				   # the 'get ... webchart ...' function of module 93_DbLog
				$from =~ s/ /_/ig;
				$to =~ s/ /_/ig;
				if ($duration eq "minutestats") {$duration="timerange"};
				my $string = main::fhem("get "  . $args[0] . ' - webchart ' . $from . ' ' . $to . ' ' . $device . ' ' . $duration . ' TIMESTAMP ' . $reading, 1);
				my @response = main::fronthem_decodejson($string);
				
				foreach my $data (@response) {
					my $i = 0;
					foreach my $row (@{$data->{data}}) {
						if ($mode eq "raw") { # [TIMESTAMP,VALUE]
							push(@{$data[0]->{plotdata}[$i]}, main::fronthem_TimeStamp($row->{TIMESTAMP}));
							push(@{$data[0]->{plotdata}[$i]}, sprintf("%#.4f", $row->{VALUE}) * 1);
						}
						elsif ($mode eq "avg") { # [TIMESTAMP,AVG]
							push(@{$data[0]->{plotdata}[$i]}, main::fronthem_TimeStamp($row->{TIMESTAMP}));
							push(@{$data[0]->{plotdata}[$i]}, $duration eq "timerange" ? sprintf("%#.4f", $row->{VALUE}) * 1 : sprintf("%#.4f", $row->{AVG}) * 1);
						}
						elsif ($mode eq "sum") { # [TIMESTAMP,SUM]
							push(@{$data[0]->{plotdata}[$i]}, main::fronthem_TimeStamp($row->{TIMESTAMP}));
							push(@{$data[0]->{plotdata}[$i]}, $duration eq "timerange" ? sprintf("%#.4f", $row->{VALUE}) * 1 : sprintf("%#.4f", $row->{SUM}) * 1);
						}
						elsif ($mode eq "min") { # [TIMESTAMP,MIN]
							push(@{$data[0]->{plotdata}[$i]}, main::fronthem_TimeStamp($row->{TIMESTAMP}));
							push(@{$data[0]->{plotdata}[$i]}, $duration eq "timerange" ? sprintf("%#.4f", $row->{VALUE}) * 1 : sprintf("%#.4f", $row->{MIN}) * 1);
						}
						elsif ($mode eq "max") {  # [TIMESTAMP,MAX]
							push(@{$data[0]->{plotdata}[$i]}, main::fronthem_TimeStamp($row->{TIMESTAMP}));
							push(@{$data[0]->{plotdata}[$i]}, $duration eq "timerange" ? sprintf("%#.4f", $row->{VALUE}) * 1 : sprintf("%#.4f", $row->{MAX}) * 1);
						}
						$i++;
					}	
				}
			}
		}	

		$param->{gads} = [@data];
		return undef;
  }
  elsif ($param->{cmd} eq 'rcv') {
	$param->{result} = $gadval;
	$param->{results} = [];
	return undef;
  }
  elsif ($param->{cmd} eq '?') {
	return 'usage: plot';
  }
  return undef;
}

###############################################################################
#
# Plot data from fhem filelog
# @param <name of logfile> <column> <regex>
#
###############################################################################
sub Plotfile(@)
{
  my ($param) = @_;
  my $cmd = $param->{cmd};
  my $gad = $param->{gad};
  my $gadval = $param->{gadval};

  my $device = $param->{device};
  my $reading = $param->{reading};
  my $event = $param->{event};
  
  my $mode = $param->{mode};
  my $start = $param->{start};
  my $end = $param->{end};
  my $count = $param->{count};
  my $interval = $param->{interval};
  my $updatemode = $param->{updatemode};
  
  my @args = @{$param->{args}};
  my $cache = $param->{cache};
     
  return "error $gad: converter syntax: missing paramter: name of database" if (@args < 1 || @args > 4);
 
  if ($param->{cmd} eq 'get') {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'plot') {
    $param->{cmd} = 'send';
  }
  if ($param->{cmd} eq 'send') {
	my @data = {
			"item" => $gad . '.' . $mode . '.' . $start . '.' . $end  . '.' . $count,
			"updatemode" => $updatemode,
			"plotdata" => [],
	};
	
	my $column = 3;
	if ($reading eq "state") {
		$reading = "";
	} elsif($reading ne "state" && @args == 1) {
		$column = 4;
	} elsif(@args > 1) { 
		$column = $args[1] * 1;
		if ($args[2]) {
			$reading = $args[2];
		}
	}	
	my $regex = $args[3] ? $args[3] : "";
		
		if ($updatemode eq 'point') {
			if (!$args[3]) {
				push(@{$data[0]->{plotdata}[0]},  main::fronthem_ActualTimeStamp(main::gettimeofday()));
				push(@{$data[0]->{plotdata}[0]}, sprintf("%#.4f", $event) * 1);
			}
		}
		else {
			my $from = main::FmtDateTime(main::fronthem_Time(time(), $start));
			$from =~ s/ /_/ig;
			my $to = main::FmtDateTime(main::fronthem_Time(time(), $end));			
			$to =~ s/ /_/ig;
			
			my $string = main::fhem("get "  . $args[0] . " - - " . $from . " " . $to ." " . $column .":" . $reading . ":0:" . $regex, 1);
			my @response = split("\n", $string);
			pop @response;
			
			for (my $i = 0; $i < @response; $i++) {			
				$response[$i] =~ /([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}:[0-9]{2}:[0-9]{2})\s+([0-9\.-]+)/;				
				push(@{$data[0]->{plotdata}[$i]}, main::fronthem_TimeStamp($1));
				push(@{$data[0]->{plotdata}[$i]}, sprintf("%#.4f", $2) * 1);
			}
		}	
		$param->{gads} = [@data];
		return undef;
  }
  elsif ($param->{cmd} eq 'rcv') {
	$param->{result} = $gadval;
	$param->{results} = [];
	return undef;
  }
  elsif ($param->{cmd} eq '?') {
	return 'usage: plotfile';
  }
  return undef;
}

1;

=pod
=item helper
=item summary fronthem utility functions
=item summary_DE fronthem Hilfsfunktionen
=begin html

  <p>
    <a name="fronthemUtils" id="fronthemUtils"></a>
  </p>
  <h3>
    fronthemUtils
  </h3>
  <ul>
    This is a collection of converter functions that can be used with
    fronthemDevice<br/>
    </br>
    </br>
    <b>Defined converter functions</b><br/><br/>
	<ul>
	  <li><b>AnAus</b><br>invert state values an|aus to 0|1</li><br/>
	  <li><b>Log</b><br>send readings collected in dummy device as status.log</li><br/>
	  <li><b>NumInvert</b><br>direct invert of numerical values</li><br/>
	  <li><b>Plot</b><br>Plot data from fhem database<br>
	  parameter for converter: Plot &lt;name of database&gt;<br>
	  For MySQL databases the averaging mode allows time-weighted averaging. <br>
	  The averaging time is specified by the tmin parameter in plot.period.  <br>
	  For details see <a href="https://forum.fhem.de/index.php/topic,118668.msg1251787.html#msg1251787 ">here</a> and other contribution in that thread
	  </li><br/>
	  <li><b>Plotfile</b><br>Plot data from fhem filelog<br>
	  parameter for converter: Plotfile &lt;column&gt; &lt;regex&gt;
	  </li><br/>
	  <li><b>UZSU</b><br>for use with UZSU-Widget in smartVisu<br>
	  parameter for converter: UZSU &lt;save&gt; (optional: save - set it to save data for WeekdayTimer, <b>attr global autosave</b> must be set to 1 )
	  </li><br/>
	</ul>
  </ul>
  
=end html

=begin html_DE

  <p>
    <a name="fronthemUtils" id="fronthemUtils"></a>
  </p>
  <h3>
    fronthemUtils
  </h3>
  <ul>
    Sammlung an Converter-Functionen, die mit
    fronthemDevice eingesetzt werden k&ouml;nnen<br/>
    </br>
    </br>
    <b>Converter-Functionen</b><br/><br/>
	<ul>
	  <li><b>AnAus</b><br>wandelt die Werte an|aus in 0|1 um und umgekehrt</li><br/>
	  <li><b>Log</b><br>Sendet Readings, die in einem Dummy-Device gesammelt werden als status.log</li><br/>
	  <li><b>NumInvert</b><br>wandelt numerische Werte direkt um</li><br/>
	  <li><b>Plot</b><br>Plot-Daten aus der FHEM-Datenbank<br>
	  Parameter f&uuml;r converter: Plot &lt;name of database&gt;
	  </li><br/>
	  <li><b>Plotfile</b><br>Plot-Daten aus filelog von FHEM<br>
	  Parameter f&uuml;r converter: Plotfile &lt;column&gt; &lt;regex&gt;
	  </li><br/>
	  <li><b>UZSU</b><br>for use with UZSU-Widget in smartVisu<br>
	  Parameter f&uuml;r converter: UZSU &lt;save&gt; (optional: save - Parameter angeben, wenn die Daten f&uuml;r WeekdayTimer gespeichert werden sollen,
	  <b>attr global autosave</b> muss daf&uuml;r auf den Wert 1 gesetzt werden)
	  </li><br/>
	</ul>
  </ul>
  
=end html_DE
=cut
