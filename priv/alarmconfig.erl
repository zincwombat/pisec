{inputs,[
	% format, {portNum,label,LongDesc,enabled,AssertLevel,Type}
	{0,enable,"Alarm Enable",true,0,control},
	{1,test,"Test",true,0,control},
	{2,front_door,"Front Door Sensor",true,1,sensor},
	{3,rear_door,"Rear Door Sensor",true,1,sensor},
	{4,hallway,"Hallway PIR Sensor",false,1,sensor},
	{5,living,"Living Room PIR Sensor",true,1,sensor},
	{6,input7,"Input 6",false,0,sensor},
	{7,input8,"Input 7",false,0,sensor}
]}.
{port_handler_history_size,	20}.
{outputs,[
	% format, {PortNum,label,LongDesc,enabled,initialState,led|power}
	{0,siren,"Siren (Relay 1)",true,off,power},
	{1,relay1,"Relay 2",true,off,power},
	{2,power_led,"Power On LED",true,off,led},
	{3,alarm_status_led,"Alarm Status LED",true,off,led},
	{4,output4,"Output 4",true,off,power},
	{5,output5,"Output 5",true,off,power},
	{6,output6,"Output 6",true,off,power},
	{7,output7,"Output 7",true,off,power}
]}.
{output_flash_slow,		500}.
{output_flash_normal,	250}.
{output_flash_fast,		100}.
{timer_wait_arm,		30000}.
{twilio_notify,			false}.