{inputs,[
	% format, {portNum,label,LongDesc,enabled,AssertLevel,Type}
	{0,enable,"Alarm Enable",true,0,control},
	{1,test,"Test",true,0,control},
	{2,front_door,"Front Door Sensor",true,1,sensor},
	{3,rear_door,"Rear Door Sensor",true,1,sensor},
	{4,hallway,"Hallway PIR Sensor",false,0,sensor},
	{5,living,"Living Room PIR Sensor",false,0,sensor},
	{6,input7,"Input 6",false,0,sensor},
	{7,input8,"Input 7",false,0,sensor}
]}.
{port_handler_history_size,	20}.
{outputs,[
	% format, {PortNum,desc,LongDesc,initialState}
	{0,siren,"Siren (Relay 1)",off},
	{1,relay1,"Relay 2",off},
	{2,power_led,"Power On LED",off},
	{3,alarm_active_led,"Alarm Active LED",off},
	{4,output4,"Output 4",off},
	{5,output5,"Output 5",off},
	{6,output6,"Output 6",off},
	{7,output7,"Output 7",off}
]}.
{output_flash_slow,		500}.
{output_flash_normal,	250}.
{output_flash_fast,		100}.
{timer_wait_arm,		30000}.