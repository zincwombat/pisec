{ports,[
	% format, {portNum,label,LogDesc,enabled,AssertLevel}
	{3,front_door,"Front Door Sensor",true,0},
	{4,rear_door,"Rear Door Sensor",true,0},
	{5,hallway,"Hallway PIR Sensor",true,0},
	{6,living,"Living Room PIR Sensor",true,0},
	{7,input7,"Input 7",false,0},
	{8,input8,"Input 8",false,0}
]}.
{control,[
	{1,enable,"Alarm Enable",0},
	{2,test,"Test",0}
]}.
{port_handler_history_size,	20}.
{outputs,[
	% format, {PortNum,desc,LongDesc,initialState}
	{1,siren,"Siren (Relay 1)",off},
	{2,relay1,"Relay 2",off},
	{3,power_led,"Power On LED",off},
	{4,alarm_active_led,"Alarm Active LED",off},
	{5,output4,"Output 5",off},
	{6,output5,"Output 6",off},
	{7,output6,"Output 7",off},
	{8,output7,"Output 8",off}
]}.
{output_flash_slow,		500}.
{output_flash_normal,	250}.
{output_flash_fast,		100}.