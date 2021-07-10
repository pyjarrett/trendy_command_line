package Trendy_Command_Line.Options is

    -- Different things that options can do.
    type Option_Action is (True_When_Set,
                           False_When_Set,
                           Store_Int,
                           Store_String,
                           Store_Operands);

    Min_Following_Operands : constant array (Option_Action) of Natural :=
                               (True_When_Set  => 0,
                                False_When_Set => 0,
                                Store_Int      => 1,
                                Store_String   => 1,
                                Store_Operands => 1);

    Max_Following_Operands : constant array (Option_Action) of Natural :=
                               (True_When_Set  => 0,
                                False_When_Set => 0,
                                Store_Int      => 1,
                                Store_String   => 1,
                                Store_Operands => Natural'Last);

end Trendy_Command_Line.Options;
