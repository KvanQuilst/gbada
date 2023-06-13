with Interfaces.C; use Interfaces.C;

package Screen.Curses is

  function Init_Screen return int
    with Import => True, Convention => C;

  procedure Kill_Screen
    with Import => True, Convention => C;

end Screen.Curses;
