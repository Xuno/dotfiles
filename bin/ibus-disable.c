
#include <ibus.h>

int main()
{
    g_type_init();
    IBusBus *bus = ibus_bus_new();
    IBusInputContext *ic = ibus_input_context_get_input_context(
            ibus_bus_current_input_context(bus), ibus_bus_get_connection(bus));
    ibus_input_context_set_capabilities(ic, 0);
    ibus_input_context_disable(ic);
    g_assert(ibus_input_context_is_enabled(ic) == FALSE);
    return 0;
}
