#include "helpers.h"

#ifdef __APPLE__

// Free our ridley_if_data data structure
void free_ridley_if_data(ridley_if_data_t* interfaces, int interfacesNum) {
  for (int i = 0; i < interfacesNum; i++) {
    free(interfaces[i].ridley_ifi_name);
  }

  // Finally, deallocate the outermost array
  free(interfaces);
}

#endif
