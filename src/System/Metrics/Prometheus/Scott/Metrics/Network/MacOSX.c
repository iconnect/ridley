
#include <sys/types.h>

#include <sys/socket.h>

#include <ifaddrs.h>

#include <regex.h>

#include <stddef.h>

#include "helpers.h"

#include <stdlib.h>

#include <string.h>

#include <net/if.h>


void set_scott_ifi_data(struct if_data* netData, scott_if_data_t* devStats) {

  //devStats["receive_packets"] = convertFreeBSDCPUTime(uint64(netData.ifi_ipackets));
  devStats->scott_ifi_ipackets = (long)(netData->ifi_ipackets);

  //devStats["transmit_packets"] = convertFreeBSDCPUTime(uint64(netData.ifi_opackets));
  devStats->scott_ifi_opackets = (long)(netData->ifi_opackets);

  //devStats["receive_errs"] = convertFreeBSDCPUTime(uint64(netData.ifi_ierrors));
  devStats->scott_ifi_ierrors = (long)(netData->ifi_ierrors);

  //devStats["transmit_errs"] = convertFreeBSDCPUTime(uint64(netData.ifi_oerrors));
  devStats->scott_ifi_oerrors = (long)(netData->ifi_oerrors);

  //devStats["receive_bytes"] = convertFreeBSDCPUTime(uint64(netData.ifi_ibytes));
  devStats->scott_ifi_ibytes  = (long)(netData->ifi_ibytes);

  //devStats["transmit_bytes"] = convertFreeBSDCPUTime(uint64(netData.ifi_obytes));
  devStats->scott_ifi_obytes  = (long)(netData->ifi_obytes);

  //devStats["receive_multicast"] = convertFreeBSDCPUTime(uint64(netData.ifi_imcasts));
  devStats->scott_ifi_imcasts  = (long)(netData->ifi_imcasts);
  //devStats["transmit_multicast"] = convertFreeBSDCPUTime(uint64(netData.ifi_omcasts));
  devStats->scott_ifi_omcasts  = (long)(netData->ifi_omcasts);
  //devStats["receive_drop"] = convertFreeBSDCPUTime(uint64(netData.ifi_iqdrops));
  devStats->scott_ifi_iqdrops = (long)(netData->ifi_iqdrops);

  //devStats["transmit_drop"] = convertFreeBSDCPUTime(uint64(netData.ifi_oqdrops));
  // Not present in this version of if_data
}


scott_if_data_t * inline_c_System_Metrics_Prometheus_Scott_Metrics_Network_MacOSX_0_a1808202dc69045a59f9e139dfa413540d8933a0(int * totalInterfaces_inline_c_0) {

        struct ifaddrs *ifap, *ifa;
        struct if_data *netData;
        scott_if_data_t *netDev = malloc(30 * sizeof(scott_if_data_t));
        int interfaceIdx = 0;

        // Compile a regex to ignore certain devices
        regex_t regex;
        int ignoreDevice;

        ignoreDevice = regcomp(&regex, "^(ram|loop|fd|(h|s|v|xv)d[a-z]|nvme\\d+n\\d+p)\\d+$", 0);

        if (ignoreDevice) {
          netDev[0].scott_ifi_error = 1;
          return netDev;
        }

        if (getifaddrs(&ifap) == -1) {
          netDev[0].scott_ifi_error = 1;
          return netDev;
        }

        // Iterate over all the network interfaces found
        for (ifa = ifap; ifa; ifa = ifa->ifa_next) {
          if (ifa->ifa_addr->sa_family == AF_LINK) {
             char* currentDevice = malloc((strlen(ifa->ifa_name) + 1) * sizeof(char));
             strcpy(currentDevice, ifa->ifa_name);
             ignoreDevice = regexec(&regex, currentDevice, 0, NULL, 0);
             if (!ignoreDevice) {
                 regfree(&regex);
                 free(currentDevice);
                 continue;
             } else {
                 regfree(&regex);
             }

             scott_if_data_t *devStats = &netDev[interfaceIdx];
             netData = ifa->ifa_data;
             devStats->scott_ifi_name = currentDevice;
             set_scott_ifi_data(netData, devStats);
             devStats->scott_ifi_error = 0;
             interfaceIdx++;

          }
        }
        freeifaddrs(ifap);
        *totalInterfaces_inline_c_0 = interfaceIdx;
        return netDev;
       
}

