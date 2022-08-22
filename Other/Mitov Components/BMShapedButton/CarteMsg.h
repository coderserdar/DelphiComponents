// Copyright Eric DUPONT roderic@roderic.com

#ifndef _CARTEMSG_H_
#define _CARTEMSG_H_

#include <system.hpp>

#ifndef DECLARATION_CARTE_MESSAGES
#define DECLARATION_CARTE_MESSAGES virtual void __fastcall Dispatch(void *Message);
#endif /* DECLARATION_CARTE_MESSAGES */

#ifndef COMMENCE_DEFINITION_CARTE_MESSAGES
#define COMMENCE_DEFINITION_CARTE_MESSAGES(Classe) void __fastcall Classe::Dispatch(void *Message) \
        {                                           \
          switch  (((PMessage)Message)->Msg)        \
          {
#endif /* COMMENCE_DEFINITION_CARTE_MESSAGES */

#ifndef FINI_DEFINITION_CARTE_MESSAGES
#define FINI_DEFINITION_CARTE_MESSAGES END_MESSAGE_MAP
#endif /* FINI_DEFINITION_CARTE_MESSAGES */

#endif /* _CARTEMSG_H_ */
