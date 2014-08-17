#include "ircchannel.h"

IrcChannel::IrcChannel()
{
}

/*
 *
 */

void IrcChannel::addIrcMessage(Protobuf::Messages::IrcMessage msg)
{
    msgs.push_front(msg);
}
