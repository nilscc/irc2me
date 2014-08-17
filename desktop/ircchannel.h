#pragma once

#include "messages.pb.h"
#include <deque>

class IrcChannel
{
public:
    IrcChannel();

    void addIrcMessage(Protobuf::Messages::IrcMessage msg);

private:
    std::deque<Protobuf::Messages::IrcMessage> msgs;

};
