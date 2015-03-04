declare module "ProtoBuf" {

    export class Builder {
        build<T>(path? : string) : T;
    }

    export function loadProto(proto : string, builder? : Builder, filename? : string) : Builder;

}
