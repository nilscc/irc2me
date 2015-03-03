declare module "src/libs/dcodeIO/ProtoBuf" {

    export class Builder {
        build(path? : string) : any;
    }

    export function loadProto(proto : string, builder? : Builder, filename? : string) : Builder;

}
