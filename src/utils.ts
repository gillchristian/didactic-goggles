import * as t from 'io-ts'

export function optional<RT extends t.Any>(
  type: RT,
  name: string = `${type.name} | undefined`,
): t.UnionType<
  [RT, t.UndefinedType],
  t.TypeOf<RT> | undefined,
  t.OutputOf<RT> | undefined,
  t.InputOf<RT> | undefined
> {
  return t.union<[RT, t.UndefinedType]>([type, t.undefined], name)
}
