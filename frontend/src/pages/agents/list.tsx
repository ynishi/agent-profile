import {
  DeleteButton,
  EditButton,
  List,
  ShowButton,
  useTable,
} from "@refinedev/antd";
import { BaseRecord, useMany } from "@refinedev/core";
import { Space, Table } from "antd";

export const AgentList = () => {
  const { tableProps } = useTable({
    syncWithLocation: true,
    meta: {
      select: "*",
    },
  });

  const { data: profileData, isLoading: profileIsLoading } = useMany({
    resource: "profiles",
    ids:
      tableProps?.dataSource
        ?.map((item) => item?.profiles?.id)
        .filter(Boolean) ?? [],
    queryOptions: {
      enabled: !!tableProps?.dataSource,
    },
  });

  return (
    <List>
      <Table {...tableProps} rowKey="id">
        <Table.Column dataIndex={"agent_id"} title={"ID"} />
        <Table.Column dataIndex="name" title={"Name"} />
        <Table.Column dataIndex="description" title={"Description"} />
        <Table.Column dataIndex="version" title={"Version"} />
        <Table.Column
          title={"Actions"}
          dataIndex="actions"
          render={(_, record: BaseRecord) => (
            <Space>
              <EditButton hideText recordItemId={record.agent_id} />
              <ShowButton hideText recordItemId={record.agent_id} />
              <DeleteButton hideText recordItemId={record.agent_id} />
            </Space>
          )}
        />
      </Table>
    </List>
  );
};
