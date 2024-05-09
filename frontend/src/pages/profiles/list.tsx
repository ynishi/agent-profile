import {
  DeleteButton,
  EditButton,
  List,
  ShowButton,
  useTable,
} from "@refinedev/antd";
import { BaseRecord } from "@refinedev/core";
import { Space, Table } from "antd";

export const ProfileList = () => {
  const { tableProps } = useTable({
    syncWithLocation: true,
  });

  return (
    <List>
      <Table {...tableProps} rowKey="profile_id">
        <Table.Column dataIndex="profile_id" title={"ID"} />
        <Table.Column dataIndex="name" title={"name"} />
        <Table.Column dataIndex="description" title={"description"} />
        <Table.Column
          title={"Actions"}
          dataIndex="actions"
          render={(_, record: BaseRecord) => (
            <Space>
              <EditButton hideText recordItemId={record.profile_id} />
              <ShowButton hideText recordItemId={record.profile_id} />
              <DeleteButton hideText recordItemId={record.profile_id} />
            </Space>
          )}
        />
      </Table>
    </List>
  );
};
