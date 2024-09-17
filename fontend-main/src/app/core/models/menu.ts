export interface MenuItem {
  id: number;
  name: string;
  route?: string;
  icon: string;
  path?: string;
  disabled?: boolean;
  children?: MenuItem[];
}
