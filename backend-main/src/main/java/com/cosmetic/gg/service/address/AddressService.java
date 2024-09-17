package com.cosmetic.gg.service.address;

import java.util.List;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.request.address.AddressSupplierRequest;
import com.cosmetic.gg.dto.response.address.AddressObjectResponse;
import com.cosmetic.gg.entity.address.Address;
import com.cosmetic.gg.entity.address.District;
import com.cosmetic.gg.entity.address.Province;
import com.cosmetic.gg.entity.address.Ward;
import com.cosmetic.gg.model.address.AddressModel;

public interface AddressService{
	
	List<Address> search(String code, EStatus status, String provinceId, String districtId, String wardId);
	
	List<Error> validator(Address address);
	
	Address addUser(AddressModel addressModel);
	
	Address addSupplier (AddressSupplierRequest addressSupplier);
	
	Address updateUser (AddressModel addressModel);
	
	Address updateSupplier(AddressSupplierRequest addressSupplier);
	
	Error delete(String id);
	
	AddressModel detail(String id);
	
	List<AddressObjectResponse> getByUser(String userId);
	
	List<AddressObjectResponse> getBySupplier(String supplierId);
	
	Error changeDefault(String addressId, String userId);
	
	List<Province> getProvinces(String name);
	
	List<District> getDistricts(String provinceId);
	
	List<Ward> getWards(String districtId);
}
