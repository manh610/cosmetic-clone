package com.cosmetic.gg.service.address.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.authentication.service.UserDetailsServiceImpl;
import com.cosmetic.gg.common.enums.EAddressType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.request.address.AddressSupplierRequest;
import com.cosmetic.gg.dto.response.address.AddressObjectResponse;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.address.Address;
import com.cosmetic.gg.entity.address.District;
import com.cosmetic.gg.entity.address.Province;
import com.cosmetic.gg.entity.address.SupplierAddress;
import com.cosmetic.gg.entity.address.UserAddress;
import com.cosmetic.gg.entity.address.Ward;
import com.cosmetic.gg.entity.supply.Supplier;
import com.cosmetic.gg.model.address.AddressModel;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.address.AddressRepository;
import com.cosmetic.gg.repository.address.DistrictRepository;
import com.cosmetic.gg.repository.address.ProvinceRepository;
import com.cosmetic.gg.repository.address.SupplierAddressRepository;
import com.cosmetic.gg.repository.address.UserAddressRepository;
import com.cosmetic.gg.repository.address.WardRepository;
import com.cosmetic.gg.repository.supply.SupplierRepository;
import com.cosmetic.gg.service.address.AddressService;

@Service
public class AddressServiceImpl implements AddressService{
	
	private static final Logger log = LoggerFactory.getLogger(AddressServiceImpl.class);
	
	@Autowired
	private AddressRepository addressRepository;
	
	@Autowired
	private ProvinceRepository provinceRepository;
	
	@Autowired
	private DistrictRepository districtRepository;
	
	@Autowired
	private WardRepository wardRepository;
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private SupplierRepository supplierRepository;
	
	@Autowired
    private UserAddressRepository userAddressRepository;
	
	@Autowired
	private SupplierAddressRepository supplierAddressRepository;
	
	@Override
	public List<Address> search(String code, EStatus status, String provinceId, String districtId, String wardId) {
		List<Address> result = new ArrayList<>();
		try {
			result = addressRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(code)) ? null : code,
					Objects.isNull(status) ? EStatus.ACTIVE.name() : status.name(),
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(provinceId)) ? null : provinceId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(districtId)) ? null : districtId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(wardId)) ? null : wardId);
			
			return result;
		}catch(Exception ex) {
			log.error("Error while searching address", ex.getCause());
		}
		return result;
	}
	
	public Address getById(String id) {
		try {
			Address addressEntity = addressRepository.findById(id).orElse(null);
			if(addressEntity == null || addressEntity.getStatus() == EStatus.DELETED)
				addressEntity = null;
			
			return addressEntity;
		}catch(Exception ex) {
			log.error(String.format("Error while getting address by id: %s", id), ex.getCause());
		    return null;
		}
	}
	
	@Override
	public List<Error> validator(Address address) {
		List<Error> errors = new ArrayList<>();
		try {
			Address addressCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(address.getId()))) {
				addressCheck = addressRepository.findById(address.getId()).orElse(null);
				if(addressCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));

				if(addressCheck.getStatus() == EStatus.DELETED)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(address.getWardId()))) {
				Ward wardEntity = wardRepository.findById(address.getWardId()).orElse(null);
				if(Objects.isNull(wardEntity) || !wardEntity.getDistrictId().equalsIgnoreCase(address.getDistrictId()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Xã/Phường không khớp Quận/Huyện", "Ward is not match District"));
			}
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(address.getDistrictId()))) {
				District districtEntity = districtRepository.findById(address.getDistrictId()).orElse(null);
				if(Objects.isNull(districtEntity) || !districtEntity.getProvinceId().equalsIgnoreCase(address.getProvinceId()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Quận/Huyện không khớp Thành phố/Tỉnh", "District is not match Province"));
			}
			
		}catch(Exception ex) {
			log.error("Error while validating address data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}

	@Override
	public Address addUser (AddressModel addressModel) {
		try {
			Address addressEntity = ModelMapper.map(addressModel, Address.class);
			User userEntity = userRepository.findById(addressModel.getUserId()).orElse(null);
			if(userEntity == null || userEntity.getStatus() != EStatus.ACTIVE) return null;
			
			List<Address> addressChecks = addressRepository.findByDefault(true, addressModel.getUserId());
			if(!addressChecks.isEmpty() && addressModel.isDefault() == true)
				return null;
			
			addressEntity.setStatus(EStatus.ACTIVE);
			Integer quantityAddress = userAddressRepository.cntAddressByUser(userEntity.getId());
			if(quantityAddress > 10) 
				return null;
			
			addressEntity = addressRepository.save(addressEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(addressEntity.getId())))
		        return null;
			
			UserAddress userAddress = new UserAddress();
			userAddress.setAddressId(addressEntity.getId());
			userAddress.setUserId(userEntity.getId());
			userAddress = userAddressRepository.save(userAddress);
			
			return addressEntity;
		}catch(Exception ex) {
			log.error("Error while creating address for user", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Address addSupplier (AddressSupplierRequest addressSupplier) {
		try {
			Address addressEntity = ModelMapper.map(addressSupplier, Address.class);
			Supplier supplierEntity = supplierRepository.findById(addressSupplier.getSupplierId()).orElse(null);
			if(supplierEntity == null || supplierEntity.getStatus() != EStatus.ACTIVE) return null;
			
			addressEntity.setStatus(EStatus.ACTIVE);
			addressEntity = addressRepository.save(addressEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(addressEntity.getId())))
		        return null;
			
			SupplierAddress supplierAddress = new SupplierAddress();
			supplierAddress.setAddressId(addressEntity.getId());
			supplierAddress.setSupplierId(supplierEntity.getId());
			supplierAddress.setBranch(addressSupplier.getBranch());
			supplierAddress = supplierAddressRepository.save(supplierAddress);
			
			return addressEntity;
		}catch(Exception ex) {
			log.error("Error while creating address for supplier", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Address updateUser(AddressModel addressModel) {
		try {
			Address addressEntity = ModelMapper.map(addressModel, Address.class);
			if(addressEntity == null)
				return null;
			
			List<Address> addressChecks = addressRepository.findByDefault(true, addressModel.getUserId());
			if(!addressChecks.isEmpty() && addressModel.isDefault() == true)
				return null;
			
			User userEntity = userRepository.findById(addressModel.getUserId()).orElse(null);
			if(userEntity == null || userEntity.getStatus() != EStatus.ACTIVE) return null;
			
			addressEntity = addressRepository.save(addressEntity);
			if(addressEntity == null)
				return null;
			return addressEntity;
		}catch(Exception ex) {
			log.error("Error while updating address for user", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Address updateSupplier(AddressSupplierRequest addressSupplier) {
		try {
			Address addressEntity = ModelMapper.map(addressSupplier, Address.class);
			if(addressEntity == null)
				return null;
			Supplier supplierEntity = supplierRepository.findById(addressSupplier.getSupplierId()).orElse(null);
			if(supplierEntity == null || supplierEntity.getStatus() != EStatus.ACTIVE) return null;
			
			addressEntity = addressRepository.save(addressEntity);
			if(addressEntity == null)
				return null;
			
			SupplierAddress supplierAddress = new SupplierAddress();
			supplierAddress.setAddressId(addressEntity.getId());
			supplierAddress.setSupplierId(supplierEntity.getId());
			supplierAddress.setBranch(addressSupplier.getBranch());
			supplierAddress = supplierAddressRepository.save(supplierAddress);
			return addressEntity;
		}catch(Exception ex) {
			log.error("Error while updating address for supplier", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Address addressEntity = getById(id);
			if(addressEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			if(addressEntity.isDefault())
				return new Error().builder(ErrorCode.ADDRESS_INVALID_DEFAULT);
			
			addressEntity.setStatus(EStatus.DELETED);
			addressEntity = addressRepository.save(addressEntity);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting address", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public AddressModel detail(String id) {
		try {
//			Object rs = addressRepository.detail(id);
//			if(rs instanceof Object[]) {
//				Object[] objArray = (Object[]) rs;
//				AddressModel addressModel = new AddressModel();
//				addressModel.setId((String) objArray[0]);
//				addressModel.setCode((String) objArray[1]);
//				addressModel.setProvinceId((String) objArray[2]);
//				addressModel.setDistrictId((String) objArray[3]);
//				addressModel.setWardId((String) objArray[4]);
//				addressModel.setUserId((String) objArray[5]);
//				addressModel.setDetail((String) objArray[6]);
//				
//				EStatus status = ((String) objArray[7]).equalsIgnoreCase("ACTIVE")? EStatus.ACTIVE : EStatus.DELETED;
//				addressModel.setStatus(status);
//				addressModel.setDefault((boolean) objArray[8]);
//				addressModel.setProvinceFullName((String) objArray[9]);
//				addressModel.setDistrictFullName((String) objArray[10]);
//				addressModel.setWardFullName((String) objArray[11]);
//				
//				return addressModel;
//			}
			return null;
		}catch(Exception ex) {
			log.error(String.format("Error while getting detail address by id: %s", id), ex.getCause());
		    return null;
		}
	}
	
	@Override
	public List<AddressObjectResponse> getByUser(String userId){
		List<AddressObjectResponse> results = new ArrayList<>();
		try {
			User user = userRepository.findById(userId).orElse(null);
			if(user == null || user.getStatus() != EStatus.ACTIVE)
				return Collections.emptyList();
			
			List<Object> rs = addressRepository.findByUser(userId);
			for(Object element: rs) {
				if(element instanceof Object[]) {
					Object[] objArray = (Object[]) element;
					AddressObjectResponse addressModel = new AddressObjectResponse();
					addressModel.setId((String) objArray[0]);
					addressModel.setWardFullName((String) objArray[1]);
					addressModel.setProvinceId((String) objArray[2]);
					addressModel.setDistrictId((String) objArray[3]);
					addressModel.setWardId((String) objArray[4]);
					addressModel.setDetail((String) objArray[5]);
					
					EStatus status = ((String) objArray[6]).equalsIgnoreCase("ACTIVE")? EStatus.ACTIVE : EStatus.DELETED;
					addressModel.setStatus(status);
					addressModel.setDefault((boolean) objArray[7]);
					addressModel.setProvinceFullName((String) objArray[8]);
					addressModel.setDistrictFullName((String) objArray[9]);
					addressModel.setObjectId(user.getId());
					addressModel.setFullName((String) objArray[10]);
					addressModel.setPhone((String) objArray[11]);
					EAddressType addressType = ((String) objArray[12]).equalsIgnoreCase("HOME")? EAddressType.HOME : EAddressType.WORK;
					addressModel.setAddressType(addressType);
					
					results.add(addressModel);
				}
			}
			return results;
		}catch(Exception ex) {
			log.error(String.format("Error while getting address by userId: %s", userId), ex.getCause());
			return results;
		}
	}
	
	@Override
	public List<AddressObjectResponse> getBySupplier(String supplierId){
		List<AddressObjectResponse> results = new ArrayList<>();
		try {
			Supplier supplier = supplierRepository.findById(supplierId).orElse(null);
			if(supplier == null || supplier.getStatus() == EStatus.INACTIVE)
				return Collections.emptyList();
			
			List<Object> rs = addressRepository.findBySupplier(supplierId);
			for(Object element: rs) {
				if(element instanceof Object[]) {
					Object[] objArray = (Object[]) element;
					AddressObjectResponse addressModel = new AddressObjectResponse();
					addressModel.setId((String) objArray[0]);
					addressModel.setBranch((String) objArray[1]);
					addressModel.setProvinceId((String) objArray[2]);
					addressModel.setDistrictId((String) objArray[3]);
					addressModel.setWardId((String) objArray[4]);
					addressModel.setDetail((String) objArray[5]);
					
					EStatus status = ((String) objArray[6]).equalsIgnoreCase("ACTIVE")? EStatus.ACTIVE : EStatus.DELETED;
					addressModel.setStatus(status);
					addressModel.setDefault((boolean) objArray[7]);
					addressModel.setProvinceFullName((String) objArray[8]);
					addressModel.setDistrictFullName((String) objArray[9]);
					addressModel.setWardFullName((String) objArray[10]);
					addressModel.setObjectId(supplier.getId());
					
					results.add(addressModel);
				}
			}
			return results;
		}catch(Exception ex) {
			log.error(String.format("Error while getting address by userId: %s", supplierId), ex.getCause());
			return results;
		}
	}
	
	@Override
	public Error changeDefault(String addressId, String userId) {
		try {
			Address addressEntity = getById(addressId);
			if(addressEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			User userEntity = userRepository.findById(userId).orElse(null);
			if(userEntity == null || userEntity.getId() == null || userEntity.getStatus() != EStatus.ACTIVE)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			List<Address> address = addressRepository.findByDefault(true, userId);
			for(Address item: address) {
				item.setDefault(false);
				addressRepository.save(item);
			}
			
			addressEntity.setDefault(true);
			addressRepository.save(addressEntity);
			
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while changing default address", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public List<Province> getProvinces(String name) {
		try {
			return provinceRepository.findProvinces(Boolean.TRUE.equals(StringUtils.isNullOrEmpty(name)) ? null : name);
		}catch(Exception ex) {
			log.error("Error while getting provinces", ex.getCause());
			return Collections.emptyList();
		}
	}
	
	@Override
	public List<District> getDistricts(String provinceId) {
		try {
			if(StringUtils.isNullOrEmpty(provinceId))
				return Collections.emptyList();
			
			return districtRepository.findDistricts(provinceId);
		}catch(Exception ex) {
			log.error("Error while getting districts by province code", ex.getCause());
			return Collections.emptyList();
		}
	}
	
	@Override
	public List<Ward> getWards(String districtId) {
		try {
			if(StringUtils.isNullOrEmpty(districtId))
				return Collections.emptyList();
			
			return wardRepository.findWards(districtId);
		}catch(Exception ex) {
			log.error("Error while getting wards by district code", ex.getCause());
			return Collections.emptyList();
		}
	}
}
