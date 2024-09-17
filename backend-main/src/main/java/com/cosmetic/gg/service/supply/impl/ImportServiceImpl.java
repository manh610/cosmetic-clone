package com.cosmetic.gg.service.supply.impl;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.product.ProductDetailResponse;
import com.cosmetic.gg.dto.response.supply.ImportDetailResponse;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.entity.supply.Import;
import com.cosmetic.gg.entity.supply.ProductImport;
import com.cosmetic.gg.entity.supply.Supplier;
import com.cosmetic.gg.model.supply.ImportModel;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.repository.supply.ImportRepository;
import com.cosmetic.gg.repository.supply.ProductImportRepository;
import com.cosmetic.gg.repository.supply.SupplierRepository;
import com.cosmetic.gg.service.product.ProductService;
import com.cosmetic.gg.service.supply.ImportService;

@Service
public class ImportServiceImpl implements ImportService{

	private static final Logger log = LoggerFactory.getLogger(ImportServiceImpl.class);
	
	@Autowired
	private ImportRepository importRepository;
	
	@Autowired
	private SupplierRepository supplierRepository;
	
	@Autowired
	private ProductImportRepository productImportRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private ProductService productService;
	
	@Override
	public Map<String, Object> search(String keyword, String supplierId, LocalDateTime importDateFrom, LocalDateTime importDateTo,
							Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			if(Objects.isNull(importDateFrom) && !Objects.isNull(importDateTo))
				importDateFrom = importDateTo;
			if(!Objects.isNull(importDateFrom) && Objects.isNull(importDateTo))
				importDateTo = importDateFrom;
			
			List<Import> imports = importRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(supplierId)) ? null : supplierId,
					Objects.isNull(importDateFrom) ? null : importDateFrom,
					Objects.isNull(importDateTo) ? null : importDateTo,
					pageIndex, pageSize);
			
			Integer totalItem = importRepository.cntImport(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(supplierId)) ? null : supplierId,
					Objects.isNull(importDateFrom) ? null : importDateFrom,
					Objects.isNull(importDateTo) ? null : importDateTo);
			
			result.put("data", imports);
			result.put("totalItem", totalItem);
		}catch (Exception ex) {
	      result.put("data", new ArrayList<>());
	      result.put("totalItem", 0);
	      log.error("Error while searching import product", ex.getCause());
	    }
	    return result;
	}
	
	@Override
	public List<Error> validator(ImportModel importModel) {
		List<Error> errors = new ArrayList<>();
		try {
			Import importCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(importModel.getId()))) {
				importCheck = importRepository.findById(importModel.getId()).orElse(null);
				if(importCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert importCheck != null;
				if(!importCheck.getCode().equals(importModel.getCode())) {
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
				}
			}
			importCheck = importRepository.findByKey(importModel.getCode());
			if(importCheck != null && !importCheck.getId().equals(importModel.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
			
			Supplier supplier = supplierRepository.findById(importModel.getSupplierId()).orElse(null);
			if(supplier == null || supplier.getId() == null || supplier.getStatus() == EStatus.DELETED)
				errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			
			for(String item: importModel.getProductId()) {
				Product product = productRepository.findById(item).orElse(null);
				if(product == null) 
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
		}catch(Exception ex) {
			log.error("Error while validating import product data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Import create(ImportModel importModel) {
		try {
			Import importEntity = ModelMapper.map(importModel,Import.class);
			importEntity.prepareEntity();
			importEntity = importRepository.save(importEntity);
			if (importEntity == null)
				return null;
			
			for(String item: importModel.getProductId()) {
				ProductImport productImport = new ProductImport();
				productImport.setImportId(importEntity.getId());
				productImport.setProductId(item);
				productImport = productImportRepository.save(productImport);
				if(productImport == null || productImport.getId() == null)
					return null;
			}
			return importEntity;
		}catch(Exception ex) {
			log.error("Error while creating import product", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Import update(ImportModel importModel) {
		try {
			Import importEntity = ModelMapper.map(importModel,Import.class);
			importEntity.prepareEntity();
			importEntity = importRepository.save(importEntity);
			if (importEntity == null)
				return null;
			return importEntity;
		}catch(Exception ex) {
			log.error("Error while creating import product", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Import importCheck = importRepository.findById(id).orElse(null);
			if(importCheck == null || importCheck.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			importRepository.deleteById(id);
			List<ProductImport> productImport = productImportRepository.findByImport(id);
			for(ProductImport item: productImport) {
				productImportRepository.deleteById(item.getId());
			}
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting import", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public ImportDetailResponse detail(String id) {
		try {
			ImportDetailResponse response = new ImportDetailResponse();
			Import importCheck = importRepository.findById(id).orElse(null);
			if(importCheck == null || importCheck.getId() == null)
				return null;
			Import importEntity = importRepository.findById(id).orElse(null);
			response.setId(importEntity.getId());
			response.setCode(importEntity.getCode());
			response.setImportDate(importEntity.getImportDate());
			response.setVat(importEntity.getVat());
			response.setRepresentativeName(importEntity.getRepresentativeName());
			response.setRepresentativeEmail(importEntity.getRepresentativeEmail());
			response.setRepresentativePhone(importEntity.getRepresentativePhone());
			response.setSupplierId(importEntity.getSupplierId());
			response.setSupplierAddressId(importEntity.getSupplierAddressId());
			response.setStatus(importEntity.getStatus());
			response.setDescription(importEntity.getDescription());
			response.setCreatedAt(importEntity.getCreatedAt());
			response.setCreatedBy(importEntity.getCreatedBy());
			response.setUpdatedAt(importEntity.getUpdatedAt());
			response.setUpdatedBy(importEntity.getUpdatedBy());
			
			List<ProductDetailResponse> productId = new ArrayList<>();
			List<ProductImport> productImport = productImportRepository.findByImport(id);
			for(ProductImport item: productImport) {
				ProductDetailResponse rs = productService.detail(item.getProductId());
				productId.add(rs);
			}
			response.setProductId(productId);
			
			return response;
		}catch(Exception ex) {
			log.error("Error while getting detail import product", ex.getCause());
			return null;
		}
	}
}
